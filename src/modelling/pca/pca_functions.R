#Dependencies
library(glmnet)
library(caret)
library(tidyverse)
library(recipes)
library(moments)
library(ParBayesianOptimization)
library(parallel)
library(fastknn)

#..................................Bagging Function...................................#
bagged_model <- function(train, test, label_train, alpha, s_lambda, times, calibrate = FALSE) {
  
  #' Generates a bagged (bootstrapped aggregated) elastic net model given a set of hyper parameters alpha and lambda.
  #'
  #' @param train a tibble containing the training data. The frame should contain only numeric values and be standardized and scaled. 
  #' @param test a tibble containing the validation or test data that will be used to score the model's log loss. The frame should contain only numeric values and be standardized and scaled. 
  #' @param label_train a factor ("result_factor") corresponding to the train tibble.  
  #' @param alpha a numeric value in [0,1] to train each elastic net model within the bag.
  #' @param s_lambda an integer value in {1, 2, 3, .... infinity} for subsetting the prediction matrix generated by glmnet. 
  #' @param times an integer that specifies the number of bootstrap models to be created and then aggregated
  #' @param calibrate a logical: should the probabilities output by the trained model be calibrated? If TRUE, probabilities are calibrated using platt scaling. If FALSE, probabilities are left unprocessed. Default = TRUE.
  #'
  #' @return 
  #' A list containing a tibble named "predictions" which holds the predictions from the model on the test set, and another
  #' tibble containing variable importance scales generating during fitting.
  #'
  #' @export
  #'
  
  set.seed(3742301)
  samples <- caret::createResample(y = label_train, times = times)
  pred <- vector("list", length(samples))
  var_imp <- vector("list", length(samples))
  insample_pred <- vector("list", length(samples))
  
  for (g in 1:length(samples)) {
    
    train_temp <- train[samples[[g]], ]
    train_label <- label_train[samples[[g]]]
    model_x <- glmnet(x = data.matrix(train_temp[, !names(train_temp) %in% c("result_factor")]), y = train_label, family = "binomial", alpha = alpha, nlambda = 120, standardize = FALSE)
    s_lambda <- case_when(s_lambda > length(model_x$lambda) ~ length(model_x$lambda), TRUE ~ s_lambda)
    
    pred[[g]] <- predict(model_x, newx = data.matrix(test[, !names(test) %in% c("result_factor")]), type = "response")[, s_lambda]
    insample_pred[[g]] <- predict(model_x, newx = data.matrix(train_temp[, !names(train_temp) %in% c("result_factor")]), type = "response")[, s_lambda]
    
    var_imp[[g]] <- tibble::rownames_to_column(caret::varImp(model_x, lambda = model_x$lambda[s_lambda]), var = "variable")
    colnames(var_imp[[g]])[2] <- paste("Overall:", g, sep = "")
    remove(model_x, train_temp, train_label)
    
  }
  
  pred <- bind_cols(pred) %>%
    transmute(predicted = rowMeans(.))
  
  insample_pred <- bind_cols(insample_pred) %>%
    transmute(predicted = rowMeans(.))
  
  var_imp <- var_imp %>% reduce(left_join, by = "variable") 
  
  means <- var_imp %>% 
    select_if(is.numeric) %>%
    transmute(variable_importance = rowMeans(.))
  
  var_imp <- tibble(variable = var_imp$variable, mean_importance = means$variable_importance)
  
  if (calibrate == TRUE) {
    list(predictions = platt_scale(label_train = label_train, predicted_prob_train = insample_pred, 
                                   predicted_prob_test = pred), variable_importance = var_imp)
  } else {
    list(predictions = pred$predicted, variable_importance = var_imp)  
  }
}

#..................................Platt Scaling/sigmoid........................................#
platt_scale <- function(label_train, predicted_prob_train, predicted_prob_test) {
  
  #' Calibrates a vector of probabilities using platt scaling.
  #' 
  #' @param label_train a vector of labels (factors) for the training dataset
  #' @param predicted_prob_train  a numeric vector of probabilities generated on the training dataset 
  #' @param predicted_prob_test a numeric vector of probabilities generated on the testing dataset
  #'
  #' @return 
  #' A numeric vector containing calibrated probabilities on the test set.
  #'
  #' @export
  #'
  
  model <- glm(formula = label_train ~ ., data = predicted_prob_train, family = binomial(link = "logit"))
  
  predict(model, newdata = predicted_prob_test, type = c("response"))
  
}

#..................................Log Loss Function....................................#

log_loss <- function(scores, label) {
  
  #' Computes the log loss given a set of probabilities and ground truth labels.
  #'
  #' @param scores a numeric vector of probabilities where the positive class is 1 and the negative class is 0. 
  #' @param label a factor of W (win) or L (losses) representing the ground truth.
  #' 
  #' @return
  #' A vector of length one containing the mean log loss given the ground truth labels.
  #'
  #' @export
  #' 
  
  if (is.factor(label)) {
    u <- ifelse(label ==  "W", 1,0)
  } else {
    u <- label
  }
  
  tmp <- data.frame(scores = scores, target = u)
  
  #In case there are probabilities of 1 or 0 predicted by the model (very unlikely unless severe overfitting)
  tmp <- tmp %>%
    mutate(scores = ifelse(scores == 1, 0.9999999999999999999, ifelse(scores == 0 , 0.0000000000000000001, scores))) %>%
    mutate(log_loss = -(target * log(scores) + (1 - target) * log(1 - scores)))
  
  mean(tmp$log_loss)
}
#..................................kNN Function....................................#
add_knn_variables <- function(train_data, test_data, include_PCA = FALSE, distances = TRUE, use_only_variables =  NULL) {
  
  #' Binds kNN variables to the dataset (does not delete the original variables used to create them)
  #' Returns cumulative euclidian distances (sum of euclidian distances) to the k most closest neighbours for each class label to an observation, by default.
  #' 
  #' @param train_data a tibble containing the training set. Should not contain the target variable (ResultProper) and if ResultProper is included, this variable will be removed before running the PCA. The tibble should only contain numeric values and be standardized and scaled. 
  #' @param test_data a tibble containing the validation or test set. The tibble should only contain numeric values and be standardized and scaled.
  #' @param include_PCA a logical. If TRUE, includes any PCA components (see function addPCA_variables) in the creation of the kNN variables. Default = FALSE. The PCA variables should start with the prefix "PC".
  #' @param distances a logial. If TRUE, will return cumulative euclidian distances to the k most closest neighbours for each class label to an observation. If FALSE, calculates the proportion of winners (and therefore, losers as well) out of the k most closest neighbours to an observation.
  #' @param use_only_variables a character vector that specifies which variables to use in traindata to compute kNN distances for. By default, uses all variables in traindata.
  #'
  #' @return
  #' A list containing two tibbles corresponding to the original training and testing datasets with kNN variables included.
  #'
  #' @export
  #'
  
  if (!is.null(use_only_variables)) {
    train_data_tmp <- train_data %>% select(., use_only_variables)
    test_data_tmp <- test_data %>% select(., use_only_variables)
  }
  
  if (include_PCA == TRUE) {
    train_data_tmp <- train_data %>% select_if(., is.numeric)
    test_data_tmp <- test_data %>% select_if(., is.numeric)
  } else {
    train_data_tmp <- train_data %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("pc"))
    test_data_tmp <- test_data %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("pc"))
  }
  
  if (distances == TRUE) {
    new_frames_with_knn <- fastknn::knnExtract(
      xtr = data.matrix(train_data_tmp), 
      ytr = train_data$result_factor,
      xte = data.matrix(test_data_tmp),
      k = 1,
      normalize = NULL)
    
    knn_train <- new_frames_with_knn$new.tr %>% as_tibble(.) 
    
    knn_train_params <- caret::preProcess(knn_train, method = c("center", "scale"))
    knn_train <- predict(knn_train_params, newdata = knn_train)
    
    knn_test <- new_frames_with_knn$new.te %>% as_tibble(.) %>% predict(knn_train_params, newdata = .)
    
    list(
      train = bind_cols(train_data, knn_train) %>% set_names(~str_to_lower(.)),
      test = bind_cols(test_data, knn_test) %>% set_names(~str_to_lower(.)))
    
  } else {
    
    knn_train <- tibble(knn_W = fastknn(
      xtr = data.matrix(train_data_tmp),
      ytr = train_data$result_factor,
      xte = data.matrix(train_data_tmp),
      k = 5, 
      method = "vote", 
      normalize = NULL)$prob[,2]) 
    
    knn_train_params <- caret::preProcess(knn_train, method = c("center", "scale"))
    knn_train <- predict(knn_train_params, newdata = knn_train)
    
    knn_test <- tibble(knn_w = fastknn(
      xtr = data.matrix(train_data_tmp), 
      ytr = train_data$result_factor, 
      xte = data.matrix(test_data_tmp), 
      k = 5, 
      method = "vote", 
      normalize = NULL)$prob[,2]) %>%
      predict(knn_train_params, newdata = .)
    
    list(train = bind_cols(train_data, knn_train) %>% set_names(~str_to_lower(.)),
         test = bind_cols(test_data, knn_test) %>% set_names(~str_to_lower(.)))
  }
}
#.......................Define recipe.............................................#

pre_process_recipe <- function(trainX) {
  
  #' Creates a recipe that defines all of the preprocessing steps necessary for the dataset.
  #'
  #' @param trainX a tibble that represents the training dataset. Must include the target column as a factor variable labelled as "ResultProper".
  #'
  #' @return
  #' A recipe object (a list) that defines the preprocessing pipeline.
  #'
  #' @export
  #'
  
  main_recipe <- recipe(result_factor ~., data = trainX) %>%
    step_dummy(all_predictors(), -all_numeric()) %>%
    step_interact(terms = ~ srs:fenwick:elo_rating) %>%
    step_interact(terms = ~ h2h:vegas_odds) %>%
    step_interact(terms = ~ faceoff_win_percentage:shot_percentage) %>%
    step_interact(terms = ~ contains("round"):vegas_odds) %>%
    step_interact(terms = ~ sd_record:sos) %>%
    step_zv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) 
  
  main_recipe
}

#....................Transform all variables via. PCA.......................#

pca_recipe <- function(ncomp, train, test) {
  
  #' Does a PCA transformation on to the original dataset.
  #'
  #' @param ncomp an integer specifying the number of components to retain
  #' @param train a tibble of the training dataset. Should contain the target variable (ResultProper) as a factor variable, but all predictors/features should be numeric.
  #' @param test a tible of the testing dataset. Should also contain the target variable (ResultProper) as a factor variable, and only contain numeric predictors.
  #' 
  #' @return:
  #' list
  #' A list containing the transformed training and testing datasets using PCA.
  #'
  #' @export
  #'
  
  pca <- recipe(result_factor ~., data = train) %>%
    step_pca(all_predictors(), num_comp = ncomp) 
  
  train_param <- prep(pca, training = train)
  
  list(train = bake(train_param, new_data = train), test = bake(train_param, new_data = test))

}

#.........................Define outer pipe for the outer cross validation fold...........................................#

model_pipe_outer <- function(lambda_final, alpha_final, ncomp_final, processed_data, times) {
  
  #' Fits a model on a training set, then validates the model using a specified set of hyperparameters (ideally found using the inner pipeline) on a held out test set. Works with function processFolds and is not intended to be used outside of this context.
  #'
  #' @param lambda_final an integer value fed to each boostrapped elastic net model in the bag
  #' @param alpha_final a numeric value in [0,1] used in fitting each bootstrapped elastic net model fit in the bag
  #' @param ncomp_final an integer value denoting the number of principle components to retain
  #' @param processed_data a named list that contains two tibbles: one tibble called "Train" and another tibble called "Test". Both of these tibbles should be the result of calling the function processFolds.
  #' @param times an integer that provides how many bootstrapped models to fit in the bag
  #' 
  #' @return
  #' A list containing the fitted models predictions, the fitted models log loss on the test set, and variable importance scores.
  #'
  #' @export 
  #'
  
  pca_transformed <- pca_recipe(ncomp = ncomp_final, train = processed_data$train, test = processed_data$test)
  train <- pca_transformed$train
  test <- pca_transformed$test
  
  rm(pca_transformed)
  
  model <- bagged_model(train = train[, !names(train) %in% c("result_factor")], test = test, label_train = train$result_factor, 
                        alpha = alpha_final, s_lambda = lambda_final, times = times, calibrate = FALSE)
  
  log_loss_tmp <- log_loss(scores = model$predictions, label = test$result_factor)
  
  var_imp <- model$variable_importance
  
  list(predictions = model$predictions, log_loss = log_loss_tmp, var_imp = var_imp)
}

#.............................Process Folds...................................#

process_folds <- function(fold_index, main_train, use_only_variables = NULL) {
  
  #' Given a set of indices representing the training dataset (relative to the dataset mainTrain), process the training and implied testing datasets. 
  #'
  #' @param fold_index a vector of indices corresponding to the training dataset (relative to mainTrain). 
  #' @param main_train a tibble from which the training and testing sets will be derived from.
  #' @param use_only_variables a character vector of column names in mainTrain from which the kNN variables should be computed. Default = NULL.
  #'
  #' @return
  #' A named list containing the preprocessed training (Train) and test (Test) sets.
  #'
  #' @export
  #'
  
  train_param <- prep(pre_process_recipe(trainX = main_train[fold_index, ]), training = main_train[fold_index, ])
  train <- bake(train_param, new_data = main_train[fold_index, ])
  test <- bake(train_param, new_data = main_train[-fold_index, ])
  
  frames_with_knn <- add_knn_variables(train_data = train, test_data = test, distances = TRUE, use_only_variables = use_only_variables)
  
  train <- frames_with_knn$train
  test <- frames_with_knn$test
  
  rm(frames_with_knn)
  
  list(train = train, test = test)
  
}

#..........................Processed variable importance output from the base model................................#
process_var_imp <- function(var_imp_raw) {
  
  #' Processes a list of variable importance scores generated by a bag of elastic net models.
  #'  Uses simple averaging over all of the fitted models in the bag.
  #'
  #' @param var_imp_raw a list of tibbles that contain variable importance scores produced from the function bagged_model.
  #'
  #' @return 
  #' A tibble that contains the variable importance scores.
  #'
  #' @export
  #'
  
  var_imp_names <- var_imp_raw %>% 
    select(., contains("variable")) %>%
    .[,1]
  
  final <- var_imp_raw %>% 
    select(., contains("importance")) %>%
    transmute(importance = rowMeans(.)) %>%
    bind_cols(var_imp_names, .)
  
  final
}

#..........................Ensemble-simple average with different seeds................................#
train_ensemble <- function(folds, final_parameters, processed_data, label_test, times) {
  
  #' Trains an ensemble of bagged elastic nets. The ensemble is just a 
  #' simple average over all fitted models trained on different seeds.
  #' 
  #' @param folds a list of indices corresponding to a train/test split of a dataset, generated from caret::createDataPartition
  #' @param final_parameters a tibble with columns "lambda" and "alpha" used for fitting each bagged elastic net in the ensemble
  #' @param processed_data a named list that contains two tibbles: one tibble called "Train" and another tibble called "Test". Both of these tibbles should be the result of calling the function processFolds.
  #' @param label_test a factor that provides the ground truth labels (Win = W or Loss = L) for the tibble labelled "Test" in processedData.
  #' @param times number of bootstrapped models to fit in the bag
  #'
  #' @return
  #' A list containing the log loss on the test set and the variable importance scores from the fitted model.
  #'
  #' @export
  #'
  
  final_predictions <- vector("list", nrow(final_parameters))
  final_var_imp <- vector("list", nrow(final_parameters))
  
  for (k in 1:nrow(final_parameters)) {
    
    final_model <- model_pipe_outer(
      lambda_final = as.integer(final_parameters$lambda[k]),
      alpha_final = final_parameters$alpha[k], 
      ncomp_final = final_parameters$ncomp[k],
      times = times, 
      processed_data = processed_data)
    
    final_predictions[[k]] <- final_model$predictions
    final_var_imp[[k]] <- final_model$var_imp
    
    rm(final_model) 
    
  }
  
  if (nrow(final_parameters) > 1) {
    
    final_pred_processed <- final_predictions %>%
      reduce(cbind) %>%
      rowMeans(.)
    
  } else {
    
    final_pred_processed <- final_predictions[[1]]  
    
  }
  
  final_var_imp_processed <- final_var_imp %>% 
    reduce(left_join, by = "variable") %>%
    process_var_imp(.)
  
  list(
    log_loss = log_loss(scores = final_pred_processed, label = label_test), 
    var_imp = final_var_imp_processed)
  
}