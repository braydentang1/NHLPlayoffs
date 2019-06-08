#Dependencies
library(glmnet)
library(caret)
library(tidyverse)
library(recipes)
library(moments)
library(rBayesianOptimization)
library(fastknn)

#..................................Bagging Function...................................#
baggedModel = function(train, test, label_train, alpha.a, s_lambda.a, calibrate = FALSE){
  
  set.seed(3742301)
  samples = caret::createResample(y = label_train, times = 20)
  pred = vector("list", length(samples))
  varImp = vector("list", length(samples))
  insample.pred = vector("list", length(samples))
  
  for (g in 1:length(samples)){
    
    train_temp = train[samples[[g]], ]
    train.label = label_train[samples[[g]]]
    modelX = glmnet(x = data.matrix(train_temp[, !names(train_temp) %in% c("ResultProper")]), y = train.label, family = "binomial", alpha = alpha.a, nlambda = 120, standardize = FALSE)
    s_lambda.a = case_when(s_lambda.a > length(modelX$lambda) ~ length(modelX$lambda), TRUE ~ s_lambda.a)
    
    pred[[g]] = predict(modelX, newx = data.matrix(test[, !names(test) %in% c("ResultProper")]), type = "response")[, s_lambda.a]
    insample.pred[[g]] = predict(modelX, newx = data.matrix(train_temp[, !names(train_temp) %in% c("ResultProper")]), type = "response")[, s_lambda.a]
    
    varImp[[g]] = tibble::rownames_to_column(varImp(modelX, lambda = modelX$lambda[s_lambda.a]), var = "Variable")
    colnames(varImp[[g]])[2] = paste("Overall:", g, sep = "")
    remove(modelX, train_temp, train.label)
    
  }
  
  pred = bind_cols(pred) %>%
    transmute(Predicted = rowMeans(.))
  
  insample.pred = bind_cols(insample.pred) %>%
    transmute(Predicted = rowMeans(.))
  
  varImp = varImp %>% reduce(left_join, by = "Variable") 
  
  means = varImp %>% select_if(is.numeric) %>% transmute(VariableImportance = rowMeans(.))
  
  varImp = tibble(Variable = varImp$Variable, meanImportance = means$VariableImportance)
  
  if(calibrate == TRUE){
    
    list(Predictions = platt.scale(label.train = label_train, predicted.prob.train = insample.pred, 
                                   predicted.prob.test = pred), VariableImportance = varImp)
    
  } else{
    
    list(Predictions = pred$Predicted, VariableImportance = varImp)  
    
  }
  
  
}

#..................................Platt Scaling/sigmoid........................................#
#..................................Not used, makes model worse..........................#
platt.scale = function(label.train, predicted.prob.train, predicted.prob.test){
  
  model = glm(formula = label.train ~ ., data = predicted.prob.train, family = binomial(link = "logit"))
  
  scaled.prob = predict(model, newdata = predicted.prob.test, type = c("response"))
  
  scaled.prob
  
}


#..................................Log Loss Function....................................#

logLoss = function(scores, label){
  
  if (is.factor(label)){
    u = ifelse(label ==  "W", 1,0)
  } else{
    u = label
  }
  
  tmp = data.frame(scores = scores, target = u)
  tmp = tmp %>% mutate(scores = ifelse(scores == 1, 0.9999999999999999999, ifelse(scores == 0 , 0.0000000000000000001, scores))) %>%
    mutate(logLoss = -(target * log(scores) + (1-target) * log(1-scores)))
  
  mean(tmp$logLoss)
}


#..................................PCA Function....................................#
#I tried to center and scale these variables after they were mistakenly left uncentered and unscaled (recall: the model has a loss function that is a function of the 
#magnitude of the parameters themselves, hence, it is vital we center and scale variables to be unitless so that the magnitude of such variables are not unfairly penalized.
#However....the validation results were quite different than before, roughly a drop of 0.03-0.04 in AUROC which is quite significant.)

addPCA_variables = function(traindata, testdata, standardize = FALSE){
  
  traindata_tmp = traindata[, !names(traindata) %in% c("ResultProper")] %>% select_if(., is.numeric)
  testdata_tmp = testdata[, !names(testdata) %in% c("ResultProper")] %>% select_if(., is.numeric)
  
  pca_parameters = prcomp(traindata_tmp, center = FALSE, scale. = FALSE)
  pca_traindata = predict(pca_parameters, newdata = traindata_tmp)[,1:5] %>% as_tibble(.) 
  
  if(standardize == TRUE){
    
    pca_train.params = caret::preProcess(pca_traindata, method = c("center", "scale"))
    pca_traindata = predict(pca_train.params, newdata = pca_traindata)
    pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5] %>% as_tibble(.) %>% predict(pca_train.params, newdata = .)
    
  }else{
    
    if(nrow(testdata) == 1){
      
      pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5] %>% as_tibble(., rownames = "id") %>% spread(., key = "id", value = value)
      
    }else{
      
      pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5] %>% as_tibble(.)
      
    }
  }
  
  
  list(train = bind_cols(traindata, pca_traindata), test = bind_cols(testdata, pca_newdata))
}

#..................................kNN Function....................................#
#Distances = cumulative distance from observation to the first, second, third, etc. nearest neighbour for each class label. 
#So for example, knn1 = distance from observation to first nearest neighbour that has label "W". knn2 = distance from observation
#to second nearest neighbour that has label "W". knn3 = distance from observation to first nearest neighbour that has label "L", 
#and knn3 = distance from observation to second nearest neighbour that has label "L".

#Probabilities = calculates the proportion of winners out of the k most closest neighbours to an observation

addKNN_variables = function(traindata, testdata, include_PCA = FALSE, distances = TRUE){
  
  #Selects variables that are top performing....
  traindata_tmp = traindata[, !names(traindata) %in% c("ResultProper")] %>% select(., H2H, WeightedGPS, Q2Record, PowerPlayOppurtunities, PenaltyKillPercentage, VegasOpeningOdds, "TOI% QoT_mean")
  testdata_tmp = testdata[, !names(testdata) %in% c("ResultProper")] %>% select(., H2H, WeightedGPS, Q2Record, PowerPlayOppurtunities, PenaltyKillPercentage, VegasOpeningOdds, "TOI% QoT_mean")
  
  if(include_PCA == TRUE){
    
    traindata_tmp = traindata %>% select_if(., is.numeric)
    testdata_tmp = testdata %>% select_if(., is.numeric)
    
  }else{
    
    traindata_tmp = traindata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    testdata_tmp = testdata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    
  }
  
  if (distances == TRUE){
    
    newframeswithKNN = fastknn::knnExtract(xtr = data.matrix(traindata_tmp), ytr = traindata$ResultProper, xte = data.matrix(testdata_tmp), k = 1, normalize = NULL)
    KNN_train = newframeswithKNN$new.tr %>% as_tibble(.) 
    
    KNN_train.params = caret::preProcess(KNN_train, method = c("center", "scale"))
    KNN_train = predict(KNN_train.params, newdata = KNN_train)
    
    KNN_test = newframeswithKNN$new.te %>% as_tibble(.) %>% predict(KNN_train.params, newdata = .)
    
    list(train = bind_cols(traindata, KNN_train), test = bind_cols(testdata, KNN_test))
    
  }else{
    
    KNN_train  = tibble(knn_W = fastknn(xtr = data.matrix(traindata_tmp), ytr = traindata$ResultProper, xte = data.matrix(traindata_tmp), k = 5, method = "vote", normalize = NULL)$prob[,2]) 
    
    KNN_train.params = caret::preProcess(KNN_train, method = c("center", "scale"))
    KNN_train = predict(KNN_train.params, newdata = KNN_train)
    
    KNN_test = tibble(knn_W = fastknn(xtr = data.matrix(traindata_tmp), ytr = traindata$ResultProper, xte = data.matrix(testdata_tmp), k = 5, method = "vote", normalize = NULL)$prob[,2]) %>%
      predict(KNN_train.params, newdata = .)
    
    list(train = bind_cols(traindata, KNN_train), test = bind_cols(testdata, KNN_test))
    
  }
  
}

#.......................Define recipe.............................................#

preProcess.recipe = function(trainX){
  
  mainRecipe = recipe(ResultProper ~., data=trainX) %>%
    step_dummy(all_predictors(), -all_numeric()) %>%
    step_interact(terms = ~ SRS:Fenwick:ELORating) %>%
    step_interact(terms = ~ H2H:VegasOpeningOdds) %>%
    step_interact(terms = ~ FaceoffWinPercentage:ShotPercentage) %>%
    step_interact(terms = ~ contains("Round"):VegasOpeningOdds) %>%
    step_interact(terms = ~ SDRecord:SOS) %>%
    step_zv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) 
  
  mainRecipe
}

#.........................Define outer pipe for the outer cross validation fold...........................................#

modelPipe.outer = function(lambda.final, alpha.final, processedData){
  
  train = processedData$Train
  test = processedData$Test
  
  model = baggedModel(train = train[, !names(train) %in% c("ResultProper")], test=test, label_train = train$ResultProper, 
                      alpha.a = alpha.final, s_lambda.a = lambda.final, calibrate = FALSE)
  
  #For AUROC
  #ROC = roc(response = test$ResultProper, predictor = model$Predictions, levels = c("L", "W"))$auc
  
  #For Log Loss
  
  logloss = logLoss(scores = model$Predictions, label = test$ResultProper)
  
  VarImp = model$VariableImportance
  
  list(Predictions = model$Predictions, LogLoss = logloss, VarImp = VarImp)
}

#.............................Process Folds...................................#

processFolds = function(fold.index, mainTrain){
  
  train.param = prep(preProcess.recipe(trainX = mainTrain[fold.index,]), training = mainTrain[fold.index,])
  train = bake(train.param, new_data = mainTrain[fold.index,])
  test = bake(train.param, new_data = mainTrain[-fold.index,])
  
  frameswithPCA = addPCA_variables(traindata = train, testdata = test)
  
  train = frameswithPCA$train
  test = frameswithPCA$test
  
  rm(train.param, frameswithPCA)
  
  frameswithKNN = addKNN_variables(traindata = train, testdata = test, distances = TRUE)
  
  train = frameswithKNN$train
  test = frameswithKNN$test
  
  rm(frameswithKNN)
  
  list(Train = train, Test = test)
  
}


#..........................Processed variable importance output from the base model................................#
processVarImp = function(varImpRaw){
  
  varImpNames = varImpRaw %>% 
    select(., contains("Variable")) %>%
    .[,1]
  
  final = varImpRaw %>% 
    select(., contains("Importance")) %>%
    transmute(Importance = rowMeans(.)) %>%
    bind_cols(varImpNames, .)
  
  final
}

#..........................Ensemble-simple average with different seeds................................#
train.ensemble = function(folds, seed.a, finalParameters, numofModels, processedData, label_test){
  
  finalPredictions = vector("list", numofModels)
  finalVarImp = vector("list", numofModels)
  
  set.seed(seed.a)
  seeds.EachModel = sample(1:1000000000, numofModels, replace = FALSE)
  
  for (k in 1:length(seeds.EachModel)){
    
    finalModel = modelPipe.outer(lambda.final = as.integer(finalParameters$lambda[k]), alpha.final = finalParameters$alpha[k], processedData = processedData)
    
    finalPredictions[[k]] = finalModel$Predictions
    finalVarImp[[k]] = finalModel$VarImp
    
  }
  
  finalPredictions.processed = finalPredictions %>% reduce(cbind) %>% rowMeans(.)
  finalVarImp.processed = finalVarImp %>% reduce(left_join, by = "Variable") %>% processVarImp(.)
  
  list(LogLoss = logLoss(scores = finalPredictions.processed, label = label_test), 
       VarImp = finalVarImp.processed)
  
}