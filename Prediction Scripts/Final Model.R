library(tidyverse)
library(glmnet)
library(recipes)
library(caret)
library(pROC)

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/")

#..................................Bagging Function...................................#
baggedModel = function(train, test, label_train, alpha.a, s_lambda.a){
  
  set.seed(40689)
  samples = caret::createResample(y = label_train, times = 15)
  pred = vector("list", length(samples))
  varImp = vector("list", length(samples))
  
  for (g in 1:length(samples)){
    train_temp = train[samples[[g]], ]
    a = label_train[samples[[g]]]
    modelX = glmnet(x = data.matrix(train_temp[, !names(train_temp) %in% c("ResultProper")]), y = a, family = "binomial", alpha = alpha.a, nlambda = 120, standardize = FALSE)
    s_lambda.a = case_when(s_lambda.a > length(modelX$lambda) ~ length(modelX$lambda), TRUE ~ s_lambda.a)
    pred[[g]] = predict(modelX, newx = data.matrix(test[, !names(test) %in% c("ResultProper")]), type = "response")[, s_lambda.a]
    varImp[[g]] = tibble::rownames_to_column(varImp(modelX, lambda = modelX$lambda[s_lambda.a]), var = "Variable")
    colnames(varImp[[g]])[2] = paste("Overall:", g, sep = "")
    remove(modelX, train_temp, a)
  }
  
  pred = bind_cols(pred) %>%
    transmute(Predicted = rowMeans(.))
  
  varImp = varImp %>% Reduce(function(x,y) left_join(x,y, by = "Variable"), .) 
  
  means = varImp %>% select_if(is.numeric) %>% transmute(VariableImportance = rowMeans(.))
  
  varImp = tibble(Variable = varImp$Variable, meanImportance = means$VariableImportance)
  
  list(Predictions = pred$Predicted, VariableImportance = varImp)
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
    
    pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5] %>% as_tibble(.) 
    
  }
  
  
  list(train = bind_cols(traindata, pca_traindata), test = bind_cols(testdata, pca_newdata))
}


#..................................Read data in....................................#
#Change directories to pull in data from the "Required Data Sets" folder located in the repository.

cat("Reading in Data..... \n")
allData = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/HockeyReference2.csv") %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/HockeyReference1.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/CorsicaAllTeamStats.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/CorsicaGameScoreStats.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/ELORatings.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/ESPNStats.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/FenwickScores.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/NHLOfficialStats.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/SCFScores.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/VegasOddsOpening.csv")) %>%
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/EvolvingHockey_WAR.csv")) %>%
  mutate(ResultProper = as.factor(ResultProper))

#...................................Read in Variable Importance List After RFE....................#

VarImportance.List = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Model Output/finalVarImp.March28th.csv") %>% .[,1]

#...................................Engineering of some features..................#

allData = allData %>% 
  mutate(Round = as.factor(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),13))) %>%
  mutate(PenaltyMinstoPowerPlaylog = sign(PenaltyMinsPG*60*82 /PowerPlayPercentage) * log(abs(PenaltyMinsPG*60*82 /PowerPlayPercentage) + 1)) %>%
  mutate(Ratio_of_SRStoPoints = (SRS/Points)^1/3) %>%
  mutate(AverageGoalDiff_PerGame = GoalsFor/82) %>%
  mutate(AveragePenaltyDiff_PerGame = PenaltyMinsPG/82) %>%
  mutate(PowerPlaytoPenaltyKill = sign(PowerPlayPercentage/PenaltyKillPercentage) * log(abs(PowerPlayPercentage/PenaltyKillPercentage) + 1)) %>%
  mutate(PPO_x_PenaltyKill = PowerPlayOppurtunities * PenaltyKillPercentage) %>%
  mutate(PointsPercentage = Points/164) %>%
  mutate(GS_max_log = sign(GS_mean) * log(abs(GS_mean) + 1)) %>%
  mutate(CA_Per60Team_log = sign(CA_Per60Team) * log(abs(CA_Per60Team) + 1)) %>%
  mutate(Ratio_of_GoalstoGoalsAgainstlog = sign(GoalsFor/GoalsAgainst) * log(abs(GoalsFor/GoalsAgainst) +1)) %>%
  mutate(Ratio_of_HitstoBlockslog = sign(HitsatES/BlocksatES) * log(abs(HitsatES/BlocksatES) + 1)) %>%
  mutate(SCFtoGoalsAgainstlog = sign(SCF/GoalsAgainst) * log(abs(SCF/GoalsAgainst) + 1)) %>%
  mutate(CorsiDifftoSOSlog = sign((CF_Per60Team - CA_Per60Team)/SOS) * log(abs((CF_Per60Team - CA_Per60Team)/SOS) + 1)) %>%
  mutate(xGDifftoSOS = (xGF.60 - xGA.60)/SOS) %>% 
  mutate(GStoSOS = GS_mean / SOS) %>%
  mutate(SRStoSOS = SRS/SOS) %>%
  mutate("ixGF/60_max.TO.Rel CF%_max" = allData$'Rel CF%_max' / allData$'ixGF/60_max') %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0,.))) %>%
  mutate_if(is.numeric, funs(ifelse(is.infinite(.), 0,.)))

#.......................Define recipe.............................................#

preProcess.recipe = function(trainX){
  
  mainRecipe = recipe(ResultProper ~., data=trainX) %>%
    step_dummy(all_predictors(), -all_numeric()) %>%
    step_zv(all_predictors()) %>%
    step_interact(terms = ~ SRS:Fenwick:ELORating) %>%
    step_interact(terms = ~ H2H:VegasOpeningOdds) %>%
    step_interact(terms = ~ RegularSeasonWinPercentage:contains("Points")) %>%
    step_interact(terms = ~ FaceoffWinPercentage:ShotPercentage) %>%
    step_interact(terms = ~ contains("Round"):VegasOpeningOdds) %>%
    step_interact(terms = ~ SDRecord:SOS) %>%
    step_zv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) 
  
  mainRecipe
}

#........................Define random search function..................................#

randomGridSearch = function(iterations, innerTrainX, innerTestX, seed.a){
  
  score = 0
  alpha.fin = numeric(1)
  lambda.fin = integer(1)
  
  set.seed(seed.a)
  alpha_val = as.numeric(runif(n = iterations, min = 0, max = 1))
  s.lambda_val = as.integer(sample(1:90, iterations, replace = TRUE))
  
  for(m in 1:iterations){
    
    writeLines(paste("Iteration:", m, sep = " "))
    
    modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                         label_train = innerTrainX$ResultProper, alpha.a = alpha_val[m], s_lambda.a = s.lambda_val[m])
    
    score.new = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc
    
    if(score.new > score){
      alpha.fin = alpha_val[m]
      lambda.fin = s.lambda_val[m]
      score = score.new
    }
  }
  list(alpha = alpha.fin, lambda = lambda.fin)
}

#.........................Define inner pipe for the inner cross validation...........................................#
modelPipe.inner = function(mainTrain, seed.a){
  
  set.seed(seed.a)  
  innerFolds = createDataPartition(y = mainTrain$ResultProper, times = 1, p = 0.75)
  
  train.param = prep(preProcess.recipe(trainX = mainTrain[innerFolds[[1]],]), training = mainTrain[innerFolds[[1]],])
  train = bake(train.param, new_data = mainTrain[innerFolds[[1]],])
  test = bake(train.param, new_data = mainTrain[-innerFolds[[1]],])
  
  frameswithPCA = addPCA_variables(traindata = train, testdata = test)
  
  train = frameswithPCA$train
  test = frameswithPCA$test
  
  rm(train.param, frameswithPCA)
  
  #NEW: Subset top 25 variables from VarImportance List, found after 80 iterations of RFE
  
  train = train %>% select(., ResultProper, VarImportance.List$Variable[1:25])
  test = test %>% select(., ResultProper, VarImportance.List$Variable[1:25])
  
  results = randomGridSearch(iterations = 125, innerTrainX = train, innerTestX = test, seed = seed.a)
  
  
  list(alpha = results$alpha, lambda = results$lambda)
  
}

#...........................Find the final model parameters.................................................................#

retune.model = function(data){
    finalParameters = bind_cols(modelPipe.inner(mainTrain = allData, seed.a = 40689))
    write_csv(finalParameters, "finalParameters.csv")
  }

#...........................Prediction Script.................................................................................#
#Requires new data samples. 

predict.NHL = function(training, newdata, finalParameters){
  
  baggedModel(train = training, test = newdata, label_train = training$ResultProper, alpha.a = finalParameters$alpha, s_lambda.a = finalParameters$lambda)$Predictions
  
}





