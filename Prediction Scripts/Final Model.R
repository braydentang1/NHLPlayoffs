library(tidyverse)
library(glmnet)
library(recipes)
library(caret)
library(pROC)
library(foreach)
library(doParallel)

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

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") %>%
            filter(Year == 2019) %>%
            select(Team1, Team2, Highest.Seed)

#...................................Engineering of some features..................#

allData = allData %>% 
  mutate(Round = as.factor(c(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),13),rep(1,8)))) %>%
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


#..................................Separate out the new observations from all prior data........................#

newdata = allData %>% filter(., is.na(ResultProper))
allData = allData %>% filter(., !is.na(ResultProper))


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

randomGridSearch = function(innerTrainX, innerTestX, grid){
  
  for(m in 1:nrow(grid)){
    
    writeLines(paste("Iteration:", m, sep = " "))
    
    modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                         label_train = innerTrainX$ResultProper, alpha.a = as.numeric(grid[m, 1]), s_lambda.a = as.integer(grid[m,2]))
    
    grid[m, 3] = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc
    
  }
  
  grid
  
}

#.........................Define inner pipe for the inner cross validation...........................................#
modelPipe.inner = function(mainTrain, seed.a, iterations){
  
  set.seed(seed.a)  
  innerFolds = createFolds(y = mainTrain$ResultProper, k = 3)

  cluster = makeCluster(detectCores())
  registerDoParallel(cluster)
  
  #Create grid
  
  set.seed(seed.a)  
  grid = tibble(alpha = as.numeric(runif(n = iterations, min = 0, max = 1)), s.lambda_val = as.integer(sample(15:90, iterations, replace = TRUE)), score = rep(0, iterations)) 
  
results = foreach(m = 1:length(innerFolds), .packages = c("tidyverse", "pROC", "glmnet", "caret", "recipes", "fastknn")) %dopar% {
    
    #Load some custom functions
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R")
  
  train.param = prep(preProcess.recipe(trainX = mainTrain[-innerFolds[[m]],]), training = mainTrain[-innerFolds[[m]],])
  train = bake(train.param, new_data = mainTrain[-innerFolds[[m]],])
  test = bake(train.param, new_data = mainTrain[innerFolds[[m]],])
  
  frameswithPCA = addPCA_variables(traindata = train, testdata = test)
  
  train = frameswithPCA$train
  test = frameswithPCA$test
  
  rm(train.param, frameswithPCA)
  
  frameswithKNN = addKNN_variables(traindata = train, testdata = test, distances = TRUE)
  
  train = frameswithKNN$train
  test = frameswithKNN$test
  
  rm(frameswithKNN)
  
  randomGridSearch(innerTrainX = train, innerTestX = test, grid = grid)
  
  }
  
  stopCluster(cluster)
  rm(cluster)

  processedResults = results %>% 
    reduce(left_join, by = c("alpha", "s.lambda_val")) %>% 
    select(., contains("score")) %>%
    transmute(Average = rowMeans(.)) %>%
    bind_cols(grid[,1:2], .)

  alpha = as.numeric(processedResults[which.max(processedResults$Average), 1])
  lambda = as.integer(processedResults[which.max(processedResults$Average), 2])
  
  list(alpha = alpha, lambda = lambda, validation.score = max(processedResults$Average)) 
  
}

#...........................Prediction Script.................................................................................#
#Requires new data samples. 

predict.NHL = function(training, newdata, finalParameters){
  
  recipe.parameters = prep(preProcess.recipe(training), training = training)
  
  training = bake(recipe.parameters, new_data = training)
  
  newdata = newdata %>% bake(recipe.parameters, new_data = .)
  
  frameswithPCA = addPCA_variables(traindata = training, testdata = newdata, standardize = FALSE)
  
  training = frameswithPCA$train
  newdata = frameswithPCA$test
  
  rm(frameswithPCA)
  
  frameswithKNN = addKNN_variables(traindata = training, testdata = newdata, distances = TRUE)
  
  training = frameswithKNN$train
  newdata = frameswithKNN$test
  
  rm(frameswithKNN)
  
  baggedModel(train = training, test = newdata, label_train = training$ResultProper, alpha.a = finalParameters$alpha, s_lambda.a = finalParameters$lambda)$Predictions
  
}

#...........................Global.........................................#

finalParameters = modelPipe.inner(mainTrain = allData, seed.a = 40689, iterations = 125)

predictions = predict.NHL(training = allData, newdata = newdata, finalParameters = finalParameters)

finalScores = template %>% bind_cols(., Prob.Win.HighestSeed = predictions)

