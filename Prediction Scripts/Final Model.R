library(tidyverse)
library(glmnet)
library(recipes)
library(caret)
library(pROC)
library(foreach)
library(doParallel)

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/")

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
  bind_cols(read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets/TimeRelatedPlayoffFeatures.csv")) %>%
  mutate(ResultProper = as.factor(ResultProper))

#...................................Engineering of some features..................#

allData = allData %>% 
  mutate(Round = as.factor(c(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),13),c(1,1,1,1,1,1,1,1,2,2,2,2)))) %>%
  mutate(PenaltyMinstoPowerPlaylog = sign(PenaltyMinsPG*60*82 /PowerPlayPercentage) * log(abs(PenaltyMinsPG*60*82 /PowerPlayPercentage) + 1)) %>%
  mutate(Ratio_of_SRStoPoints = (SRS/Points)^1/3) %>%
  mutate(PowerPlaytoPenaltyKill = sign(PowerPlayPercentage/PenaltyKillPercentage) * log(abs(PowerPlayPercentage/PenaltyKillPercentage) + 1)) %>%
  mutate(PPO_x_PenaltyKill = PowerPlayOppurtunities * PenaltyKillPercentage) %>%
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

#.................................To measure the log loss of the 2019 Round 1 Playoffs..........................#

#allData_tmp = allData[1:195,]
#newdata = allData[196:nrow(allData),] %>% filter(., !is.na(ResultProper))
#newdata_label = newdata$ResultProper

#.........................Define inner pipe for the inner cross validation...........................................#
modelPipe.inner = function(mainTrain, seed.a, iterations){
  
  set.seed(seed.a)  
  innerFolds = createFolds(y = mainTrain$ResultProper, k = 3)

  #Create grid
  
  set.seed(seed.a)  
  grid = tibble(alpha = as.numeric(runif(n = iterations, min = 0, max = 1)), s.lambda_val = as.integer(sample(15:90, iterations, replace = TRUE)), score = rep(0, iterations)) 
  
  cluster = makeCluster(detectCores())
  registerDoParallel(cluster)
  
results = foreach(m = 1:length(innerFolds), .packages = c("tidyverse", "pROC", "glmnet", "caret", "recipes", "fastknn")) %dopar% {
    
    #Load some custom functions
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/logLoss.R")
    source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/plattScale.R")
    
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

  alpha = as.numeric(processedResults[which.min(processedResults$Average), 1])
  lambda = as.integer(processedResults[which.min(processedResults$Average), 2])
  
  list(alpha = alpha, lambda = lambda, validation.score = min(processedResults$Average)) 
  
}

#...........................Prediction Script.................................................................................#
#Requires new data samples. 

#Load custom functions for function below.
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/logLoss.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/plattScale.R")

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
  
  model = baggedModel(train = training, test = newdata, label_train = training$ResultProper, alpha.a = finalParameters$alpha, s_lambda.a = finalParameters$lambda, calibrate = FALSE)
  
  list(Prediction = model$Predictions, Variable.Importance = model$VariableImportance)
}

#...........................Global.........................................#

finalParameters = modelPipe.inner(mainTrain = allData, seed.a = 89529, iterations = 130)

predictions = predict.NHL(training = allData, newdata = newdata, finalParameters = finalParameters)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") %>%
  filter(Year == 2019, Round == "semi-finals") %>%
  select(Team1, Team2, Highest.Seed)

finalScores = template %>% bind_cols(., Prob.Win.HighestSeed = predictions$Prediction)

#............................Log Loss 2019....................................#
#0.9735544, :(
#finalParameters_2018 = modelPipe.inner(mainTrain = allData_tmp, seed.a = 40689, iterations = 130)
#predictions = predict.NHL(training = allData_tmp, newdata = newdata, finalParameters = finalParameters_2018)

#logLoss(scores = predictions$Prediction, label = newdata_label)
