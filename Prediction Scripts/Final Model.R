library(tidyverse)
library(glmnet)
library(recipes)
library(caret)
library(parallel)

source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/logLoss.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/plattScale.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/processVarImp.R")

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
  mutate(Round = as.factor(c(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),14)))) %>%
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

newdata = allData %>% .[208:209,] %>% mutate(ResultProper = rep(NA, nrow(.)))
allData = allData[1:207, ]

#.........................Define inner pipe for the inner cross validation...........................................#

modelPipe.inner = function(mainTrain, seed.a, iterations){
  
  set.seed(seed.a)  
  innerFolds = createFolds(y = mainTrain$ResultProper, k = 3)

  results = vector("list", length(innerFolds))
  #Create grid
  
  set.seed(seed.a)  
  grid = tibble(alpha = as.numeric(runif(n = iterations, min = 0, max = 1)), s.lambda_val = as.integer(sample(15:90, iterations, replace = TRUE)), score = rep(0, iterations)) 

for(m in 1:length(innerFolds)){
  
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
  
  results[[m]] = randomGridSearch(innerTrainX = train, innerTestX = test, grid = grid)
  
}
  
  processedResults = results %>% 
    reduce(left_join, by = c("alpha", "s.lambda_val")) %>% 
    select(., contains("score")) %>%
    transmute(Average = rowMeans(.)) %>%
    bind_cols(grid[,1:2], .)
  
  alpha = as.numeric(processedResults[which.min(processedResults$Average), 1])
  lambda = as.integer(processedResults[which.min(processedResults$Average), 2])
  
  list(alpha = alpha, lambda = lambda, validation.score = min(processedResults$Average)) 
  
}

#...........................Preprocess all the data; give the recipe parameters for all newdata........#

preProcessData = function(data){
  
  recipe.parameters = prep(preProcess.recipe(data), training = data)
  
  processedDataSet = bake(recipe.parameters, new_data = data) 
  
  list(Data = processedDataSet, recipeParameters = recipe.parameters)
}

#...........................Prediction Script.................................................................................#
#Requires new data samples. 

predict.NHL = function(processedDataSet, recipeParameters, newdata, finalParameters){
  
  newdata = newdata[names(newdata) %in% c("ResultProper"), ]
  newdata = bake(recipeParameters, new_data = newdata)
  
  frameswithPCA = addPCA_variables(traindata = processedDataSet, testdata = newdata, standardize = FALSE)
  
  processedDataSet = frameswithPCA$train
  newdata = frameswithPCA$test
  
  rm(frameswithPCA)
  
  frameswithKNN = addKNN_variables(traindata = processedDataSet, testdata = newdata, distances = TRUE, include_PCA = FALSE)
  
  processedDataSet = frameswithKNN$train
  newdata = frameswithKNN$test
  
  rm(frameswithKNN)
  
  predictions = vector("list", nrow(finalParameters))
  varImp = vector("list", nrow(finalParameters))
  
  for (i in 1:nrow(finalParameters)){
    
  model = baggedModel(train = processedDataSet, test = newdata, label_train = processedDataSet$ResultProper, alpha.a = finalParameters$alpha[i], s_lambda.a = finalParameters$lambda[i], calibrate = FALSE)
  predictions[[i]] = model$Predictions
  varImp[[i]] = model$VariableImportance
  
  }
  
  finalPredictions.processed = predictions %>% reduce(cbind) %>% rowMeans(.)
  finalVarImp.processed = varImp %>% reduce(left_join, by = "Variable") %>% processVarImp(.)
  
  list(Prediction = finalPredictions.processed, Variable.Importance = finalVarImp.processed)
}

#...........................Global.........................................#

set.seed(40689)
seeds.Model = sample(1:1000000000, 5)

#...............If you want to retrain the model, uncomment this and run.................#

cluster = makeCluster(detectCores(), outfile = "messages.txt")
setDefaultCluster(cluster)

# Load packages on each cluster
clusterEvalQ(cluster, c(library(caret), library(recipes), library(tidyverse), library(glmnet),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/logLoss.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/plattScale.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/modelPipe_inner.R"),
                        source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")))

finalParameters = parLapply(NULL, seeds.Model, fun = modelPipe.inner, mainTrain = allData, iterations = 90) %>% reduce(bind_rows)
stopCluster(cluster)
rm(cluster)

saveRDS(finalParameters, file = "finalParameters-sf.rds")

processedData = preProcessData(data = allData)
saveRDS(processedData, file = "processedData-sf.rds")

#.................Otherwise, run this instead..................................#

# finalParameters = readRDS("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/RDS Objects/finalParameters.rds")
# processedData = readRDS("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/RDS Objects/processedData.rds")

#.................Find, and then present final predictions....................................#
 
predictions = predict.NHL(processedDataSet = processedData$Data, recipeParameters = processedData$recipeParameters, newdata = newdata, finalParameters = finalParameters)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") %>%
  filter(Year == 2019, Round == "finals") %>%
  select(Team1, Team2, Highest.Seed)

finalScores = template %>% bind_cols(., Prob.Win.HighestSeed = predictions$Prediction)