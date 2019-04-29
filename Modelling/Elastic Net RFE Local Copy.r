
#Set the directory for parallel computation status checks. Change this to any folder on your computer so that we can monitor 
#the status of the repeated cross validation.
setwd("C:/Users/Brayden/Desktop/status")

#Dependencies

library(glmnet)
library(caret)
library(pROC)
library(tidyverse)
library(recipes)
library(moments)
library(doParallel)
library(foreach)
library(fastknn)
library(boot)

source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addKNN_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/addPCA_variables.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/logLoss.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/randomGridSearch.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/preProcess_recipe.R")
source("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Prediction Scripts/baggedModel.R")

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
  mutate(ResultProper = as.factor(ResultProper)) %>%
  filter(!is.na(ResultProper))

#...................................Engineering of some features..................#

allData = allData %>% 
  mutate(Round = as.factor(c(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),13),c(1,1,1,1,1,1,1,1)))) %>%
  mutate(PenaltyMinstoPowerPlaylog = sign(PenaltyMinsPG*60*82 /PowerPlayPercentage) * log(abs(PenaltyMinsPG*60*82 /PowerPlayPercentage) + 1)) %>%
  mutate(Ratio_of_SRStoPoints = (SRS/Points)^1/3) %>%
  mutate(AveragePenaltyDiff_PerGame = PenaltyMinsPG/82) %>%
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

options(repr.matrix.max.rows=600, repr.matrix.max.cols=200, scipen = 999)


#...................................Check skewness and kurtosis..................#

kurt = allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::kurtosis(., na.rm=TRUE))) %>%
                                         gather(., Variable, Kurtosis)
allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                         gather(., Variable, Skewness) %>%
                                         left_join(., kurt, by = "Variable")
rm(kurt)
allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                          gather(., Variable, Skew) %>%
                                          filter(., abs(Skew) >= 1)


#.........................Define outer pipe for the outer cross validation fold...........................................#

modelPipe.outer = function(folds, lambda.final, alpha.final, Variables = NULL){
  
  train.param = prep(preProcess.recipe(trainX = allData[folds[[1]],]), training = allData[folds[[1]],])
  train = bake(train.param, new_data = allData[folds[[1]], ])
  test = bake(train.param, new_data = allData[-folds[[1]], ])
  
  frameswithPCA = addPCA_variables(traindata = train, testdata = test)
  
  train = frameswithPCA$train
  test = frameswithPCA$test
  
  rm(train.param, frameswithPCA)
  
  frameswithKNN = addKNN_variables(traindata = train, testdata = test, distances = TRUE)
  
  train = frameswithKNN$train 
  test = frameswithKNN$test 
  
  rm(frameswithKNN)
  
  if(!is.null(Variables)){
    
    train = train %>% select(., ResultProper, Variables$Variable)
    test = test %>% select(., ResultProper, Variables$Variable)
    
  }
  
  model = baggedModel(train = train[, !names(train) %in% c("ResultProper")], test=test, label_train = train$ResultProper, 
                      alpha.a = alpha.final, s_lambda.a = lambda.final, calibrate = FALSE)
  
  #For AUROC
  #ROC = roc(response = test$ResultProper, predictor = model$Predictions, levels = c("L", "W"))$auc
  
  #For Log Loss
  
  logloss = logLoss(scores = model$Predictions, label = test$ResultProper)
  
  VarImp = model$VariableImportance
  
  list(LogLoss = logloss, VarImp = VarImp)
}


#.........................Define inner pipe for the inner cross validation...........................................#
modelPipe.inner = function(folds, seed.a, iterations, Variables = NULL){
  
  mainTrain = allData[folds[[1]], ]
  
  set.seed(seed.a)  
  innerFolds = createFolds(y = mainTrain$ResultProper, k = 3)
  
  #Create grid
  
  set.seed(seed.a)  
  grid = tibble(alpha = as.numeric(runif(n = iterations, min = 0, max = 1)), s.lambda_val = as.integer(sample(15:90, iterations, replace = TRUE)), score = rep(0, iterations)) 
  
  results = vector("list", length(grid))
  
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
    
    if(!is.null(Variables)){
      
      train = train %>% select(., ResultProper, Variables$Variable)
      test = test %>% select(., ResultProper, Variables$Variable)
      
    }
    
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

#................................RFE selection......................................#
#Takes the VarImp list from the model that uses all predictors. Then, takes a vector of subset.sizes to use in recursive elimination.

RFE = function(VarImp, allSubsets, allFolds, seed, iterations){

finalResults.RFE = vector("numeric", length(allSubsets))

for(i in 1:length(allSubsets)){

  VarImp.subset = VarImp %>% arrange(., -Importance) %>% .[1:allSubsets[i], 1] 
  
  bestParam.RFE = modelPipe.inner(folds = allFolds, seed.a = seed, iterations = iterations, Variables = VarImp.subset)
  finalResults.RFE[i] = modelPipe.outer(folds = allFolds, lambda.final = bestParam$lambda, alpha.final = bestParam$alpha, Variables = VarImp.subset)$LogLoss
  
  rm(bestParam.RFE)
  
}

tibble(Subset.Sizes = allSubsets, LogLoss = finalResults.RFE)

}

#...........................Global.........................................#

set.seed(40689)
seeds = sample(1:1000000000, 2, replace = FALSE)
LogLoss.status = rep(as.numeric(NA), length(seeds))
LogLoss.status.RFE = rep(as.numeric(NA), length(seeds))
subset.sizes = c(20,30,40)

cluster = makeCluster(detectCores(), outfile = "messages.txt")
registerDoParallel(cluster)

results = foreach(p = 1:length(seeds), .combine = "c", .packages = c("tidyverse", "glmnet", "caret", "pROC", "recipes", "fastknn")) %dopar% {
  
  set.seed(seeds[p])
  allFolds = caret::createDataPartition(y = allData$ResultProper, times = 1, p = 0.75)
  
  bestParam = modelPipe.inner(folds = allFolds, seed.a = seeds[p], iterations = 3)
  finalResults = modelPipe.outer(folds = allFolds, lambda.final = bestParam$lambda, alpha.final = bestParam$alpha)
  
  LogLoss.status[p] = finalResults$LogLoss

  VarImp = processVarImp(varImpRaw = as_tibble(finalResults$VarImp))
  
  rfeSelection = RFE(VarImp = VarImp, allSubsets = subset.sizes, allFolds = allFolds, seed = seeds[p], iterations = 3)
  
  writeLines(paste("REPEAT:", p, "...", "Running Average Log Loss Test Set:", mean(LogLoss.status, na.rm = TRUE), sep = " "))
  list(LogLoss = finalResults$LogLoss, VarImp = VarImp, RFE = rfeSelection)

}

stopCluster(cluster)
rm(cluster, ROC.status, ROC.status.RFE)

finalLogLoss = unlist(results[c(seq(1, length(results), 3))])
finalLogLoss.RFE = results[c(seq(3, length(results), 3))] %>% reduce(left_join, by = "Subset.Size")
RFE.data = as.numeric(finalLogLoss.RFE[2, 2:ncol(finalLogLoss.RFE)])
finalVarImp = processVarImp(varImpRaw = results[c(seq(2, length(results),3))] %>% reduce(bind_cols)) 

#...................................Bootstrap the vector finalROC and RFE.data..............................#

mean.custom = function(x, d){
  
  mean(x[d])
  
}

bootstrapped.All.CI = boot.ci(boot(data = finalROC, statistic = mean.custom, R = 9000), type = "basic")
bootstrapped.Sub.25 = boot.ci(boot(data = RFE.data, statistic = mean.custom, R = 9000), type = "basic")

#...................................Paste the Results.........................................................#

paste("Final LogLoss: ", round(mean(finalROC),5), " with a 95% confidence interval given by via. Bootstrapping: ", "[", round(bootstrapped.All.CI$basic[1,4],5), ", ", 
      round(bootstrapped.All.CI$basic[1,5],5), "]", sep = "")

paste("Final LogLoss for Top 75th Percentile: ", round(mean(RFE.data),5), " with a 95% confidence interval given by via. Bootstrapping: ", "[", round(bootstrapped.Sub.25$basic[1,4],5), ", ", 
      round(bootstrapped.Sub.25$basic[1,5],5), "]", sep = "")

finalLogLoss.RFE.processed = finalLogLoss.RFE %>% select_if(., is.numeric) %>% transmute(Overall.LogLoss = rowMeans(.)) %>% bind_cols(Subset.Size = c("All", subset.sizes),.)

