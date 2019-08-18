#Set the directory for parallel computation status checks. Change this to any folder on your computer so that we can monitor 
#the status of the repeated cross validation.
setwd("/home/brayden/Desktop/status")

source('/home/brayden/GitHub/NHLPlayoffs/Modelling/All Functions.R')

#..................................Read data in....................................#
#Change directories to pull in data from the "Required Data Sets" folder located in the repository.

cat("Reading in Data..... \n")
allData = read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/HockeyReference2.csv") %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/HockeyReference1.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/CorsicaAllTeamStats.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/CorsicaGameScoreStats.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/ELORatings.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/ESPNStats.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/FenwickScores.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/NHLOfficialStats.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/SCFScores.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/VegasOddsOpening.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/EvolvingHockey_WAR.csv")) %>%
  bind_cols(read_csv("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets/TimeRelatedPlayoffFeatures.csv")) %>%
  mutate(ResultProper = as.factor(ResultProper)) %>%
  filter(!is.na(ResultProper))

#...................................Engineering of some features..................#

allData = allData %>% 
  mutate(Round = as.factor(c(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),13),c(1,1,1,1,1,1,1,1,2,2,2,2,3,3)))) %>%
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
   
    rm(finalModel) 
    
  }
  
  finalPredictions.processed = finalPredictions %>% reduce(cbind) %>% rowMeans(.)
  finalVarImp.processed = finalVarImp %>% reduce(left_join, by = "Variable") %>% processVarImp(.)
  
  list(LogLoss = logLoss(scores = finalPredictions.processed, label = label_test), 
       VarImp = finalVarImp.processed)
  
}
#..........................Global Envrionment..............................................................#
#Forced to define a bunch of stuff globally because the package rBayesianOptimization doesn't let you pass any other arguments except the ones being tuned...
#I don't know why this wasn't thought of.
set.seed(40689)
allSeeds = sample(1:1000000000, 40, replace = FALSE)

giveResults = function(seed, allData){
  
  writeLines(paste("Seed:", seed))
  
  set.seed(seed)
  allFolds = caret::createDataPartition(y = allData$ResultProper, times = 1, p = 0.80)
  mainTrain = allData[allFolds[[1]], ]
  
  set.seed(seed)
  innerFolds = caret::createMultiFolds(y = mainTrain$ResultProper, k = 3, times = 5)
  
  finalParameters = vector("list", length(innerFolds)/3)
  
  for(i in 1:(length(innerFolds)/3)){
  
  writeLines(paste("Fitting Five Models For Seed:", seed, "in Rep:",i))
    
  innerFolds.temp = innerFolds[str_detect(string = names(innerFolds), pattern = paste("Rep", i, sep = ""))]
  allProcessedFrames = lapply(innerFolds.temp, FUN = processFolds, mainTrain = mainTrain)
  
  writeLines(paste("Finished Processing Data For Seed:", seed, "in Rep:", i))
  
  bestParam = BayesianOptimization(FUN =  function(alpha, lambda){
        
    scores = vector("numeric", length(allProcessedFrames))
                                   
      for(m in 1:length(allProcessedFrames)){
                                     
        model = baggedModel(train = allProcessedFrames[[m]]$Train, test = allProcessedFrames[[m]]$Test, label_train = allProcessedFrames[[m]]$Train$ResultProper, alpha = alpha, s_lambda.a = as.integer(lambda), calibrate = FALSE)
        scores[m] = logLoss(scores = model$Predictions, label = allProcessedFrames[[m]]$Test$ResultProper)

        rm(model)
        
      }
        

    list(Score = -mean(scores))
    
    }
    , bounds = list(alpha = c(0, 1), lambda = c(15L, 90L)), parallel = FALSE,
                                   initPoints = 4, nIters = 42, convThresh = 100, verbose = 1)
  
  writeLines(paste("Store Final Parameters For Seed:", seed, "in Rep:", i))
  finalParameters[[i]] = tibble(alpha = bestParam$ScoreDT$alpha[which.max(bestParam$ScoreDT$Score)], lambda = as.integer(bestParam$ScoreDT$lambda[which.max(bestParam$ScoreDT$Score)]))

  rm(innerFolds.temp, allProcessedFrames, bestParam)
  gc()
  
  }
  
  rm(i, mainTrain)
  gc()
  
  writeLines(paste("Bind Rows for Seed:", seed))
  finalParameters = bind_rows(finalParameters)
  
  writeLines(paste("Score the Test Set for Seed:", seed))
  processedData = processFolds(fold.index = allFolds[[1]], mainTrain = allData)
  finalTestSet.Score = train.ensemble(folds = allFolds, seed.a = seed, finalParameters = finalParameters, numofModels = length(innerFolds)/3, processedData = processedData, label_test = allData$ResultProper[-allFolds[[1]]])
  
  writeLines(paste("Log Loss Test Set:", finalTestSet.Score$LogLoss, sep = " "))
  
  list(LogLoss = finalTestSet.Score$LogLoss, VarImp = finalTestSet.Score$VarImp)
  #finalTestSet.Score$LogLoss
  
}

results = mclapply(X = allSeeds, FUN = giveResults, allData = allData, mc.cores = 6)
#results = lapply(X = allSeeds, FUN = giveResults, allData = allData)

finalLogLoss = unlist(lapply(results, function(x) {x$LogLoss})) 
finalVarImp = processVarImp(varImpRaw = lapply(results, function(x) {x$VarImp}) %>% Reduce(function(x,y) left_join(x,y, by = "Variable"),.)) 

#...................................Bootstrap the vector finalROC and RFE.data..............................#

mean.custom = function(x, d){
  
  mean(x[d])
  
}

bootstrapped.All.CI = boot.ci(boot(data = finalLogLoss, statistic = mean.custom, R = 100000), type = "basic")

#...................................Paste the Results.........................................................#

paste("Final LogLoss: ", round(mean(finalLogLoss),5), " with a 95% confidence interval given by via. Bootstrapping: ", "[", round(bootstrapped.All.CI$basic[1,4],5), ", ", 
      round(bootstrapped.All.CI$basic[1,5],5), "]", sep = "")

finalVarImp %>% arrange(., -Importance)

#..............................................Graphing the log loss scores.........................#

# graphingParameters = tibble(LogLoss = finalLogLoss)
# 
# ggplot(data = graphingParameters, aes(graphingParameters$LogLoss), colour = "Hist") +
#   geom_histogram(bins = 10, binwidth = 0.01, colour = "green", fill = "darkgrey") +
#   labs(title = "40 Repeats of Nested Cross Validation; Using Data up To 2019 Round 3", x = "LogLoss", subtitle = "Bins = 10, Width = 0.01")
