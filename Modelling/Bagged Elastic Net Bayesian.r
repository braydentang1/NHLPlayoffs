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
  mutate_if(is.numeric, .funs = list(~ ifelse(is.nan(.), 0,.))) %>%
  mutate_if(is.numeric, .funs = list(~ ifelse(is.infinite(.), 0,.))) 

#...................................Check skewness and kurtosis..................#

kurt = allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::kurtosis(., na.rm=TRUE))) %>%
                                         gather(., Variable, Kurtosis)
skew = allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                         gather(., Variable, Skewness) %>%
                                         left_join(., kurt, by = "Variable")
rm(kurt)
allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                          gather(., Variable, Skew) %>%
                                          filter(., abs(Skew) >= 1)


#..........................Global Envrionment..............................................................#
set.seed(40689)
allSeeds = sample(1:1000000000, 42, replace = FALSE)

giveResults = function(seed, allData, times = 20, p = 0.8, k = 3, numofModels = 5, nIters = 42, useOnlyVariables = NULL){
  
  ############################################################################################
  # Runs the entire modelling pipeline from start to finish.
  #
  # Arguments:
  #
  # seed -- an integer to determine data splitting
  # allData -- the entire dataset to train and evaluate the model using k-fold cross validation and Monte Carlo cross validation.
  # times -- number of models to include in each bootstrap model. Default = 20.
  # p -- numeric value in [0,1] to determine how much of the entire dataset should be used for training. Default = 0.8.
  # k -- integer value specifying the number of folds in k-fold cross validation for hyperparameter tuning. Default = 3.
  # numofModels -- integer value that specifies the amount of models to fit in the ensemble. Default = 5.
  # nIters -- an integer specifying the number of rounds to use in Bayesian Optimization for hyperparameter tuning. Default = 42.
  # useOnlyVariables -- a character vector that gives specific variables to use when producing kNN variables. Default = NULL.
  #
  # Returns:
  #
  # list
  #  A list containing the log loss on the test set and the variable importance scores from the fitted model.
  #
  ############################################################################################
  
  writeLines(paste("Seed:", seed))
  
  set.seed(seed)
  allFolds = caret::createDataPartition(y = allData$ResultProper, times = 1, p = p)
  mainTrain = allData[allFolds[[1]], ]
  
  set.seed(seed)
  innerFolds = caret::createMultiFolds(y = mainTrain$ResultProper, k = k, times = numofModels)
  
  finalParameters = vector("list", length(innerFolds)/k)
  
  for(i in 1:(length(innerFolds)/k)){
  
  writeLines(paste("Fitting Five Models For Seed:", seed, "in Rep:",i))
    
  innerFolds.temp = innerFolds[str_detect(string = names(innerFolds), pattern = paste("Rep", i, sep = ""))]
  allProcessedFrames = lapply(innerFolds.temp, FUN = processFolds, mainTrain = mainTrain, useOnlyVariables = useOnlyVariables)
  
  writeLines(paste("Finished Processing Data For Seed:", seed, "in Rep:", i))
  
  bestParam = BayesianOptimization(FUN =  function(alpha, lambda){
        
    scores = vector("numeric", length(allProcessedFrames))
                                   
      for(m in 1:length(allProcessedFrames)){
                                     
        model = baggedModel(train = allProcessedFrames[[m]]$Train, test = allProcessedFrames[[m]]$Test, label_train = allProcessedFrames[[m]]$Train$ResultProper, alpha = alpha, s_lambda.a = as.integer(lambda),
                            times = times, calibrate = FALSE)
        scores[m] = logLoss(scores = model$Predictions, label = allProcessedFrames[[m]]$Test$ResultProper)

        rm(model)
        
      }
        

    list(Score = -mean(scores))
    
    }
    , bounds = list(alpha = c(0, 1), lambda = c(15L, 100L)), parallel = FALSE,
                                   initPoints = 4, nIters = nIters, convThresh = 100, verbose = 1)
  
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
  processedData = processFolds(fold.index = allFolds[[1]], mainTrain = allData, useOnlyVariables = useOnlyVariables)
  finalTestSet.Score = train.ensemble(folds = allFolds, times = times, finalParameters = finalParameters, processedData = processedData, label_test = allData$ResultProper[-allFolds[[1]]])
  
  writeLines(paste("Log Loss Test Set:", finalTestSet.Score$LogLoss, sep = " "))
  
  list(LogLoss = finalTestSet.Score$LogLoss, VarImp = finalTestSet.Score$VarImp)
  #finalTestSet.Score$LogLoss
  
}

results = mclapply(X = allSeeds, FUN = giveResults, allData = allData, useOnlyVariables = c("H2H", "WeightedGPS", "Q2Record", "PowerPlayOppurtunities", "PenaltyKillPercentage", "VegasOpeningOdds", "TOI% QoT_mean"),
                   p = 0.8, k = 3, times = 20, numofModels = 5, mc.cores = 6, mc.preschedule = FALSE)
#results = lapply(X = allSeeds, FUN = giveResults, allData = allData)

finalLogLoss = unlist(lapply(results, function(x) {x$LogLoss})) 
finalVarImp = processVarImp(varImpRaw = lapply(results, function(x) {x$VarImp}) %>% Reduce(function(x,y) left_join(x,y, by = "Variable"),.)) 

#...................................Paste the Results.........................................................#

paste("Final LogLoss from Repeats: ", round(mean(finalLogLoss),5))

#..............................................Graphing the log loss scores.........................#

# graphingParameters = tibble(LogLoss = finalLogLoss)
# 
# ggplot(data = graphingParameters, aes(graphingParameters$LogLoss), colour = "Hist") +
#   geom_histogram(bins = 10, binwidth = 0.01, colour = "green", fill = "darkgrey") +
#   labs(title = "40 Repeats of Nested Cross Validation; Using Data up To 2019 Round 3", x = "LogLoss", subtitle = "Bins = 10, Width = 0.01")
