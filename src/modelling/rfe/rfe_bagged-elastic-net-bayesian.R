#Load required functions 
source("/home/brayden/GitHub/NHLPlayoffs/Modelling/RFE/All Functions - RFE.R")
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

#..........................Global Envrionment..............................................................#
set.seed(40689)
allSeeds = sample(1:1000000000, 60, replace = FALSE)

giveResults = function(seed, allData, times = 20, p = 0.8, k = 3, nIters = 42, useOnlyVariables = NULL, subsets){
  
  ############################################################################################
  # Runs the entire modelling pipeline from start to finish. Uses recursive feature elimination to choose variables.
  #
  # Arguments:
  #
  # seed -- an integer to determine data splitting
  # allData -- the entire dataset to train and evaluate the model using k-fold cross validation and Monte Carlo cross validation.
  # times -- number of models to include in each bootstrap model. Default = 20.
  # p -- numeric value in [0,1] to determine how much of the entire dataset should be used for training. Default = 0.8.
  # k -- integer value specifying the number of folds in k-fold cross validation for hyperparameter tuning. Default = 3.
  # nIters -- an integer specifying the number of rounds to use in Bayesian Optimization for hyperparameter tuning. Default = 42.
  # useOnlyVariables -- a character vector that gives specific variables to use when producing kNN variables. Default = NULL.
  # subsets -- a vector of integers that specifies the top subsets of variables to select in recursive feature eliminaation. 
  #
  # Returns:
  #
  # tibble
  #  A tibble with the subset size and log loss on a test set for a specific seed.
  #
  ############################################################################################
  set.seed(seed)
  allFolds = caret::createDataPartition(y = allData$ResultProper, times = 1, p = p)
  mainTrain = allData[allFolds[[1]], ]
  
  set.seed(seed)
  innerFolds = caret::createMultiFolds(y = mainTrain$ResultProper, k = k, times = 1)
  
  innerFolds.temp = innerFolds[str_detect(string = names(innerFolds), pattern = paste("Rep", 1, sep = ""))]
  allProcessedFrames = lapply(innerFolds.temp, FUN = processFolds, mainTrain = mainTrain, )
  
  bestParam = BayesianOptimization(FUN =  function(alpha, lambda){
        
    scores = vector("numeric", length(allProcessedFrames))
    varImp = vector("list", length(allProcessedFrames))

      for(m in 1:length(allProcessedFrames)){
                                     
        model = baggedModel(train = allProcessedFrames[[m]]$Train, test = allProcessedFrames[[m]]$Test, label_train = allProcessedFrames[[m]]$Train$ResultProper, alpha = alpha, s_lambda.a = as.integer(lambda), times = times, calibrate = FALSE)
        scores[m] = logLoss(scores = model$Predictions, label = allProcessedFrames[[m]]$Test$ResultProper)
        varImp[[m]] = model$VariableImportance

        }
    
    varImp = varImp %>% reduce(., left_join, by = "Variable") %>% processVarImp(.)    
    
    list(Score = -mean(scores), VariableImportance = varImp)
    
    }
    , bounds = list(alpha = c(0, 1), lambda = c(15L, 90L)),
                                   initPoints = 3, nIters = 45, convThresh = 1e+02, verbose = 1)
  
  processed.bestParam = bestParam$ScoreDT[which(bestParam$ScoreDT$Score == max(bestParam$ScoreDT$Score)),]
  finalVarImp = processed.bestParam %>% select(contains("VariableImportance")) %>% rename(Variable = VariableImportance.Variable, Importance = VariableImportance.Importance)
  
  rm(innerFolds.temp, bestParam)
  
  rfeResults = bind_rows(lapply(subsets, FUN = rfeSelection, allProcessedFrames = allProcessedFrames, varImp = finalVarImp)) %>%
    bind_rows(tibble(Subset.Size = nrow(finalVarImp), alpha = processed.bestParam$alpha[1], lambda = as.integer(processed.bestParam$lambda[1])))
  
  processedData = processFolds(fold.index = allFolds[[1]], mainTrain = allData)
  finalTestSet.Score = mapply(modelPipe.outer, subset.n = rfeResults$Subset.Size, lambda.final = rfeResults$lambda, alpha.final = rfeResults$alpha, MoreArgs = list(processedData = processedData,
                                                                                                                                                                    variableImportance = finalVarImp), SIMPLIFY = FALSE) %>%
    lapply(., FUN = function(x){x$LogLoss}) 
  
  tibble(Subset = rfeResults$Subset.Size, Score = unlist(finalTestSet.Score))  
}



results = mclapply(X = seeds, FUN = giveResults, allData = allData, times = 20, p = 0.8, k = 3, nIters = 42, useOnlyVariables = c("H2H", "WeightedGPS", "Q2Record", "PowerPlayOppurtunities", "PenaltyKillPercentage", "VegasOpeningOdds", "TOI% QoT_mean"),
                   subsets = c(95, 100, 105))

processedResults = results %>% reduce(., left_join, by = "Subset")
finalLogLoss = processedResults %>% select(-Subset) %>% transmute(Overall = rowMeans(.)) %>% bind_cols(Subset = processedResults$Subset, .)
