#Set the directory for parallel computation status checks. Change this to any folder on your computer so that we can monitor 
#the status of the repeated cross validation.
setwd("/home/brayden/Desktop/status")

#Dependencies

library(glmnet)
library(caret)
library(tidyverse)
library(recipes)
library(moments)
library(parallel)
library(ParBayesianOptimization)
library(fastknn)
library(boot)

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

modelPipe.outer = function(lambda.final, alpha.final, processedData, variableImportance, subset.n){
  
  variableImportance = variableImportance %>% arrange(-Importance) %>% .[1:subset.n, ]
  
  train = processedData$Train %>% select(variableImportance$Variable, ResultProper) 
  test = processedData$Test %>% select(variableImportance$Variable, ResultProper)  
  
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

#..........................RFE Selection......................................#

rfeSelection = function(allProcessedFrames, subset.size, varImp){
  
  varImp.sorted = varImp %>% arrange(-Importance) %>% .[1:subset.size,] %>% select(Variable) 
  
  bestParam = BayesianOptimization(FUN =  function(alpha, lambda){
    
    scores = vector("numeric", length(allProcessedFrames))
    varImp = vector("list", length(allProcessedFrames))
    
    for(m in 1:length(allProcessedFrames)){
      
      Train = allProcessedFrames[[m]]$Train %>% select(varImp.sorted$Variable, ResultProper)
      Test = allProcessedFrames[[m]]$Test %>% select(varImp.sorted$Variable, ResultProper)
      
      model = baggedModel(train = Train, test = Test, label_train = Train$ResultProper, alpha = alpha, s_lambda.a = as.integer(lambda), calibrate = FALSE)
      scores[m] = logLoss(scores = model$Predictions, label = Test$ResultProper)
      varImp[[m]] = model$VariableImportance
      
    }
    
    list(Score = -mean(scores))
    
  }
  , bounds = list(alpha = c(0, 1), lambda = c(15L, 90L)),
  initPoints = 3, nIters = 45, convThresh = 1e+02, verbose = 0)
  
  tibble(Subset.Size = subset.size, alpha = bestParam$ScoreDT$alpha[which.max(bestParam$ScoreDT$Score)], lambda = as.integer(bestParam$ScoreDT$lambda[which.max(bestParam$ScoreDT$Score)]))
  
}
#..........................Global Envrionment..............................................................#
set.seed(40689)
allSeeds = sample(1:1000000000, 60, replace = FALSE)

giveResults = function(seed, allData, allSeeds){
  
  writeLines(paste("Seed Index in allSeeds:", which(allSeeds == seed)))
  
  set.seed(seed)
  allFolds = caret::createDataPartition(y = allData$ResultProper, times = 1, p = 0.80)
  mainTrain = allData[allFolds[[1]], ]
  
  set.seed(seed)
  innerFolds = caret::createMultiFolds(y = mainTrain$ResultProper, k = 3, times = 1)
  
  innerFolds.temp = innerFolds[str_detect(string = names(innerFolds), pattern = paste("Rep", 1, sep = ""))]
  allProcessedFrames = lapply(innerFolds.temp, FUN = processFolds, mainTrain = mainTrain)
  
  bestParam = BayesianOptimization(FUN =  function(alpha, lambda){
        
    scores = vector("numeric", length(allProcessedFrames))
    varImp = vector("list", length(allProcessedFrames))

      for(m in 1:length(allProcessedFrames)){
                                     
        model = baggedModel(train = allProcessedFrames[[m]]$Train, test = allProcessedFrames[[m]]$Test, label_train = allProcessedFrames[[m]]$Train$ResultProper, alpha = alpha, s_lambda.a = as.integer(lambda), calibrate = FALSE)
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
  
  rfeResults = bind_rows(lapply(c(100, 95, 110), FUN = rfeSelection, allProcessedFrames = allProcessedFrames, varImp = finalVarImp)) %>%
    bind_rows(tibble(Subset.Size = nrow(finalVarImp), alpha = processed.bestParam$alpha[1], lambda = as.integer(processed.bestParam$lambda[1])))
  
  processedData = processFolds(fold.index = allFolds[[1]], mainTrain = allData)
  finalTestSet.Score = mapply(modelPipe.outer, subset.n = rfeResults$Subset.Size, lambda.final = rfeResults$lambda, alpha.final = rfeResults$alpha, MoreArgs = list(processedData = processedData,
                                                                                                                                                                    variableImportance = finalVarImp), SIMPLIFY = FALSE) %>%
    lapply(., FUN = function(x){x$LogLoss}) 
  
  tibble(Subset = rfeResults$Subset.Size, Score = unlist(finalTestSet.Score))  
}

cluster = makeCluster(detectCores(), outfile = "messages.txt")
setDefaultCluster(cluster)

clusterEvalQ(cluster, c(library(caret), library(forecast), library(tidyverse), source("/home/brayden/GitHub/NHLPlayoffs/Modelling/All Functions - RFE.R")))
results = parLapply(NULL, allSeeds, fun = giveResults, allData = allData, allSeeds = allSeeds)

stopCluster(cluster)
rm(cluster)

processedResults = results %>% reduce(., left_join, by = "Subset")
finalLogLoss = processedResults %>% select(-Subset) %>% transmute(Overall = rowMeans(.)) %>% bind_cols(Subset = processedResults$Subset, .)

#...................................Bootstrap each vector from each rfe selection..............................#

mean.custom = function(x, d){

  mean(x[d])

}

bootStrap = function(rowValue, fun, processedResults, subsets){
  
bootstrapped.All.CI = boot.ci(boot(data = as.numeric(processedResults[rowValue, 2:ncol(processedResults)]), statistic = fun, R = 100000), type = "basic")

paste("Final LogLoss for Subset Size ", subsets[rowValue], ": ", round(bootstrapped.All.CI$t0,5), " with a 95% confidence interval given by via. Bootstrapping: ", "[", round(bootstrapped.All.CI$basic[1,4],5), ", ",
      round(bootstrapped.All.CI$basic[1,5],5), "]", sep = "")

}

#Paste results.
lapply(1:nrow(finalLogLoss), FUN = bootStrap, fun = mean.custom, processedResults = processedResults, subsets = finalLogLoss$Subset)