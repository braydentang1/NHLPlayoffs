
#Set the directory for parallel computation status checks. Change this to any folder on your computer so that we can monitor 
#the status of the repeated cross validation.
setwd("C:/Users/Brayden/Documents/NHLModel/Status")

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
    pred[[g]] = predict(modelX, newx = data.matrix(test[, !names(test) %in% c("ResultProper")]), type = "response")[, s_lambda.a]
    varImp[[g]] = varImp(modelX, lambda = modelX$lambda[s_lambda.a])
    colnames(varImp[[g]])[1] = paste("Overall:", g, sep = "")
    remove(modelX, train_temp, a)
  }
  
  pred = pred %>% Reduce(function(x,y) cbind(x,y),.) %>% as_tibble() %>%
    mutate(Predicted = rowMeans(.))
  
  varImp = varImp %>% Reduce(function(x,y) cbind(x,y),.) %>% as_tibble() %>%
    mutate(VariableImportance = rowMeans(.))
  
                             
  varImp = tibble::rownames_to_column(cbind.data.frame(meanImportance = varImp$VariableImportance), var = "Variable")
  
  list(Predictions = pred$Predicted, VariableImportance = varImp)
}

#..................................Log Loss Function....................................#

logLoss = function(scores, label){
  
  if (is.factor(label)){
    u = ifelse(label ==  "W", 1,0)
  } else{
    u = label
  }
  
  tmp = data.frame(scores = scores, target = u)
  tmp = tmp %>% mutate(scores = ifelse(scores == 1, 0.9999999999999999, ifelse(scores == 0 , 0.0000000000000001, scores))) %>%
    mutate(logLoss = -(target * log(scores) + (1-target) * log(1-scores)))
  
  mean(tmp$logLoss)
}


#..................................PCA Function....................................#

addPCA_variables = function(traindata, testdata){
    
    traindata_tmp = traindata[, !names(traindata) %in% c("ResultProper")] %>% select_if(., is.numeric)
    testdata_tmp = testdata[, !names(testdata) %in% c("ResultProper")] %>% select_if(., is.numeric)
    
    pca_parameters = prcomp(traindata_tmp, center = FALSE, scale. = FALSE)
    pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5]
    pca_traindata = predict(pca_parameters, newdata = traindata_tmp)[,1:5]
    list(train = cbind(traindata, pca_traindata), test = cbind(testdata, pca_newdata))
}

#..................................kNN Function....................................#
addKNN_variables = function(traindata, testdata, include_PCA = FALSE){
    
    y = traindata$ResultProper
    
    if(include_PCA == TRUE){
        
    traindata_tmp = traindata %>% select_if(., is.numeric) 
    testdata_tmp = testdata %>% select_if(., is.numeric) 
        
        }else{
  
    traindata_tmp = traindata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    testdata_tmp = testdata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    
    }
    
    newframeswithKNN = fastknn::knnExtract(xtr = data.matrix(traindata_tmp), ytr = y, xte = data.matrix(testdata_tmp), k = 2)
    KNN_train = newframeswithKNN$new.tr %>% as_tibble(.) %>% transmute_all(., .funs = function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE))
    KNN_test = newframeswithKNN$new.te %>% as_tibble(.) %>% transmute_all(., .funs = function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)) 
    list(train = bind_cols(traindata, KNN_train), test = bind_cols(testdata, KNN_test))
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
              mutate(ResultProper = as.factor(ResultProper))

#...................................Engineering of some features..................#
allData = allData %>% 
            mutate(Ratio_of_GoalstoGoalsAgainst = GoalsFor/GoalsAgainst) %>%
            mutate(Ratio_of_HitstoBlocks = HitsatES/BlocksatES) %>%
            mutate(logofPoints = sign(Points) * log(abs(Points) + 1)) %>%
            mutate(sqrtofPoints = abs(Points)^0.5) %>%
            mutate(PenaltyMinstoPowerPlay = PenaltyMinsPG*60*82 /PowerPlayPercentage) %>%
            mutate(Ratio_of_SRStoPoints = SRS/Points) %>%
            mutate(AverageGoalDiff_PerGame = GoalsFor/82) %>%
            mutate(AveragePenaltyDiff_PerGame = PenaltyMinsPG/82) %>%
            mutate(logofSOG = sign(SOG) * log(abs(SOG) + 1)) %>%
            mutate(sqrtofRPI = abs(RPI)^0.5) %>%
            mutate(PowerPlaytoPenaltyKill = PowerPlayPercentage/PenaltyKillPercentage) %>%
            mutate(PowerPlaytoPenaltyKill = sign(PowerPlaytoPenaltyKill) * log(abs(PowerPlaytoPenaltyKill) + 1)) %>%
            mutate(SCFtoGoalsAgainst = SCF/GoalsAgainst) %>%
            mutate(PointsPercentage = Points/164) %>%
            mutate(GS_max_log = sign(GS_mean) * log(abs(GS_mean) + 1)) %>%
            mutate(CA_Per60Team_log = sign(CA_Per60Team) * log(abs(CA_Per60Team) + 1)) %>%
            mutate(Ratio_of_GoalstoGoalsAgainstlog = sign(Ratio_of_GoalstoGoalsAgainst) * log(abs(Ratio_of_GoalstoGoalsAgainst) +1)) %>%
            mutate(Ratio_of_HitstoBlockslog = sign(Ratio_of_HitstoBlocks) * log(abs(Ratio_of_HitstoBlocks) + 1)) %>%
            mutate(SCFtoGoalsAgainstlog = sign(SCFtoGoalsAgainst) * log(abs(SCFtoGoalsAgainst) + 1)) %>%
            mutate(PointsPercentagesqrt = abs(PointsPercentage)^0.5) %>%
            mutate(CorsiDifftoSOSlog = sign((CF_Per60Team - CA_Per60Team)/SOS) * log(abs((CF_Per60Team - CA_Per60Team)/SOS) + 1)) %>%
            mutate(xGDifftoSOS = (xGF.60 - xGA.60)/SOS) %>% 
            mutate(GStoSOS = GS_mean / SOS) %>%
            mutate(SRStoSOS = SRS/SOS) %>%
            mutate("CF% QoT_min" = sign(allData$"CF% QoT_min") * log(abs(allData$"CF% QoT_min") + 1)) %>%
            mutate(ZSR_min = sign(ZSR_min) * log(abs(ZSR_min) + 1)) %>%
            mutate("Rel CF%_min" = sign(allData$"Rel CF%_min") * log(abs(allData$"Rel CF%_min") + 1)) %>%
            mutate("ixGF/60_max.TO.Rel CF%_max" = `Rel CF%_max` / `ixGF/60_max`) %>%
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
                                          filter(., Skew >= 1)

#.......................Define recipe.............................................#
preProcess.recipe = function(trainX){
  
  mainRecipe = recipe(ResultProper ~., data=trainX) %>%
    step_zv(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) %>%
    step_interact(terms = ~ SRS:Fenwick:ELORating) %>%
    step_interact(terms = ~ PenaltyKillPercentage:PowerPlayOppurtunities) %>%
    step_interact(terms = ~ H2H:VegasOpeningOdds) %>%
    step_interact(terms = ~ RegularSeasonWinPercentage:contains("Points")) %>%
    step_interact(terms = ~ FaceoffWinPercentage:ShotPercentage)
    
  mainRecipe
}

randomGridSearch = function(iterations, innerTrainX, innerTestX){
  
  score = 0
  alpha.fin = numeric(1)
  lambda.fin = integer(1)
  
  for(m in 1:iterations){
    
    writeLines(paste("Iteration:", m, sep = " "))
    
    alpha_val = as.numeric(runif(1, 0, 1))
    s.lambda_val = as.integer(sample(1:80, 1))
    
    modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                       label_train = innerTrainX$ResultProper, alpha.a = alpha_val, s_lambda.a = s.lambda_val)
  
    score.new = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc
    
    if(score.new > score){
      alpha.fin = alpha_val
      lambda.fin = s.lambda_val
      score = score.new
    }
  }
  list(alpha = alpha.fin, lambda = lambda.fin)
}

modelPipe.outer = function(j, folds, lambda.final, alpha.final){
 
  train.param = prep(preProcess.recipe(trainX = allData[-folds[[j]],]), training = allData[-folds[[j]],])
  train = bake(train.param, new_data = allData[-folds[[j]], ])
  test = bake(train.param, new_data = allData[folds[[j]], ])
  
  frameswithPCA = addPCA_variables(traindata = train, testdata = test)
  
  train = frameswithPCA$train
  test = frameswithPCA$test
  
  rm(train.param, frameswithPCA)
    
  model = baggedModel(train = train[, !names(train) %in% c("ResultProper")], test=test, label_train = train$ResultProper, 
                                             alpha.a = alpha.final, s_lambda.a = lambda.final)
  
  ROC = roc(response = test$ResultProper, predictor = model$Predictions, levels = c("L", "W"))$auc
  VarImp = model$VariableImportance
  
  list(ROC = ROC, VarImp = VarImp)
}

modelPipe.inner = function(k, folds){
  
  mainTrain = allData[-folds[[k]], ]
  
  innerFolds = createDataPartition(y = mainTrain$ResultProper, times = 1, p = 0.80)

    train.param = prep(preProcess.recipe(trainX = mainTrain[innerFolds[[1]],]), training = mainTrain[innerFolds[[1]], ])
    train = bake(train.param, new_data = mainTrain[innerFolds[[1]],])
    test = bake(train.param, new_data = mainTrain[-innerFolds[[1]],])
      
      frameswithPCA = addPCA_variables(traindata = train, testdata = test)
      
      train = frameswithPCA$train
      test = frameswithPCA$test
      
      rm(train.param, frameswithPCA)
        
      results = randomGridSearch(iterations = 70, innerTrainX = train, innerTestX = test)
 
  list(alpha = results$alpha, lambda = results$lambda)
  
}

processVarImp = function(varImpRaw, final = FALSE){
  
  varImpNames = varImpRaw %>% 
    select(., contains("Variable")) %>%
    .[,1]
  
  if(final == FALSE){
  
  final = varImpRaw %>% 
                        select(., contains("meanImportance")) %>%
                        transmute(Importance = rowMeans(.)) %>%
                        bind_cols(varImpNames, .)
  }else{
    
  final = varImpRaw %>% 
                        select(., contains("Importance")) %>%
                        transmute(Importance = rowMeans(.)) %>%
                        bind_cols(varImpNames, .)
  }
  
  final
}

set.seed(40689)
seeds = sample(1:1000000000, 150, replace = FALSE)

cluster = makeCluster(detectCores())
registerDoParallel(cluster)

results = foreach(p = 1:length(seeds), .combine = "c", .packages = c("tidyverse", "glmnet", "caret", "pROC", "recipes", "fastknn")) %dopar% {
  
  set.seed(seeds[p])
  allFolds = caret::createFolds(y = allData$ResultProper, k = 3)
  
  bestParam = bind_rows(lapply(1:length(allFolds), FUN = modelPipe.inner, folds = allFolds)) 
  finalResults = mapply(FUN = modelPipe.outer, j = 1:length(allFolds), lambda.final = bestParam$lambda, alpha.final = bestParam$alpha, 
                      MoreArgs = list(folds = allFolds), SIMPLIFY = FALSE)

  ROC = mean(unlist(lapply(finalResults, function(x){unlist(x$ROC)})))

  VarImp = processVarImp(varImpRaw = as_tibble(bind_cols(lapply(finalResults, function(x){(x$VarImp)}))), final = FALSE)
  
  write_csv(tibble(ROC = ROC), paste("Iteration_", p, ".csv", sep = ""))
  list(ROC = ROC, VarImp = VarImp)

}

stopCluster(cluster)
rm(cluster)

finalROC = unlist(results[c(seq(1, length(results), 2))])
finalVarImp = processVarImp(varImpRaw = results[c(seq(2, length(results),2))] %>% Reduce(bind_cols,.), final = TRUE) 

paste("Final AUROC: ", mean(finalROC), " with a 95% confidence interval given by ", "[", mean(finalROC) - qnorm(0.975)*sd(finalROC)/(length(finalROC)^0.5), ", ", 
      mean(finalROC) + qnorm(0.975)*sd(finalROC)/(length(finalROC)^0.5), "]", sep = "")


finalVarImp %>% arrange(., -Importance)
