#Set the directory for parallel computation status checks.
setwd("C:/Users/Brayden/Documents/NHLModel/Status")

#Dependencies

require(glmnet)
require(caret)
require(pROC)
require(tidyverse)
require(recipes)
require(moments)
require(doParallel)
require(foreach)
require(fastknn)

#Bagging Function
baggedModel = function(train, test, label_train, alpha.a, s_lambda.a){
  
  set.seed(40689)
  samples = caret::createResample(y = label_train, times = 15)
  pred = list()
  varImp = list()
  
  for (g in 1:length(samples)){
    train_temp = train[samples[[g]], ]
    a = label_train[samples[[g]]]
    modelX = glmnet(x = data.matrix(train_temp), y = a, family = "binomial", alpha = alpha.a, nlambda = 120, standardize = FALSE)
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
  
  out = list(Predictions = pred$Predicted, VariableImportance = varImp)
}

#LogLoss Function
logLoss = function(scores, label){
  
  if (is.factor(label)){
    u = ifelse(label ==  "W", 1,0)
  } else{
    u = label
  }
  
  tmp = data.frame(scores = scores, target = u)
  tmp = tmp %>% mutate(scores = ifelse(scores == 1, 0.9999999999999999, ifelse(scores == 0 , 0.0000000000000001, scores))) %>%
    mutate(logLoss = -(target * log(scores) + (1-target) * log(1-scores)))
  
  out = mean(tmp$logLoss)
  
}

#PCA Function

addPCA_variables = function(traindata, testdata){
    
    traindata_tmp = traindata %>% select_if(., is.numeric)
    testdata_tmp = testdata %>% select_if(., is.numeric)
    
    pca_parameters = prcomp(traindata_tmp, center = FALSE, scale. = FALSE)
    pca_newdata = predict(pca_parameters, newdata = testdata_tmp)[,1:5]
    pca_traindata = predict(pca_parameters, newdata = traindata_tmp)[,1:5]
    out = list(train = cbind(traindata, pca_traindata), test = cbind(testdata, pca_newdata))

}

#kNN Function

addKNN_variables = function(traindata, testdata, include_PCA = FALSE){
    
    y = traindata$ResultProper

    if(include_PCA == TRUE){
        
    traindata_tmp = traindata %>% select_if(., is.numeric)
    testdata_tmp = testdata %>% select_if(., is.numeric)
        
        }else{
        
    traindata_tmp = traindata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    testdata_tmp = testdata %>% select_if(., is.numeric) %>% as_tibble(.) %>% select(-starts_with("PC"))
    }
    
    newframeswithKNN = fastknn::knnExtract(xtr = data.matrix(traindata_tmp), ytr = y, xte = data.matrix(testdata_tmp), k = 1)
    KNN_train = newframeswithKNN$new.tr %>% as_tibble(.) %>% transmute_all(., .funs = function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE))
    KNN_test = newframeswithKNN$new.te %>% as_tibble(.) %>% transmute_all(., .funs = function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)) 
    out = list(train = cbind(traindata, KNN_train), test = cbind(testdata, KNN_test))
}

#Read Data In

cat("Reading in Data..... \n")
allData = read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/HockeyReference2.csv") %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/HockeyReference1.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/CorsicaAllTeamStats.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/CorsicaGameScoreStats.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/ELORatings_January25_2019.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/ESPNStats.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/FenwickScores.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/NHLOfficialStatsJanuary25th.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/SCFScores_Feb3rd_2019.csv")) %>%
              bind_cols(read_csv("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets/VegasOddsOpening.csv")) %>%
              mutate(ResultProper = as.factor(ResultProper))
  

#...................................Do Some Engineering of Features..................#
allData = allData %>% mutate(Round = as.factor(rep(c(1,1,1,1,1,1,1,1,2,2,2,2,3,3,4),12))) %>%
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
            mutate_if(is.numeric, funs(ifelse(is.nan(.), 0,.))) %>%
            mutate_if(is.numeric, funs(ifelse(is.infinite(.), 0,.)))

options(repr.matrix.max.rows=600, repr.matrix.max.cols=200, scipen = 999)

kurt = allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::kurtosis(., na.rm=TRUE))) %>%
                                         gather(., Variable, Kurtosis)
allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                         gather(., Variable, Skewness) %>%
                                         left_join(., kurt, by = "Variable")
rm(kurt)
allData %>% select_if(., is.numeric) %>% summarize_all(., funs(moments::skewness(., na.rm=TRUE))) %>%
                                          gather(., Variable, Skew) %>%
                                          filter(., Skew >= 1)

#...................................Data Splitting....................................#
set.seed(40689)
seeds = sample(1:1000000000, 150, replace = FALSE)
ROC_rep = numeric()
LogLoss_rep = numeric()

cluster = makeCluster(detectCores())
registerDoParallel(cluster)

results = foreach(p = 1:length(seeds), .packages = c("tidyverse", "caret", "recipes", "glmnet", "pROC", "fastknn"),
                  .combine = "c") %dopar% {

set.seed(seeds[p])
allFolds = caret::createFolds(y = allData$ResultProper, k = 3)

ROC = numeric()
LogLoss = numeric()
finalParam = list()
VarImp = list()

for(j in 1:length(allFolds)) {

  #..................................Generate Random Grid and Define Training Set..............................#
  cat("Generating Random Grid.....\n")
  set.seed(346002)
  randomGrid = data.frame(alpha = runif(146,0,1), lambda = sample(1:70,146, replace = TRUE))
  
  trainX = allData[-allFolds[[j]],]
  
  #...........................Create Inner Data Partition for Hyper Parameter Tuning.....#
  set.seed(40689)
  innerPartition = caret::createDataPartition(y=trainX$ResultProper, times = 1, p = 0.80)
  
  #...................................Tune Model.........................................#
  
  ROCFinal = list()
  LogLossFinal = list()
 for (k in 1:length(innerPartition)){
    
    innerTrainX = trainX[innerPartition[[k]],]
    
    #...................................Define Recipe, Do More Engineering.................#

    innermainRecipe = recipe(ResultProper ~., data=innerTrainX) %>%
      step_zv(all_numeric()) %>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_dummy(all_predictors(), -all_numeric()) %>%
      step_zv(all_predictors()) %>%
      step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) %>%
      step_interact(terms = ~ SRS:Fenwick:ELORating) %>%
      step_interact(terms = ~ RegularSeasonWinPercentage:contains("Points")) %>%
      step_interact(terms = ~ FaceoffWinPercentage:ShotPercentage) %>%
      step_interact(terms = ~ contains("Round"):VegasOpeningOdds) %>%
      step_interact(terms = ~ SDRecord:SOS) 

    innerPreProcessing = prep(innermainRecipe, training = innerTrainX)
    innerTrainX = bake(innerPreProcessing, new_data=innerTrainX)
    y = innerTrainX$ResultProper
     
    innerTestX = trainX[-innerPartition[[k]],]
    innerTestX = bake(innerPreProcessing, new_data=innerTestX)
    
    frameswithPCA = addPCA_variables(traindata = innerTrainX, testdata = innerTestX)
    innerTrainX = frameswithPCA$train
    innerTestX = frameswithPCA$test
    rm(frameswithPCA)
     
    #....................................Training Model...................................#
    ROCtemp = numeric()
    logLosstemp = numeric()

for (m in 1:nrow(randomGrid)){
        
      alpha_val = as.numeric(randomGrid[m, 1])
      s.lambda_val = as.integer(randomGrid[m,2])
    
      modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                            label_train = y, alpha.a = alpha_val, s_lambda.a = s.lambda_val)
      ROCtemp[m] = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions,
                       levels = c("L", "W"))$auc
      logLosstemp[m] = logLoss(scores = modelX$Predictions, label = innerTestX$ResultProper)
      remove(modelX, alpha_val, s.lambda_val)
    }
    ROCFinal[[k]] = ROCtemp
    LogLossFinal[[k]] = logLosstemp
    remove(innerTrainX, innermainRecipe, innerTestX, innerPreProcessing, ROCtemp, logLosstemp, m)
    gc()
  }
  
  remove(innerPartition, k)
    
#...................................Get the Best Parameters...........................#
    
  #For ROC:
  ROCFinal = ROCFinal %>% Reduce(function(x,y) cbind(x,y), .) %>% as_tibble(.) %>% mutate(., AverageROC = rowMeans(.))
  indx = which.max(ROCFinal$AverageROC)
  alpha_final = as.numeric(randomGrid[indx, 1])
  s.lambda_final = as.integer(randomGrid[indx, 2])
  remove(indx)
  finalParam[[j]] = data.frame(alpha = alpha_final, lambda = s.lambda_final)
  
  #For LogLoss:
  #LogLossFinal = LogLossFinal %>% Reduce(function(x,y) cbind(x,y), .) %>% data.table(.)
  #LogLossFinal = LogLossFinal[, .(AverageLogLoss = rowMeans(.SD)), ]
  #indx = which.min(LogLossFinal$AverageLogLoss)
  #mstop_final = as.integer(randomGrid[indx, 1])
  #nu_final = as.numeric(randomGrid[indx, 2])
  #remove(indx)
  #finalParam[[j]] = data.table(mstop = mstop_final, nu= nu_final)
  
  
  #...................................Define Recipe, Do More Engineering................#
  
   mainRecipe = recipe(ResultProper ~., data=trainX) %>%
      step_zv(all_numeric()) %>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_dummy(all_predictors(), -all_numeric()) %>%
      step_zv(all_predictors()) %>%
      step_knnimpute(neighbors = 15, all_numeric(), all_predictors()) %>%
      step_interact(terms = ~ SRS:Fenwick:ELORating) %>%
      step_interact(terms = ~ RegularSeasonWinPercentage:contains("Points")) %>%
      step_interact(terms = ~ FaceoffWinPercentage:ShotPercentage) %>%
      step_interact(terms = ~ contains("Round"):VegasOpeningOdds) %>%
      step_interact(terms = ~ SDRecord:SOS) 
  
  #.......Join Aggregations with Test Data and Pre Process Training and Test Data..................#
  
  trainXparam = prep(mainRecipe, training = trainX)
  
  trainX = bake(trainXparam, new_data=trainX)
  y = trainX$ResultProper
                                 
  testX = bake(trainXparam, new_data = testX)
                                 
  frameswithPCA = addPCA_variables(traindata = trainX, testdata = testX)
  trainX = frameswithPCA$train
  testX = frameswithPCA$test
  
  rm(frameswithPCA)

  modelX = baggedModel(train = trainX[, !names(trainX) %in% c("ResultProper")], test=testX, label_train = y, 
                       alpha.a = alpha_final, s_lambda.a = s.lambda_final)
                                 
  ROC[j] = roc(response = testX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc 
  LogLoss[j] = logLoss(scores = modelX$Predictions, label = testX$ResultProper) 
  VarImp[[j]] = modelX$VariableImportance
  remove(modelX, alpha_final, s.lambda_final, trainXparam, mainRecipe, trainX, y, ROCFinal, LogLossFinal, 
         RoundLookup)
  gc()
                            
}                                 

ROC_rep = mean(ROC)
LogLoss_rep = mean(LogLoss)

rm(ROC, LogLoss)

                                 
#............................Extract Variable Importance for Outer Fold..............................#
#Variable Importance...if we ncomp as a tuning variable this no longer has any meaning. Either select
#ncomp using the embedded feature selection or don't run this part of the code.
                                 
finalVarImp = VarImp %>% Reduce(function(x,y) cbind(x, y$meanImportance),.)
finalVarImp = finalVarImp %>% set_names(., c("Variable", seq(1, length(allFolds),1))) %>%
                              as_tibble(.) %>%
                              mutate(AverageImp = rowMeans(select(., -Variable))) %>%
                              select(., c(Variable, AverageImp)) %>%
                              arrange(desc(AverageImp)) %>%
                              mutate(RelativeImportance = round(AverageImp/sum(AverageImp), 5))                           
remove(VarImp)

VarImp_rep = finalVarImp
remove(finalVarImp)

write.table(ROC_rep, file = paste("Iteration_", p, ".txt", sep=""), row.names = FALSE)
list(ROC_rep, LogLoss_rep, VarImp_rep)  
                      
}
stopCluster(cluster)
ROC_rep = results[seq(1,length(results), 3)] %>% unlist(.)
LogLoss_rep = results[seq(2, length(results), 3)] %>% unlist(.)
VarImp = results[seq(3, length(results),3)]
 
writeLines(paste("Final AUROC:", mean(ROC_rep), sep = " "))
writeLines(paste("Final Log Loss:", mean(LogLoss_rep), sep = " "))

writeLines(paste("A 95% CI for the AUROC is:", "[", mean(ROC_rep) - 1.96 * (sd(ROC_rep))/(length(ROC_rep)^0.5), ", ",
                mean(ROC_rep) + 1.96 * (sd(ROC_rep))/(length(ROC_rep)^0.5), "]", sep = ""))
writeLines(paste("A 95% CI for the Log Loss is:", "[", mean(LogLoss_rep) - 1.96 * (sd(LogLoss_rep))/(length(LogLoss_rep)^0.5), ", ",
                mean(LogLoss_rep) + 1.96 * (sd(LogLoss_rep))/(length(LogLoss_rep)^0.5), "]", sep = ""))
rm(cluster)

#Extract Variable Importances
VarImp_final = VarImp %>% Reduce(function(...) left_join(..., by="Variable", all.x=TRUE), .)
indx = colnames(VarImp_final[, grepl(paste(c("Variable", "RelativeImportance"), collapse = "|"), colnames(VarImp_final))])
VarImp_final = VarImp_final[, names(VarImp_final) %in% indx]
rm(indx)
                                 
options(repr.matrix.max.rows=600, repr.matrix.max.cols=200, scipen = 999)
Variable = as.data.frame(VarImp_final$Variable) %>% set_names("Variable")
VarImp_final = VarImp_final[,2:ncol(VarImp_final)] %>% mutate(AverageRelativeImportance = rowMeans(.)) %>%
                                                       select(., c(AverageRelativeImportance)) %>%
                                                       bind_cols(Variable,.) %>%
                                                       arrange(desc(AverageRelativeImportance)) 
VarImp_final
