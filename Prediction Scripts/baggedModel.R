baggedModel <-
function(train, test, label_train, alpha.a, s_lambda.a, calibrate = FALSE){
  
  set.seed(40689)
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
