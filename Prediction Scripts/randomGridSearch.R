randomGridSearch <-
function(iterations, innerTrainX, innerTestX, seed.a){
  
  score = 0
  alpha.fin = numeric(1)
  lambda.fin = integer(1)
  
  set.seed(seed.a)
  alpha_val = as.numeric(runif(n = iterations, min = 0, max = 1))
  s.lambda_val = as.integer(sample(1:90, iterations, replace = TRUE))
  
  for(m in 1:iterations){
    
    writeLines(paste("Iteration:", m, sep = " "))
    
    modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                         label_train = innerTrainX$ResultProper, alpha.a = alpha_val[m], s_lambda.a = s.lambda_val[m])
    
    score.new = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc
    
    if(score.new > score){
      alpha.fin = alpha_val[m]
      lambda.fin = s.lambda_val[m]
      varimp.fin = modelX$VariableImportance
      score = score.new
    }
  }
  list(alpha = alpha.fin, lambda = lambda.fin, VarImp = varimp.fin)
}
