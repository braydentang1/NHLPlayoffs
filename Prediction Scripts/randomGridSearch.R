randomGridSearch <-
function(innerTrainX, innerTestX, grid){
  
  for(m in 1:nrow(grid)){
    
    writeLines(paste("Iteration:", m, sep = " "))
    
    modelX = baggedModel(train = innerTrainX[, !names(innerTrainX) %in% c("ResultProper")], test = innerTestX, 
                         label_train = innerTrainX$ResultProper, alpha.a = as.numeric(grid[m, 1]), s_lambda.a = as.integer(grid[m,2]))
    
    #For AUROC
    #grid[m, 3] = roc(response = innerTestX$ResultProper, predictor = modelX$Predictions, levels = c("L", "W"))$auc
    
    #For LogLoss
    grid[m, 3] = logLoss(scores = modelX$Predictions, label = innerTestX$ResultProper)
    
  }
  
  grid
  
}
