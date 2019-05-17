modelPipe.inner <-
function(folds, seed.a, iterations){
  
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
