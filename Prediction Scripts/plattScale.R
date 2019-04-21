platt.scale <-
function(label.train, predicted.prob.train, predicted.prob.test){
  
  model = glm(formula = label.train ~ ., data = predicted.prob.train, family = binomial(link = "logit"))
  
  scaled.prob = predict(model, newdata = predicted.prob.test, type = c("response"))
  
  scaled.prob
  
}
