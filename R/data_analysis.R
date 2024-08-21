data_analysis <- function(method, method_arguments = list(), alpha = 0.05, data) {
  
  original_call <- match.call()

  results <- do.call(method, c(list(X = data$X, y = data$y, alpha = alpha), method_arguments)) 
  
  return(list("call" = original_call, "results" = results))
  
}