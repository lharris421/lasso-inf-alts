sim_example <- function(method, method_arguments = list(), alpha = 0.05,
                simulation_function, simulation_arguments = list(), seed = 1234) {
  
  original_call <- match.call()
    
  set.seed(seed)
  data <- do.call(simulation_function, simulation_arguments)
  results <- do.call(method, c(list(X = data$X, y = data$y, alpha = alpha), method_arguments)) %>%
    mutate(truth = data$beta) 
    
  return(list("call" = original_call, "results" = results))
  
}
