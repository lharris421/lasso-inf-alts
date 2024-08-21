sim <- function(method, method_arguments = list(), alpha = 0.05,
                simulation_function, simulation_arguments = list(), iterations = 1000) {
  
  original_call <- match.call()
  
  if(!exists(".Random.seed")) set.seed(NULL)
  starting_seed <- .Random.seed
  
  pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  
  ret <- list()
  for (i in 1:iterations) {
    
    data <- do.call(simulation_function, simulation_arguments)
    results <- do.call(method, c(list(X = data$X, y = data$y, alpha = alpha), method_arguments)) 
    ret[[i]] <- results %>%
      mutate(truth = data$beta, iteration = i) 
    
    setTxtProgressBar(pb, i)
  
  }
  
  ret <- do.call(rbind, ret)
  ret <- list("call" = original_call, "results" = ret, "seed" = starting_seed)
  
  close(pb)
  return(ret)
  
}

