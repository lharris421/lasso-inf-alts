source("./scripts/setup.R")

## A script version set up of the simulation
## this differs mildly in behavior, allowing a more granular handling in lambda


params <- list(seed = 1234, iterations = 1000,
               simulation_function = "gen_data_snr", simulation_arguments = list(
                 n = 100, p = 100,
                 beta = c(-2, 2, -1, 1, -0.5, 0.5, -0.5, 0.5, rep(0, 92)),
                 family="hetero"
               ), script_name = "sim_ortho_lam")

res <- list()

pb <- txtProgressBar(min = 0, max = params$iterations, initial = 0, style = 2) 

set.seed(params$seed)
seeds <- round(runif(params$iterations) * 1e9)

for(i in 1:params$iterations) {
  
  set.seed(seeds[i])
  data <- do.call(params$simulation_function, params$simulation_arguments)
  
  lasso_lambda <- cv.ncvreg(data$X, data$y, penalty = "lasso")$lambda.min
  mcp_lambda <- cv.ncvreg(data$X, data$y, penalty = "MCP")$lambda.min
  
  res[[i]] <- list()
  for (j in 1:length(methods)) {
    
    lambda <- ifelse(names(methods)[j] == "mcp", mcp_lambda, lasso_lambda)
    
    time_taken <- system.time({
      results <- do.call(methods[[j]]$method, c(list(X = data$X, y = data$y, lambda = lambda), methods[[j]]$method_arguments)) 
    })
    
    res[[i]][[names(methods)[j]]] <- results %>%
      mutate(truth = data$beta, iteration = i, method = names(methods)[j], time = time_taken["elapsed"]) 
    
  }
  
  setTxtProgressBar(pb,i)
  
}

method_names <- names(methods)
for (i in 1:length(methods)) {
  
  tmp_results <- extract_named_elements(res, method_names[i])
  tmp_results <- do.call(rbind, tmp_results)
  results <- list("results" = tmp_results)
  
  ## Clean up the call argument
  method_info <- methods[[method_names[i]]]
  params[names(method_info)] <- method_info
  
  indexr::save_objects("./rds", results, args_list = params, overwrite = TRUE) 
  
}
