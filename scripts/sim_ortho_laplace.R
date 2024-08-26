source("./scripts/setup.R")

params <- list(n = 100, p = 60, SNR = 1, seed = 1234, method = "posterior", 
               method_arguments = list(penalty = "lasso"), iterations = 10)

res <- list()
for(i in 1:params$iterations) {
  
  true_rate <- sqrt(2*params$p)
  true_lambda <- true_rate / params$n
  beta <- rlaplace(params$p, rate = true_rate)
  data <- gen_ortho_snr(params$n, params$p, beta = beta)
  
  res[[i]] <- do.call(params$method, c(list(X = data$X, y = data$y, lambda = true_lambda, sigma2 = 1), params$method_arguments)) %>%
    mutate(truth = data$beta)
  
  if (i %% 100 == 0) print(i)
  
}

res <- do.call(rbind, res)

indexr::save_objects("./rds", res, args_list = params)
