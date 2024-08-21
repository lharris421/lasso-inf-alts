source("./scripts/setup.R")

p <- 100
n <- 100

true_rate <- sqrt(2*p)
true_lambda <- true_rate / n
penalty <- "MCP"

coverages <- numeric(100)
for (i in 1:100) {
  laplace_beta <- rlaplace(p, rate = 1)
  dat <- gen_data_snr(n = n, p = p, p1 = p, beta = laplace_beta, rho = 0)
  
  cv_fit <- cv.ncvreg(dat$X, dat$y, penalty = penalty)
  
  coverages[i] <- 
    posterior(X = dat$X, y = dat$y, lambda = cv_fit$lambda.min) %>%
    # posterior(X = dat$X, y = dat$y, lambda = true_lambda, sigma2 = 1) %>%
    mutate(truth = dat$beta, covered = lower <= truth & truth <= upper) %>%
    pull(covered) %>%
    mean()
  
  print(coverages[i])
  
}
mean(coverages)


