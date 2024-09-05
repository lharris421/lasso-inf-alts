boot_ncv <- function(X, y, cv_fit, lambda, sigma2, alpha = 0.05, penalty = "lasso", submethod = "hybrid") {
  
  res <- boot_ncvreg(X, y, cv_fit, penalty = penalty, lambda = lambda, sigma2 = sigma2)
  cis <- ci.boot_ncvreg(res, alpha = alpha, methods = submethod)
  
  cis %>%
    mutate(lambda = res$lambda, estimate = res$estimates)
  
}