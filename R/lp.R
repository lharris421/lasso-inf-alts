lp <- function(X, y, alpha = 0.2, lambda = NULL) {
  
  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(X, y)
    lambda <- cv_fit$lambda.min
  }
  
  tryCatch({
    fit.lasso.allinfo <- lasso.proj(
      X, y,
      lambda = lambda
    )
    ci_hdi <- confint(fit.lasso.allinfo, level = 1 - alpha)
    
    ci <- ci_hdi %>%
      data.frame(variable = rownames(ci_hdi)) %>%
      mutate(estimate = fit.lasso.allinfo$bhat) %>%
      select(variable, estimate, lower, upper) %>%
      mutate(lambda = lambda)
    
    res <- ci
    return(res)
  }, error = function(e) {
    print(e);
    res <- NULL
    return(res)
  })
}