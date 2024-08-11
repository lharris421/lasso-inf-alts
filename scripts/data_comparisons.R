source("./scripts/setup.R")

dat <- hdrm::readData("whoari")

cv_fit <- cv.ncvreg(dat$X, dat$y, penalty = "lasso", returnX = FALSE, returnY = FALSE)
original_estimates <- data.frame(
  estimate = coef(cv_fit$fit, lambda = cv_fit$lambda.min)[-1],
  variable = colnames(dat$X)
)

pipe_res <- pipe_ncvreg(dat$X, dat$y, alpha = 0.05, lambda = cv_fit$lambda.min)
blp_res <- blp(dat$X, dat$y, alpha = 0.05, boot.shortcut = TRUE, lambda = cv_fit$lambda.min)
posterior_res <- posterior(dat$X, dat$y, alpha = 0.05, lambda = cv_fit$lambda.min)
mcp_posterior_res <- posterior(dat$X, dat$y, penalty = "MCP", alpha = 0.05, lambda = cv_fit$lambda.min)

res_list <- list(
  pipe = pipe_res,
  blp = blp_res,
  lasso = posterior_res,
  MCP = mcp_posterior_res
)

compare_methods(res_list)
