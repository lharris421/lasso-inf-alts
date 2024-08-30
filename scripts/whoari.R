source("./scripts/setup.R")

params <- list(script = "whoari", alpha = 0.05)

dat <- hdrm::readData("whoari")

cv_fit <- cv.ncvreg(dat$X, dat$y, penalty = "lasso")
cv_fit_mcp <- cv.ncvreg(dat$X, dat$y, penalty = "MCP")

pipe_res <- pipe_ncvreg(dat$X, dat$y, alpha = 0.05, lambda = cv_fit$lambda.min)
blp_res <- blp(dat$X, dat$y, alpha = 0.05, boot.shortcut = TRUE, lambda = cv_fit$lambda.min)
posterior_res <- posterior(dat$X, dat$y, alpha = 0.05, lambda = cv_fit$lambda.min)
mcp_posterior_res <- posterior(dat$X, dat$y, penalty = "MCP", alpha = 0.05, lambda = cv_fit_mcp$lambda.min)

indexr::save_objects("./rds", pipe_res, args_list = c(params, method = "pipe_ncvreg"))
indexr::save_objects("./rds", blp_res, args_list = c(params, method = "lp"))
indexr::save_objects("./rds", posterior_res, args_list = c(params, method = "posterior", method_arguments = list(penalty = "lasso")))
indexr::save_objects("./rds", mcp_posterior_res, args_list = c(params, method = "posterior", method_arguments = list(penalty = "MCP")))
