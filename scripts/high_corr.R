devtools::load_all()

library(ncvreg)
library(hdi)
library(dplyr)
library(ggplot2)

# truth <- data.frame(variable = names(data$beta), truth = data$beta)

res <-  list()
for (i in 1:100) {
  data <- hdrm::gen_data_abn()
  cv_fit <- cv.ncvreg(data$X, data$y, penalty = "lasso")
  cv_fit_mcp <- cv.ncvreg(data$X, data$y, penalty = "MCP")
  pipe_res <- pipe_ncvreg(data$X, data$y, alpha = 0.05, lambda = cv_fit$lambda.min) %>%
    mutate(method = "pipe")
  blp_res <- lp(data$X, data$y, alpha = 0.05, lambda = cv_fit$lambda.min) %>%
    mutate(method = "blp")
  posterior_res <- posterior(data$X, data$y, alpha = 0.05, lambda = cv_fit$lambda.min) %>%
    mutate(method = "lasso")
  mcp_posterior_res <- posterior(data$X, data$y, penalty = "MCP", alpha = 0.05, lambda = cv_fit_mcp$lambda.min) %>%
    mutate(method = "mcp")
  res[[i]] <- bind_rows(pipe_res, blp_res, posterior_res, mcp_posterior_res)
}

truth <- data.frame(variable = names(data$beta), truth = data$beta) %>%
  rowwise() %>%
  mutate(type = stringr::str_split(variable, "")[[1]][1])

compare_methods(list(
  pipe = pipe_res,
  blp = blp_res,
  lasso = posterior_res,
  MCP = mcp_posterior_res
))

res <- do.call(bind_rows, res)

res %>%
  left_join(truth) %>%
  mutate(covered = lower <= truth & truth <= upper) %>%
  group_by(method, type) %>%
  summarise(mean_coverage = mean(covered))
