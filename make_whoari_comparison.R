library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "ncvreg", "dplyr", "ggplot2", "hdi"), # Packages that your targets need for their tasks.
  # format = "rds", # Optionally set the default storage format. qs is fast.
  # controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = hdrm::readData("whoari")
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name = cv_fit,
    command = cv.ncvreg(data$X, data$y, penalty = "lasso")
  ),
  tar_target(
    name = pipe_res,
    command = pipe_ncvreg(data$X, data$y, alpha = 0.05, lambda = cv_fit$lambda.min)
  ),
  tar_target(
    name = blp_res,
    command = blp(data$X, data$y, alpha = 0.05, boot.shortcut = TRUE, lambda = cv_fit$lambda.min)
  ),
  tar_target(
    name = posterior_res,
    command = posterior(data$X, data$y, alpha = 0.05, lambda = cv_fit$lambda.min)
  ),
  tar_target(
    name = mcp_posterior_res,
    command = posterior(data$X, data$y, penalty = "MCP", alpha = 0.05, lambda = cv_fit$lambda.min)
  ),
  tar_target(
    name = comparison_plot,
    command = compare_methods(list(
      pipe = pipe_res,
      blp = blp_res,
      lasso = posterior_res,
      MCP = mcp_posterior_res
    ))
  )
)
