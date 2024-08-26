## Setup
rm(list = ls())
devtools::load_all()
library(ncvreg)
library(dplyr)
library(hdrm)
library(parallel)
library(hdi)


## Simple
simulation_results <- sim(
  posterior, method_arguments = list(penalty = "lasso"),
  simulation_function = gen_data,
  simulation_arguments = list(
    n = 100, p = 60, p1 = 10, SNR = 1, signal = "homogeneous",
    corr="exchangeable", rho = 0
  )
)
indexr::save_objects("./rds", simulation_results)

# Read back in
# simulation_arguments <- list(
#   function_name = "sim",
#   method = "posterior", method_arguments = list(penalty = "lasso"),
#   simulation_function = "gen_data",
#   simulation_arguments = list(
#     n = 100, p = 60, p1 = 10, SNR = 1, signal = "homogeneous",
#     corr="exchangeable", rho = 0
#   )
# )
# indexr::read_objects("./rds", simulation_arguments)

## Troubleshooting
# simulation_results <- sim(
#   iterations = 1,
#   lp, method_arguments = list(),
#   simulation_function = gen_data,
#   simulation_arguments = list(
#     n = 100, p = 60, p1 = 10, SNR = 1, signal = "homogeneous",
#     corr="exchangeable", rho = 0
#   )
# )
# indexr::save_objects("./rds", simulation_results)


## Parallel
methods <- list(
  "lasso" = list(method = "posterior", method_arguments = list(penalty = "lasso")),
  "mcp"   = list(method = "posterior", method_arguments = list(penalty = "MCP")),
  "pipe"  = list(method = "pipe_ncvreg", method_arguments = list()),
  "lp"    = list(method = "lp", method_arguments = list())
)

simulation_info <- list(
  simulation_function = "gen_data", 
  simulation_arguments = list(
    n = 100, p = 60, p1 = 10, SNR = 1, signal = "homogeneous",
    corr="exchangeable", rho = 0
  )
)


run_sim <- function(method_info, simulation_info) {
  
  # Combine the method information and simulation information
  simulation_arguments <- c(
    method_info, simulation_info
  )
  
  # Run the simulation
  simulation_results <- do.call(sim, simulation_arguments)
  simulation_results$call[1] <- "sim" ## Need more elegant solution
  
  # Save the results
  indexr::save_objects("./rds", simulation_results, overwrite = TRUE)
}

cl <- parallel::makeCluster(4)
clusterExport(cl, c("sim", "posterior", "pipe_ncvreg", "lp", "gen_data", "ci_full_cond", "soft_threshold", "firm_threshold_c"))
clusterEvalQ(cl, {
  devtools::load_all()
  library(ncvreg)
  library(hdi) 
  library(dplyr)
  library(hdrm)
})
parallel::parLapply(cl, methods, run_sim, simulation_info)
parallel::stopCluster(cl)

## Load data back in
indexr::read_objects("./rds", c(function_name = "sim", methods[["pipe"]], simulation_info))$results %>%
  mutate(abs_truth = abs(truth), covered = lower <= truth & truth <= upper) %>%
  group_by(abs_truth) %>%
  summarise(coverage = mean(covered))
