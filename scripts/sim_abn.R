## Setup
rm(list = ls())
devtools::load_all()
library(ncvreg)
library(dplyr)
library(hdrm)
library(parallel)
library(hdi)


## Parallel
methods <- list(
  "lasso" = list(method = "posterior", method_arguments = list(penalty = "lasso")),
  "mcp"   = list(method = "posterior", method_arguments = list(penalty = "MCP")),
  "pipe"  = list(method = "pipe_ncvreg", method_arguments = list()),
  "lp"    = list(method = "lp", method_arguments = list())
)

simulation_info <- list(
  simulation_function = "gen_data_abn", 
  simulation_arguments = list(
    n = 100, p = 100, a = 10, b = 2, rho = 0.5,
    SNR = 1, signal = "homogeneous",
    noise="exchangeable", rho.noise = 0
  )
)

cl <- parallel::makeCluster(4)
clusterExport(cl, c("sim", "posterior", "pipe_ncvreg", "lp", "gen_data_abn", "ci_full_cond", "soft_threshold", "firm_threshold_c"))
clusterEvalQ(cl, {
  devtools::load_all()
  library(ncvreg)
  library(hdi) 
  library(dplyr)
  library(hdrm)
})
parallel::parLapply(cl, methods, run_sim, simulation_info)
parallel::stopCluster(cl)

