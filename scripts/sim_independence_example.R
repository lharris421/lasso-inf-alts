## Setup
source("./scripts/setup.R")

# simulation_info <- list(
#   simulation_function = "gen_data", 
#   simulation_arguments = list(
#     n = 100, p = 60, p1 = 10, SNR = 1, signal = "homogeneous",
#     corr="exchangeable", rho = 0
#   )
# )

# simulation_info <- list(
#   simulation_function = "gen_data_abn", 
#   simulation_arguments = list(
#     n = 100, p = 100, a = 10, b = 2, rho = 0.5,
#     SNR = 1, signal = "homogeneous",
#     noise="exchangeable", rho.noise = 0
#   )
# )

simulation_info <- list(
  simulation_function = "gen_ortho", 
  simulation_arguments = list(
    n = 100, p = 60, p1 = 10,
    SNR = 1, signal = "homogeneous"
  )
)

cl <- parallel::makeCluster(4)
clusterExport(cl, c("sim_example", "posterior", "pipe_ncvreg", "lp", "gen_data", "ci_full_cond", "soft_threshold", "firm_threshold_c"))
clusterEvalQ(cl, {
  devtools::load_all()
  library(ncvreg)
  library(hdi) 
  library(dplyr)
  library(hdrm)
})
parallel::parLapply(cl, methods, run_sim_example, simulation_info)
parallel::stopCluster(cl)
