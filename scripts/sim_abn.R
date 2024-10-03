## Setup
source("./scripts/setup.R")

# Ran w/o correlations between noise variables
# Ran w/ ar correlation = 0.5 between nopise variables

## Set up for all simulations
libs <- c("ncvreg", "hdi", "dplyr", "hdrm")
evalFunc <- function() {
  devtools::load_all()
}
exports <- c("posterior", "pipe_ncvreg", "lp", "ci_full_cond", "soft_threshold", "firm_threshold_c")

simulation_info <- list(
  simulation_function = "gen_data_abn",
  simulation_arguments = list(
    n = 100, p = 100, a = 8, b = 2, rho = 0.5,
    beta = c(-2, 2, -1, 1, -0.5, 0.5, -0.5, 0.5, rep(0, 92))
  )
)

# simulation_info <- list(
#   simulation_function = "gen_data_abn",
#   simulation_arguments = list(
#     n = 1000, p = 30, a = 8, b = 1, rho = 0.5,
#     beta = c(-2, 2, -1, 1, -0.5, 0.5, -0.5, 0.5, rep(0, 14))
#   )
# )

# simulation_info <- list(
#   simulation_function = "gen_data_abn",
#   simulation_arguments = list(
#     n = 200, p = 40, a = 8, b = 1, rho = 0.5,
#     beta = c(-2, 2, -1, 1, -0.5, 0.5, -0.5, 0.5, rep(0, 32))
#   )
# )

# simulation_info <- list(
#   simulation_function = "gen_data_abn",
#   simulation_arguments = list(
#     n = 500, p = 20, a = 4, b = 1, rho = 0.5,
#     beta = c(2, -1, 0.5, -0.5, rep(0, 12))
#   )
# )

run_sim(methods, simulation_info, parallel = TRUE, nClust = 5, libsEx = libs,
        clusterEv = evalFunc, clusterEx = exports)



