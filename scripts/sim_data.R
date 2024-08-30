## Setup
source("./scripts/setup.R")

## Set up for all simulations
libs <- c("ncvreg", "hdi", "dplyr", "hdrm")
evalFunc <- function() {
  devtools::load_all()
}
exports <- c("posterior", "pipe_ncvreg", "lp", "ci_full_cond", "soft_threshold", "firm_threshold_c")

simulation_info <- list(
  simulation_function = "gen_data",
  simulation_arguments = list(
    n = 100, p = 100,
    beta = c(-2, 2, -1, 1, -0.5, 0.5, -0.5, 0.5, rep(0, 92)),
    corr = "autoregressive", rho = 0.5
  )
)

methods <- methods[c("pipe", "lasso", "mcp")]
run_sim(methods, simulation_info, parallel = TRUE, nClust = 4, libsEx = libs,
        clusterEv = evalFunc, clusterEx = exports)

