## Setup
source("./scripts/setup.R")

simulation_info <- list(
  simulation_function = "gen_ortho",
  simulation_arguments = list(
    n = 100, p = 60, p1 = 10,
    SNR = 1, signal = "homogeneous"
  )
)

libs <- c("ncvreg", "hdi", "dplyr", "hdrm")
evalFunc <- function() {
  devtools::load_all()
}
exports <- c("posterior", "pipe_ncvreg", "lp", "ci_full_cond", "soft_threshold", "firm_threshold_c")

run_sim(methods, simulation_info, parallel = TRUE, nClust = 4, libsEx = libs,
        clusterEv = evalFunc, clusterEx = exports)

# run_sim(methods, simulation_info, parallel = FALSE)

