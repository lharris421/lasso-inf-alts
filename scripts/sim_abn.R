## Setup
source("./scripts/setup.R")

simulation_info <- list(
  simulation_function = "gen_data_abn",
  simulation_arguments = list(
    n = 100, p = 100, a = 10, b = 2, rho = 0.5,
    SNR = 1, signal = "homogeneous",
    noise="exchangeable", rho.noise = 0
  )
)


libs <- c("ncvreg", "hdi", "dplyr", "hdrm")
evalFunc <- function() {
  devtools::load_all()
}

run_sim(methods, simulation_info)

