# Clear enviornment
rm(list=ls())

devtools::load_all()

# Load packages
library(ncvreg)
library(ggplot2)
library(magrittr)
library(dplyr)
library(hdi)
library(hdrm)
library(parallel)

methods <- list(
  "hybrid" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "hybrid")),
  "debiased" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "debiased")),
  "posterior" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "posterior")),
  "lasso" = list(method = "posterior", method_arguments = list(penalty = "lasso")),
  "pipe"  = list(method = "pipe_ncvreg", method_arguments = list()),
  "mcp"   = list(method = "posterior", method_arguments = list(penalty = "MCP"))
  # "lp"    = list(method = "lp", method_arguments = list(original = TRUE))
)
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- 0.05
}
