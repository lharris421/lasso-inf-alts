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
  "lasso" = list(method = "posterior", method_arguments = list(penalty = "lasso")),
  "mcp"   = list(method = "posterior", method_arguments = list(penalty = "MCP")),
  "pipe"  = list(method = "pipe_ncvreg", method_arguments = list()),
  "lp"    = list(method = "lp", method_arguments = list())
)
