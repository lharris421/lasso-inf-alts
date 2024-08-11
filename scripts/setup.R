# Clear enviornment
rm(list=ls())

devtools::load_all()

# Load packages
library(ncvreg)
library(ggplot2)
library(magrittr)
library(dplyr)
library(hdi)


my_seed <- 189807771
set.seed(my_seed)
