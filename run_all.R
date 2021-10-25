# run all scripts to fit models and produce figures and tables

# load packages

library(rethinking)
library(rstan)
library(xtable)

# set model parameters

seed <- 3873
iter <- 8e3
chains <- 8
control <- list(adapt_delta = 0.99, max_treedepth = 15)

# load data objects

load("data.Rdata")

# summarize data 

source("R/summarize_data.R")

# fit models

source("R/fit_gap.R")
source("R/fit_fert.R")
source("R/fit_surv.R")
source("R/fit_sad.R")

# process model fits

source("R/plot_gap.R")
source("R/plot_fert.R")
source("R/plot_surv.R")
source("R/plot_sad.R")

# expect warnings:
# In class(x) <- NULL : Setting class(x) to NULL;   result will no longer be an S4 object
# this is from the xtable function writing the parameter summary tabs to txt
# and we can ignore this
# suppress warnings to source the script
# you can run the script line by line with warnings enabled
