args <- commandArgs(trailingOnly = T)
options(width = 115)
library(data.table)
library(parallel) # parallel
library(docstring) # documentation
library(DBI) # databases
library(xtable) # table generation
library(Hmisc, include.only = "latexTranslate") # table generation?, maybe yeet?
library(modelsummary) # analysis? maybe can be yeeted? but maybe postestimation -> TESTME
## library(furrr) # parallel processing, probably needed at least for reg_anls
library(performance) # probably postestimation (VIF)
library(tidyr) # some postestimation still uses pivot_wider


source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))

walk(OBJS_TO_RDS_REG, ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))



