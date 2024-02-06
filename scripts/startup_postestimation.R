args <- commandArgs(trailingOnly = T)
options(width = 115)
library(data.table)
library(collapse)
library(purrr)
library(parallel) # parallel
library(docstring) # documentation
library(DBI) # databases
library(xtable) # table generation
library(Hmisc, include.only = "latexTranslate") # table generation?, maybe yeet?
library(modelsummary) # analysis? maybe can be yeeted? but maybe postestimation -> TESTME
library(furrr) # parallel processing, needed for VIF
library(performance) # probably postestimation (VIF)
library(tidyr) # some postestimation still uses pivot_wider
library(glmmTMB) # for counterfactual/development? 
## library(DescTools, include.only = "Gini") # for postestimation
library(dineq, include.only = "gini.wtd")
library(stringr)

select <- dplyr::select
lag <- dplyr::lag
filter <- dplyr::filter



source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "reg_funcs.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))

walk(c(OBJS_TO_RDS_WRNGL), # OBJS_TO_RDS_REG
     ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))

## fldr_info_optmz <- readRDS(paste0(REG_MONKEY_DIR, reg_settings_optmz$batch_version, "/fldr_info_optmz.rds"))

fldr_info_optmz <- setup_regression_folders_and_files(REG_MONKEY_DIR, reg_settings_optmz$batch_version)





