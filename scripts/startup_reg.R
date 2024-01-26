args <- commandArgs(trailingOnly = T)
options(width = 115)

library(data.table)
library(dplyr)
library(purrr)
library(furrr)
library(performance)
library(glmmTMB)
library(ggplot2)
library(parallel)
library(docstring)
library(DBI)
library(xtable)
library(rsdmx)
library(Hmisc, include.only = "latexTranslate")
library(collapse)


source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "gen_cbn_df_dict.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))

walk(OBJS_TO_RDS, ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))


     
