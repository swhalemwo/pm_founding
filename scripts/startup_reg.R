args <- commandArgs(trailingOnly = T)
options(width = 115)

library(data.table)
library(dplyr)
library(purrr)
library(glmmTMB) # regression
library(ggplot2)
library(parallel) # parallel
library(docstring) # documentation
library(DBI) # databases
library(rsdmx) # saving model results
library(collapse) # data processing
library(modelsummary) # needed, probably for GOF? 
library(RSQLite) # caching



## make sure all kinds of basic functions aren't masked
select <- dplyr::select
lag <- dplyr::lag
filter <- dplyr::filter


source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "gen_cbn_df_dict.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))

## read back objects from wrangling stage
walk(OBJS_TO_RDS_WRNGL, ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))


     
