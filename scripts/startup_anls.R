library(data.table)
options(width = 115)
library(data.table)
library(collapse)
library(purrr)      
library(furrr)       # rendering of figures
library(parallel)    # parallel
library(docstring)   # documentation
library(xtable)      # table generation
library(Hmisc,       # table generation
        include.only = "latexTranslate") 
library(tidyr)       # pivot_longer/wider
library(stringr)     # string processing
library(ggbeeswarm)  # visualization
library(patchwork)   # visualization
library(ggridges)    # visualization
library(ggrepel)     # visualization
library(DBI)         # connection to mdl caches i guess
library(glmmTMB)     # running predicted figures
library(texreg,      # table generation
        include.only = "coeftostring")
library(countrycode) # summary table

## library(sf)
## library(stars)

library(scales, include.only = "trans_format")

source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "reg_funcs.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))



select <- dplyr::select
lag <- dplyr::lag
filter <- dplyr::filter

walk(c(OBJS_TO_RDS_WRNGL), # OBJS_TO_RDS_REG),
     ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))

## fldr_info_optmz <- readRDS(paste0(REG_MONKEY_DIR, reg_settings_optmz$batch_version, "/fldr_info_optmz.rds"))

fldr_info_optmz <- setup_regression_folders_and_files(REG_MONKEY_DIR, reg_settings_optmz$batch_version)

