library(data.table)
options(width = 115)
library(data.table)
library(collapse)
library(purrr)
library(furrr)
library(parallel) # parallel
library(docstring) # documentation
library(xtable) # table generation
library(Hmisc, include.only = "latexTranslate")
library(tidyr)
library(stringr)
library(stringr)
library(ggbeeswarm)
library(patchwork)
library(ggridges)

## library(sf)
## library(stars)

library(scales, include.only = "trans_format")

source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R"))

select <- dplyr::select
lag <- dplyr::lag
filter <- dplyr::filter

walk(OBJS_TO_RDS_WRNGL, ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))
