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


PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")


source(paste0(SCRIPT_DIR, "startup_static.R"))

walk(OBJS_TO_RDS, ~assign(.x, value = readRDS(file = paste0(RDS_DIR, .x, ".rds")), envir = .GlobalEnv))


     
