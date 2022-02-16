## ** startup
## *** libraries
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)
library(lme4)
library(texreg)
library(ggplot2)
library(countrycode)
library(stargazer)
library(gridExtra)
library(parallel)
library(RClickhouse)
library(docstring)
library(DBI)
library(TTR)
library(ggpubr)
library(xtable)
library(OECD)
library(rsdmx)
library(data.table)
library(wbstats)
ds <- docstring


options(show.error.messages = TRUE)
options(show.error.locations = TRUE)



## *** set static vars

PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"

PMDB_DIR <- paste0(PROJECT_DIR, "data/pmdb/") # DIR for private museum database (currently excel import)
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")
FIG_DIR <- paste0(PROJECT_DIR, "figures/")
TABLE_DIR <- paste0(PROJECT_DIR, "tables/")
WID_DIR_v1 = paste0(PROJECT_DIR, "data/wid/wid_world_db/version1_oct21/")
WID_DIR_v2 = paste0(PROJECT_DIR, "data/wid/wid_world_db/version2_feb22/")


PROC_DATA_DIR <- paste0(PROJECT_DIR, "data/processed/")

STARTING_YEAR <- 1985
ENDING_YEAR <- 2020



con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")
