## * master script for org_pop

## hacky way to get PROJ DIR
PROJECT_DIR <- gsub("org_pop/scripts", "org_pop/", x = getwd())

library(collapse) # needed for reading startup_static


SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_static.R"))

LOG_DIR <- paste0(REG_MONKEY_DIR, reg_settings_optmz$batch_version, "/logs/")
if (!dir.exists(LOG_DIR)) {dir.create(LOG_DIR, recursive = T)}

## define commands
cmd_reg            <- sprintf("Rscript regression.R %s > %slog_reg.txt 2>&1"     , PROJECT_DIR, LOG_DIR)
cmd_postestimation <- sprintf("Rscript postestimation.R %s > %slog_post.txt 2>&1", PROJECT_DIR, LOG_DIR)
cmd_anls           <- sprintf("Rscript reg_anls.R %s > %slog_anls.txt 2>&1"      , PROJECT_DIR, LOG_DIR)
cmd_finalize       <- sprintf("Rscript finalize.R %s > %slog_finalize.txt 2>&1"  , PROJECT_DIR, LOG_DIR)

## run commands
## status_reg   <- system(cmd_reg)
## print(cmd_reg)
## if (status_reg != 0) {stop("error in cmd_reg")}

## status_post  <- system(cmd_postestimation)
## if (status_post != 0) {stop("error in cmd_postestimation")}

status_anls  <- system(cmd_anls)
if (status_anls != 0) {stop("error in cmd_anls")}

status_final <- system(cmd_finalize)
if (status_final != 0) {stop("error in cmd_final")}
       
