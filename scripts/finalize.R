## * finalize everything

## get variables: need to source startup_static to get access to REG_MONKEY_DIR and batch_version
library(collapse) # needed for parsing startup_static

print("start finalizing")
args <- commandArgs(trailingOnly = T)
PROJECT_DIR <- args[1]

SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")
source(paste0(SCRIPT_DIR, "startup_static.R"))

## * finalize a run
## ** zip the results
cmd_zip <- sprintf("cd %s && zip -r res%s.zip %s", REG_MONKEY_DIR,
                   reg_settings_optmz$batch_version, reg_settings_optmz$batch_version)

print("files zipped")

## ** notification

cmd_notify <- sprintf("mutt -s \"run %s has finished\" jojo.ae@hotmail.de < /dev/null",
                      reg_settings_optmz$batch_version)
system(cmd_notify)
print("run finished, notification sent")
