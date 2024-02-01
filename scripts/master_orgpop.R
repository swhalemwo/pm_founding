## * master script for org_pop

PROJ_DIR <- getwd()

## define commands
cmd_reg            <- sprintf("Rscript regression.R %s"    , PROJ_DIR)
cmd_postestimation <- sprintf("Rscript postestimation.R %s", PROJ_DIR)
cmd_anls           <- sprintf("Rscript reg_anls.R %s"      , PROJ_DIR)

## run commands
system(cmd_reg)
system(cmd_postestimation)
system(cmd_anls)
       
