## * master script for org_pop

## hacky way to get PROJ DIR
PROJ_DIR <- getwd() %>% gsub("org_pop/scripts", "org_pop/", .)


## define commands
cmd_reg            <- sprintf("Rscript regression.R %s"    , PROJ_DIR)
cmd_postestimation <- sprintf("Rscript postestimation.R %s", PROJ_DIR)
cmd_anls           <- sprintf("Rscript reg_anls.R %s"      , PROJ_DIR)

## run commands
system(cmd_reg)
system(cmd_postestimation)
system(cmd_anls)
       
