## * master script for org_pop

## define commands
cmd_reg <- paste0("Rscript regression.R asdf=jj")
cmd_postestimation <- paste0("Rscript postestimation.R asdf=jj")
cmd_anls <- paste0("Rscript reg_anls.R")

## run commands
system(cmd_reg)
system(cmd_postestimation)
system(cmd_anls)
       
