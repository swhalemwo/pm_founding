## * regression

args <- commandArgs(trailingOnly = T)
options(width = 115)


## PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
PROJECT_DIR <- args[1]
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_reg.R"))



vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n= reg_settings_optmz$n_vrbl_thld_choices)




reg_spec_mdls_optmz <- gen_batch_reg_specs(reg_settings_optmz, vvs, vrbl_thld_choices_optmz)
print(len(reg_spec_mdls_optmz))

print(chuck(reg_settings_optmz, "batch_version"))
fldr_info_optmz <- setup_regression_folders_and_files(REG_MONKEY_DIR, reg_settings_optmz$batch_version)

setup_db_mdlcache(fldr_info_optmz)


mclapply(reg_spec_mdls_optmz, \(x) optmz_reg_spec(x, fldr_info_optmz, reg_settings_optmz),
         mc.cores = NBR_THREADS, mc.preschedule = F)


print("models have been run, now saving files")

## walk(OBJS_TO_RDS_REG, ~saveRDS(get(.x), file = paste0(RDS_DIR, .x, ".rds")))

## save fldr_info_optmz for later steps
saveRDS(fldr_info_optmz, file = paste0(fldr_info_optmz$BATCH_DIR, "fldr_info_optmz.rds"))

## this stop should never be commented out 
## stop("regression is DONE")
print("regression is DONE")



