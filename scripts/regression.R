## * regression

args <- commandArgs(trailingOnly = T)
options(width = 115)


PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_reg.R"))



vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v19",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_df_dict$counts)[1],
    mdls_to_include = c("full"),
    wtf = T,
    max_loop_nbr = 100
)



reg_spec_mdls_optmz <- gen_batch_reg_specs(reg_settings_optmz, vvs, vrbl_thld_choices_optmz)
print(len(reg_spec_mdls_optmz))


fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz$batch_version)

setup_db_mdlcache(fldr_info_optmz)


mclapply(reg_spec_mdls_optmz, \(x) optmz_reg_spec(x, fldr_info_optmz, reg_settings_optmz),
         mc.cores = NBR_THREADS, mc.preschedule = F)


print("models have been run, now saving files")

walk(OBJS_TO_RDS_REG, ~saveRDS(get(.x), file = paste0(RDS_DIR, .x, ".rds")))

## this stop should never be commented out 
## stop("regression is DONE")
print("regression is DONE")



