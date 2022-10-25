## * reg_settings versions: start storing versions

## ** v45: just some menbreg run? no maybe testing something else idk
reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    batch_nbr = "v45",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs)[1:3],
    mdls_to_include = c("full")
)


## ** v46: compares menbreg and xtnbreg

reg_settings_optmz_v46 <- list(
    nbr_specs_per_thld = 2,
    batch_nbr = "v46",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs)[1:3],
    mdls_to_include = c("full")
)


## ** v47: run all the threshold varied randomly -> no optimization 

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz_v47 <- list(
    nbr_specs_per_thld = 3,
    batch_nbr = "v47",
    vary_vrbl_lag = T,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs)[1:3],
    mdls_to_include = c("full")
)

mclapply(reg_spec_mdls_optmz, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_optmz), mc.cores = 6)

## ** v48: run all the threshold varied randomly -> no optimization, non-convergence errors fixed

reg_settings_optmz_v48 <- list(
    nbr_specs_per_thld = 3,
    batch_nbr = "v48",
    vary_vrbl_lag = T,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs)[1:3],
    mdls_to_include = c("full")
)

reg_settings_optmz <- reg_settings_optmz_v48

## ** v49: optimize run on all thresholds; why it takes so long :(((

reg_settings_optmz_v49 <- list(
    nbr_specs_per_thld = 3,
    batch_nbr = "v49",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs)[1:3],
    mdls_to_include = c("full")
)


## * read settings back in 

fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v46$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v47$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v48$batch_nbr)
