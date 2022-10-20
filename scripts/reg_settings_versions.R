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


fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v46$batch_nbr)
