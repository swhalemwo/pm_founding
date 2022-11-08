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

## ** v50: try rates: only menbreg code adjusted yet

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=2)

reg_settings_optmz_v50 <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v50",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v51: more extensive comparison of rates between xtnbreg and menbreg
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=12)


reg_settings_optmz_v51 <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v51",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v52: fixed yuuuge bug in rates data generation
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=2)


reg_settings_optmz_v52 <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v52",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v53: add more density variables, seems like there are convergence issues

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=6)


reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v53",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v54 more thorough test with more density variables, forget to plug in power -> laptop crashed after less than an hour

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=12)
reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v54",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** global density variables test with proper results
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=1)


reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v55",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v56: overnight run with wid_v3 (spain hnwi variables there now)

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)


reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v56",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v57: test newly scaled 
vrbl_thld_choices_optmz <- filter(vrbl_thld_choices, hnwi_var == "hnwi_nbr_5M",
                                  inc_ineq_var == "sptinc992j_p99p100", weal_ineq_var == "shweal992j_p99p100")


reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v57",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v58: count model

vrbl_thld_choices_optmz <- filter(vrbl_thld_choices, hnwi_var == "hnwi_nbr_5M",
                                  inc_ineq_var == "sptinc992j_p99p100", weal_ineq_var == "shweal992j_p99p100")


reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("counts"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v58",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("menbreg", "xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v59: only xtnbreg rates all thresholds

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)


reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v59",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v60: yeeted ISL and BHS: fucking meme countries deserve to dieeeeeeeeeeeeeee

reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v60",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg", "menbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)



## * read settings back in 

fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v46$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v47$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v48$batch_nbr)
