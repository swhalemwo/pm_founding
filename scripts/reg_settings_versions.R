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

## ** v61: no more closing information, artnews collectors not filtered

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v61",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v62: most recent version (maybe some data changes?)
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

## vrbl_thld_choices_optmz <- filter(vrbl_thld_choices, hnwi_var == "hnwi_nbr_5M",
##                                   inc_ineq_var == "sptinc992j_p99p100", weal_ineq_var == "shweal992j_p99p100")


reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v62",
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v63: only lag of 1 year, requires generalization of lag usage 

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=4)

## vrbl_thld_choices_optmz <- filter(vrbl_thld_choices, hnwi_var == "hnwi_nbr_5M",
##                                   inc_ineq_var == "sptinc992j_p99p100", weal_ineq_var == "shweal992j_p99p100")


reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v63",
    lags <- 1,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v64: full lag space with new parameterized framework

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=4)

## vrbl_thld_choices_optmz <- filter(vrbl_thld_choices, hnwi_var == "hnwi_nbr_5M",
##                                   inc_ineq_var == "sptinc992j_p99p100", weal_ineq_var == "shweal992j_p99p100")


reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v64",
    lags = c(1,2,3),
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v65: full stuff again to calculate one-out for all variables

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v65",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)
## ** v66: full stuff again, but without NPO.tax exemption (manually unspecified)

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v66",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v67: same as v66, just see if the one-out analysis works properly
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_nbr = "v67",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v68: test new combination of splitted config files

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=1)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v68",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v69 (nice): properly scaled squared variables

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v69",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v70: test new synchronized square variables 

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=10)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v70",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v71: full model, but accidentally only run 1 spec per threshold
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v72",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)


## ** v72: full model with 3 specs per threshold
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 3,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v72",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v73: full model with 5 specs per threshold

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 5,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v73",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v74: full model with PPP adjusted cultural spending
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 5,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v74",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v75: sam eas v74, but after adjusting CYs to exclude pre-min(AN) and fixing max wealth gini to 0.95

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 5,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v75",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("xtnbreg"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## ** v76: glmmTMB; also after yeeting ARE (super slow tho, overnight only 30k models are running)

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 5,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v76",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full")
)

## v77 to v82 were all kinds of tests not worth archiving


## ** v83: first full run with glmmTMB, caching, other optimizations
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v83",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v85: second full glmmTMB run: better ID tracking, no prescheduling
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v85",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v88: even better ID tracking? fuck I forgot
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v88",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v89: just some small version to find early convergence exits
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=3)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 6,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v89",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v90: test new convergence, still doesn't work properly :(
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=3)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v90",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    ## cbns_to_include = c("cbn_all"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)
## ** v91: test full models again whether convergence works (after yeeting gulf states), still doesn't fully
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 4,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v91",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v92: just test whether code still runs after picking up after 6 months
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=2)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v92",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)


## ** v93: check whether results are substantially different if there is no lag: yup they are
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v93",
    lags = 1,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v94: just have empty one to check optimization functions

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v94",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v95: test that modified regression.R functions still work
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=3)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v95",
    lags = 1:3,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)


## ** v96 check neighbor density

vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=2)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v96",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)

## ** v97: short test over lunch to check that GDP growth rates work 
vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=3)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 1,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v97",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_dfs_counts)[1:3],
    mdls_to_include = c("full"),
    wtf = T
)




## * read settings back in 

fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v46$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v47$batch_nbr)
fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz_v48$batch_nbr)
