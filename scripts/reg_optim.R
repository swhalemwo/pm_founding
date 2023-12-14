## * regression optimization testing
## move optimization testing to separate file


measure_optmz_all <- function(c_regspec_id, vrbl) {
    ## optimize once with default
    measure_optmz_base
    ## optimize once with all the configurations
    ## don't think this works: measure_optmz_base doesn't require max.iter/max.eval/profile columns
    ## also measure_optmz_base is run on dt_mdlvrbls_base, while measure_optmz_modfd runs on dt_mdlvrbls_modfd
    
}

measure_optmz_base <- function(c_regspec_id, vrblx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' base way of getting the optimal lag
    #' needed as measure of correctness
    
    ## get the reg_spec from the ID
    c_regspec <- chuck(l_mdls_wid, c_regspec_id)

    ## optimize lag
    t1 <- Sys.time()
    x_opt <- optmz_vrbl_lag(c_regspec, vrblx = vrblx, loop_nbr =1, fldr_info = fldr_info_optmz,
                            reg_settings = reg_settings_garage, verbose = T)
    t2 <- Sys.time()

    ## return results
    list(
        vrbl = vrblx,
        time = t2-t1,
        lag_optmz = adt(x_opt$lngtd_vrbls)[vrbl == vrblx, lag])
}
        
## measure_optmz_base(names(l_mdls_wid)[3], "ghweal992j")  # "shweal992j_p90p100")
## kinda works

## TODO: this function
measure_optmz_modfd <- function(c_regspec_id, vrblx, max_iter, max_eval, profile) {
    ## have to generate the appropriate glmmtmb_control here
    ## might be necessary to run model once completely to get good starting values
    ## might make sense to make that own function tho, to easy collect things together? see tomorrow

    ## glmmTMBControl(optCtrl = list(iter.max = max_
    
}

gl_dtc_regopt <- function(l_mdls_wid, c_itermax, c_evalmax, c_profile,
                          nbr_regspecs, nbr_mdl_topt) {
    #' generate the regression optimization dataframes: dtc: data.table, serving as configuration
    
    
    #' @param l_regspecs list of regression specification objects, genereated by gen_batch_reg_specs
    #' @c_itermax vector of iter.max configurations, specifying the thresholds (numeric)
    #' @c_evalmax vector of eval.max configurations specifying the thresholds (numeric)
    #' @c_profile vector of profile configuration, specifying toggle (boolean)
    #' @nbr_regspecs how many regspecs to choose parameters from
    #' @nbr_mdl_topt how many parameters to test for optimization 
    
    
    ## dt_conds <- expand.grid(iter_max = seq(10, 40, 10), eval_max = seq(10, 40, 10), profile = c(T,F)) %>% adt
    dt_conds <- expand.grid(iter_max = c_itermax, eval_max = c_evalmax, profile = c_profile) %>% adt

    ## choose some regspecs
    l_mdls_wid_sample <- sample(l_mdls_wid, nbr_regspecs)
    ## l_mdls_wid <- setNames(l_regspecs_test, map(l_regspecs_test, ~chuck(.x, "mdl_id")))

    ## generate dt of which variables to optimize for which models by extracting lngtd_vrbls
    ## skip interactions/squares, don't need starting lag here
    dt_mdlvrbls_base <- imap(l_mdls_wid_sample, ~adt(.x$lngtd_vrbls)[, regspec_id := .y]) %>% rbindlist %>%
        .[!grepl("interact|sqrd", vrbl)] %>% .[, lag := NULL] %>%
        .[sample(1:.N, size = nbr_mdl_topt)] # select only some parameters at random

    ## construct dt of conditions for modified optimization
    ## run dt_mdlvrbls_base
    dt_mdlvrbls_modfd <- merge(copy(dt_mdlvrbls_base)[, join_on := 1],
                               copy(dt_conds)[, join_on := 1], on = "join_on", allow.cartesian = T)

    l_dtc_regopt <- list(
        base = dt_mdlvrbls_base,
        modfd = dt_mdlvrbls_modfd)
    
    return(l_dtc_regopt)
}

stop("optim funcs done")


## * main
## set up conditions

l_mdls_wid <- setNames(reg_spec_mdls_optmz, map(reg_spec_mdls_optmz, ~chuck(.x, "mdl_id")))

c_regopt1 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(10, 40, 10),
    c_evalmax = seq(10, 40, 10),
    c_profile = c(T,F),
    nbr_regspecs = 10,
    nbr_mdl_topt = 20)

l_dtc_regopt <- do.call("gl_dtc_regopt", c_regopt1)


l_dtc_regopt$base[1, measure_optmz_base(regspec_id, vrbl)]


## *** testing that optimization arguments can be passed on

x$cfg$regcmd <- "glmmTMB_wctrl"

glmmtmb_control1 <- glmmTMBControl(optCtrl = list(iter.max = 100), profile = F)

## glmmtmb_control1$start_vlus_beta <- adt(resx$res_parsed$coef_df)[, setNames(coef, vrbl_name)]
## glmmtmb_control1$start_vlus_theta <- start_vlus_theta
## glmmtmb_control1$start_vlus_b <- start_vlus_b

## try only estimating one variable
vrbl_toopt <- "shweal992j_p90p100_lag3"


glmmtmb_control1$mainfunc_params <- list(
    start = list(
        beta = adt(resx2$res_parsed$coef_df)[, setNames(coef, vrbl_name)],
        b = start_vlus_b),
    map = list(
        beta = adt(resx2$res_parsed$coef_df)[, factor(ifelse(vrbl_name == vrbl_toopt, 1, NA))],
        b = factor(c(1, rep(NA, 149))))
        ## b = factor(rep(NA, 150)))
)

t1 <- Sys.time()
resx <- run_vrbl_mdl_vars(x, vvs, fldr_info_optmz, verbose = F, wtf = F, glmmtmb_control = glmmtmb_control1,
                  return_objs = "res_parsed")
t2 <- Sys.time()
t2-t1
