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
    l_res <- list(
        ## vrbl = vrblx, # shouldn't really be needed
        time = t2-t1,
        lag_optmz = adt(x_opt$lngtd_vrbls)[vrbl == vrblx, lag])

    return(l_res)

}
        
## measure_optmz_base(names(l_mdls_wid)[3], "ghweal992j")  # "shweal992j_p90p100")
## kinda works

## TODO: this function
measure_optmz_modfd <- function(c_regspec_id, vrblx, max_iter, max_eval, profile) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ## have to generate the appropriate glmmtmb_control here
    ## might be necessary to run model once completely to get good starting values
    ## might make sense to make that own function tho, to easy collect things together? see tomorrow

    c_regspec <- chuck(l_mdls_wid, c_regspec_id)
    c_regspec$cfg$regcmd <- "glmmTMB_wctrl"

    t1 <- Sys.time()
    
    glmmtmb_control <- glmmTMBControl(optCtrl = list(iter.max = max_iter, eval.max = max_eval),
                                      profile = profile)
    
    x_opt <- optmz_vrbl_lag(c_regspec, vrblx = vrblx, loop_nbr = 1, fldr_info = fldr_info_optmz,
                            reg_settings = reg_settings_garage, glmmtmb_control = glmmtmb_control,
                            return_obj = "dt_presence",
                            verbose = T)
    
    t2 <- Sys.time()
    t2-t1

    ## not sure this is sufficient yet, but need stuff that doesn't converge.. 
    
    list(
        ## vrbl = vrblx,
        time = t2-t1,
        lag_optmz = x_opt[, which.max(ll)],
        prop_cvrgd = x_opt[, sum(!is.na(ll))/.N])

    
}

gl_dtc_regopt <- function(l_mdls_wid, c_itermax, c_evalmax, c_profile,
                          nbr_regspecs, nbr_mdl_topt) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
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

    ## generate dt of which variables to optimize for which models by extracting variables of model
    ## skip interactions/squares, don't need starting lag here
    dt_mdlvrbls_base <- imap(l_mdls_wid_sample,
                             ~data.table(vrbl = .x$mdl_vars, regspec_id = .y)) %>% rbindlist %>%
                        .[grepl("_lag[1-5]", vrbl)] %>% # yeet those that don't have lags
                        .[!grepl("interact|sqrd", vrbl)] %>% # yeet interactions/squared
                        .[, vrbl := gsub("_lag[1-5]", "", vrbl)] %>%  # yeet lags
                        .[sample(1:.N, size = nbr_mdl_topt)] # select only some parameters at random

    ## construct dt of conditions for modified optimization
    ## run dt_mdlvrbls_base
    dt_mdlvrbls_modfd <- merge(copy(dt_mdlvrbls_base)[, join_on := 1],
                               copy(dt_conds)[, join_on := 1], on = "join_on", allow.cartesian = T)

    l_dtc_regopt <- list(
        dt_base = dt_mdlvrbls_base,
        dt_modfd = dt_mdlvrbls_modfd)
    
    return(l_dtc_regopt)
}

gd_measure_optmz_base_res <- function(dt_base) {
    ## run the optimization of the base regspecs (no glmmTMB control)
    ## run in parallel in same setup as gd_measure_optmz_modfd_res to get comparability
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    l_regopt_base <- dt_base[, .(c_regspec_id = regspec_id, vrblx = vrbl)] %>% 
        split(1:nrow(dt_base)) %>% map(as.list)

    do.call("measure_optmz_base", l_regopt_base[[1]])

    dt_base_res <- mclapply(l_regopt_base, \(x) do.call("measure_optmz_base", x),
                            mc.preschedule = F, mc.cores = 5) %>% rbindlist

    dt_base_res2 <- cbind(dt_base, dt_base_res)
    return(dt_base_res2)
    

}



gd_measure_optmz_modfd_res <- function(dt_modfd, sizex) {
    #' run measure_optmz_modfd along a bunch of configs in parallel

    dt_modfd_sample <- dt_modfd[sample(x = 1:.N, size = sizex)]
    
    ## select and rename columns as needed for measure_optmz_modfd
    l_regopt_modfd <- dt_modfd_sample[, .(c_regspec_id = regspec_id, vrblx = vrbl,
                                          max_iter = iter_max, max_eval = eval_max, profile)] %>%
        split(1:sizex) %>% map(as.list)

    ## do.call("measure_optmz_modfd", l_regopt_modfd[[1]])

    ## actually run 
    dt_modfd_sample_res <- mclapply(l_regopt_modfd, \(x) do.call("measure_optmz_modfd", x),
                                    mc.preschedule = F, mc.cores = 5) %>% rbindlist
    ## combine
    dt_modfd_sample_res <- cbind(dt_modfd_sample, dt_modfd_sample_res)

    return(dt_modfd_sample_res)

}

run_regopt <- function(c_regopt, sizex) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    print("generate the settings to test")
    l_dtc_regopt <- do.call("gl_dtc_regopt", c_regopt)

    print("calculate the base/real lags")
    ## ## generating results for base
    ## l_dtc_regopt$dt_base_res <- l_dtc_regopt$dt_base %>% copy() %>% .[, nbr := 1:.N] %>%
    ##     .[, c("time", "lag_optmz") := measure_optmz_base(regspec_id, vrbl), nbr]
    dt_base_res <- gd_measure_optmz_base_res(l_dtc_regopt$dt_base)


    ## generate results for modfd
    print("calculate the modified lags")
    dt_modfd_sample_res <- gd_measure_optmz_modfd_res(
        l_dtc_regopt$dt_modfd, sizex = sizex)

    print("combine results")
    ## combine base results with modified results to infer correct lag
    dt_res_cbnd <- dt_modfd_sample_res[dt_base_res[, .(vrbl, regspec_id, time_base = time,
                                                       lag_optmz_base = lag_optmz)],
                                                 on = .(vrbl, regspec_id)] %>%
        .[, correct_lag := lag_optmz_base == lag_optmz]

    ## combine all the objects  together to return comfily
    l_dtc_regopt2 <- c(l_dtc_regopt,
                       list(
                           dt_base_res = dt_base_res,
                           dt_modfd_sample_res = dt_modfd_sample_res,
                           dt_res_cbnd = dt_res_cbnd))
    

    return(l_dtc_regopt2)
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

l_regopt <- run_regopt(c_regopt1, sizex = 200)

c_regopt2 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(10, 20, 10),
    c_evalmax = seq(10, 20, 10),
    c_profile = c(T),
    nbr_regspecs = 108,
    nbr_mdl_topt = 5)

l_regopt <- run_regopt(c_regopt2, sizex = 20)


## narrow down on itermax  for profile = T
c_regopt3 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(3, 9, 3),
    c_evalmax = seq(3, 9, 3),
    c_profile = c(T),
    nbr_regspecs = 108,
    nbr_mdl_topt = 5)

l_regopt3 <- run_regopt(c_regopt3, sizex = 20)

## narrow down even further itermax  for profile = T
c_regopt4 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(1, 5, 1),
    c_evalmax = seq(1, 5, 1),
    c_profile = c(T),
    nbr_regspecs = 108,
    nbr_mdl_topt = 40)

l_regopt4 <- run_regopt(c_regopt4, sizex = 200)


## l_dtc_regopt$base[1, measure_optmz_base(regspec_id, vrbl)]
## l_dtc_regopt$modfd[1, measure_optmz_modfd(regspec_id, vrbl, iter_max, eval_max, profile)]

## l_dtc_regopt2$dt_res_cbnd %>%
## l_regopt$dt_res_cbnd %>%
l_regopt4$dt_res_cbnd %>% 
    ggplot(aes(x=iter_max, y=eval_max, shape = profile,
                   size = prop_cvrgd, color = correct_lag)) +
    facet_grid(~profile) + 
    geom_jitter()
## seems as with profile=T, even at iter/eval_max = 10, 10 I get correct lag estimation 

l_regopt4$dt_res_cbnd[profile == T, .(mean_time = mean(time)), .(iter_max, eval_max)] %>%
    ggplot(aes(x=iter_max, y=eval_max, fill = as.numeric(mean_time))) +
    geom_tile()

l_dtc_regopt$dt_base_res

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
