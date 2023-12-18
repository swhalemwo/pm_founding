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


m_glmmtmb_control <- function(c_regspec, glmmtmb_control, vrblx, fixbeta, fixb) {
    #' expand the glmmtmb_control object based on fixbeta/fixb
    #' set starting values, constrain all values but the  beta of vrblx

    ## first run reg if needed
    if (fixbeta|fixb) {
        ## stitch together functionality to run model
        iv_vars <- c_regspec$mdl_vars[c_regspec$mdl_vars %!in% vvs$base_vars] %>%
            keep(~!grepl("^SP\\.POP\\.TOTLm", .x))
        
        dfx <- chuck(cbn_df_dict, c_regspec$cfg$dvfmt, c_regspec$cfg$cbn_name) %>%
            select(all_of(c(vvs$base_vars, "iso3c_num", "nbr_opened",iv_vars, "SP_POP_TOTLm_lag0_uscld")))

        fx <- gen_r_f(c_regspec$cfg$dvfmt, iv_vars)

        rx_glmmTMB_base <- glmmTMB(fx, dfx, family = nbinom1)

        ## generate some some summary objects
        map_b <- rx_glmmTMB_base$fit$parfull[names(rx_glmmTMB_base$fit$parfull) == "b"]
        dt_sumry <- summary(rx_glmmTMB_base) %>% coef %>% chuck("cond") %>% adt(keep.rownames = "vrbl")


        ## add element to attach fixed elements to
        glmmtmb_control$mainfunc_params <- list(
            start = list(),
            map= list())
    }
    
    ## adjust  glmmtmb_control$mainfunc_params corresponding to fixbeta/fixb
    
    if (fixbeta) {
        ## actually important that variables are passed in correct order, otherwise map/start will be super wrong
        ## seems to be the case tho... 

        glmmtmb_control$mainfunc_params$start <- c(
            glmmtmb_control$mainfunc_params$start,
            list(beta = dt_sumry[, setNames(Estimate, vrbl)]))

        glmmtmb_control$mainfunc_params$map <- c(
            glmmtmb_control$mainfunc_params$map,
            list(beta = dt_sumry[, factor(ifelse(gsub("_lag[1-5]", "", vrbl) == vrblx, 1, NA))]))
        
    }
    
    if (fixb) {

        glmmtmb_control$mainfunc_params$start <- c(
            glmmtmb_control$mainfunc_params$start,
            list(b = map_b))

        glmmtmb_control$mainfunc_params$map <- c(
            glmmtmb_control$mainfunc_params$map,
            list(b = factor(rep(NA,  len(map_b)))))

    }

    return(glmmtmb_control)

}

measure_optmz_modfd <- function(c_regspec_id, vrblx, max_iter, max_eval, profile, fixbeta, fixb,
                                rank_check, conv_check, eigval_check) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ## have to generate the appropriate glmmtmb_control here
    ## might be necessary to run model once completely to get good starting values
    ## might make sense to make that own function tho, to easy collect things together? see tomorrow

    c_regspec <- chuck(l_mdls_wid, c_regspec_id)
    c_regspec$cfg$regcmd <- "glmmTMB_wctrl"

    t1 <- Sys.time()
    
    glmmtmb_control <- glmmTMBControl(optCtrl = list(iter.max = max_iter, eval.max = max_eval),
                                      profile = profile, rank_check = rank_check, conv_check = conv_check,
                                      eigval_check = eigval_check)

    ## run full model once to get values to fix, if necessary
    glmmtmb_control2 <- m_glmmtmb_control(c_regspec, glmmtmb_control, vrblx, fixbeta, fixb)

    
    ## FIXME: there's some weird coercion going on: LL is a dt in run_vrbl_mdl_vars? 
    x_opt <- optmz_vrbl_lag(c_regspec, vrblx = vrblx, loop_nbr = 1, fldr_info = fldr_info_optmz,
                            reg_settings = reg_settings_garage, glmmtmb_control = glmmtmb_control2,
                            return_obj = "dt_presence",
                            verbose = T)
    
    t2 <- Sys.time()
    t2-t1

    ## not sure this is sufficient yet, but need stuff that doesn't converge.. 
    
    l_res <- list(
        ## vrbl = vrblx,
        time = t2-t1,
        lag_optmz = x_opt[, which.max(ll)],
        prop_cvrgd = x_opt[, sum(!is.na(ll))/.N])

    return(l_res)
    
}

gl_dtc_regopt <- function(l_mdls_wid, c_itermax, c_evalmax, c_profile, c_fixbeta, c_fixb,
                          c_eigvalcheck, c_rankcheck, c_convcheck,
                          nbr_regspecs, nbr_mdl_topt) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the regression optimization dataframes: dtc: data.table, serving as configuration
    
    
    #' @param l_regspecs list of regression specification objects, genereated by gen_batch_reg_specs
    #' @c_itermax vector of iter.max configurations, specifying the thresholds (numeric)
    #' @c_evalmax vector of eval.max configurations specifying the thresholds (numeric)
    #' @c_profile vector of profile configuration, specifying toggle (boolean)
    #' @nbr_regspecs how many regspecs to choose parameters from
    #' @nbr_mdl_topt how many parameters to test for optimization 
    
    
    dt_conds <- expand.grid(iter_max = c_itermax, eval_max = c_evalmax, profile = c_profile,
                            fixbeta = c_fixbeta, fixb = c_fixb, eigval_check = c_eigvalcheck,
                            rank_check = c_rankcheck, conv_check = c_convcheck,
                            stringsAsFactors = F) %>% adt

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
                        .[vrbl != "SP.POP.TOTLm"] %>% # population not allowed
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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run measure_optmz_modfd along a bunch of configs in parallel

    dt_modfd_sample <- dt_modfd[sample(x = 1:.N, size = sizex)]
    
    ## select and rename columns as needed for measure_optmz_modfd
    l_regopt_modfd <- dt_modfd_sample[, .(c_regspec_id = regspec_id, vrblx = vrbl,
                                          max_iter = iter_max, max_eval = eval_max, profile,
                                          fixbeta, fixb, rank_check, eigval_check, conv_check )] %>%
        split(1:sizex) %>% map(as.list)

    do.call("measure_optmz_modfd", l_regopt_modfd[[1]])

    ## actually run 
    l_modfd_sample_res <- mclapply(l_regopt_modfd, \(x) do.call("measure_optmz_modfd", x),
                                      mc.preschedule = F, mc.cores = 5)

    dt_modfd_sample_res <- rbindlist(l_modfd_sample_res)
    
    
    ## combine
    dt_modfd_sample_res <- cbind(dt_modfd_sample, dt_modfd_sample_res)

    return(dt_modfd_sample_res)

}

run_regopt <- function(c_regopt, sizex) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    print("generate the settings to test")
    l_dtc_regopt <- do.call("gl_dtc_regopt", c_regopt)

    ## expand.grid(c_regopt[substring(names(c_regopt), 1, 2) == "c_"] ) %>% adt

    print("----------calculate the base/real lags------------")
    ## ## generating results for base
    ## l_dtc_regopt$dt_base_res <- l_dtc_regopt$dt_base %>% copy() %>% .[, nbr := 1:.N] %>%
    ##     .[, c("time", "lag_optmz") := measure_optmz_base(regspec_id, vrbl), nbr]
    dt_base_res <- gd_measure_optmz_base_res(l_dtc_regopt$dt_base)


    ## generate results for modfd
    
    print("----------calculate the modified lags-------")

    dt_modfd_sample_res <- gd_measure_optmz_modfd_res(
        l_dtc_regopt$dt_modfd, sizex = sizex)

    print("combine results")
    ## combine base results with modified results to infer correct lag
    ## dt_res_cbnd <- dt_modfd_sample_res[dt_base_res[, .(vrbl, regspec_id, time_base = time,
    ##                                                    lag_optmz_base = lag_optmz)],
    ##                                              on = .(vrbl, regspec_id)] %>%
    ##     .[, correct_lag := lag_optmz_base == lag_optmz]

    dt_res_cbnd <- dt_base_res[, .(vrbl, regspec_id, time_base = time,
                                   lag_optmz_base = lag_optmz)][
        dt_modfd_sample_res, on = .(vrbl, regspec_id)] %>%
        .[, correct_lag := lag_optmz_base == lag_optmz] %>%
        .[is.na(correct_lag), correct_lag := F] %>% # didn't converge properly -> you fail
        .[, `:=`(time_base = as.numeric(time_base), time = as.numeric(time))] %>% 
        .[, `:=`(time_saved = time_base - time, speedup = time/time_base)]

    ## combine all the objects  together to return comfily
    l_dtc_regopt2 <- c(l_dtc_regopt,
                       list(
                           dt_base_res = dt_base_res,
                           dt_modfd_sample_res = dt_modfd_sample_res,
                           dt_res_cbnd = dt_res_cbnd))
    

    return(l_dtc_regopt2)
}


stop("optim funcs done")


gd_regopt_eval <- function(l_regopt, c_regopt) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' evaluate the optimization results
    #' so far this function is manually entered via the debugger to inspect results interactively FIXME


    ## need to convert columns from regopt_name to dt_res_cbnd name to genreate condition_colums
    dt_cfg_renamer <- list(
        list(name_regopt = "c_profile", name_res = "profile"),
        list(name_regopt = "c_fixbeta", name_res = "fixbeta"),
        list(name_regopt = "c_fixb", name_res = "fixb"),
        list(name_regopt = "c_eigvalcheck", name_res = "eigval_check"),
        list(name_regopt = "c_rankcheck", name_res = "rank_check"),
        list(name_regopt = "c_convcheck", name_res = "conv_check")) %>% rbindlist

    ## generate vector of conditions for grouping/regression
    c_opts_tokeep <- c_regopt[names(c_regopt) %in% dt_cfg_renamer$name_regopt] %>% keep(~len(.x) > 1)
    l_condcols <- dt_cfg_renamer[name_regopt %in% names(c_opts_tokeep), name_res]
    
    ## l_regopt$dt_res_cbnd[, `:=`(time_base = as.numeric(time_base), time = as.numeric(time))]
    ## l_regopt$dt_res_cbnd[, time_saved := time_base - time]
    ## l_regopt$dt_res_cbnd[, speedup := time/time_base] # ratio of base  time to overall time
    
    ## generate a histogram
    hist(l_regopt$dt_res_cbnd$time_saved)

    ## generate crosstab, i.e. aggregate by treatment combinations
    dt_acc_crosstab <- l_regopt$dt_res_cbnd[, .(accuracy = mean(correct_lag),
                                                time_base = mean(time_base), time = mean(time),
                                                savings = mean(time_base - time),
                                                speedup = mean(speedup),
                                                .N),
                                            by = l_condcols]

    ## look add most promising combinations
    ## just time savings
    dt_acc_crosstab %>% .[accuracy > 0.95] %>% .[order(-savings)] 

    ## time savings and accuracy
    ggplot(dt_acc_crosstab, aes(x=speedup, y=accuracy))+ geom_point() 
    
    dt_acc_crosstab[accuracy > 0.95 & speedup < 0.6]

    ## look at why rank_check, conv_check and eigval_check don't matter
    ## no idea how to pass vector to dt[i] component
    ## dt_acc_crosstab[order(get(l_condcols))]
    ## fsort(dt_acc_crosstab, l_condcols)
    ## dt_acc_crosstab[order(profile, fixbeta, fixb, eigval_check, rank_check, conv_check)]

    ## look at sparseX, doesn't seem to matter
    ## t1 <- Sys.time()
    ## glmmTMB(cyl ~ mpg + disp, rbind(mtcars, mtcars, mtcars, mtcars, mtcars), sparseX = T)
    ## t2 <- Sys.time()
    ## t2-t1

    ## look at no optimization condition: should be same as base
    l_regopt$dt_res_cbnd[profile == F & fixbeta == F & fixb == F & eigval_check == F & rank_check == "warning"
                         & conv_check == "warning", .(time_base, time, time_saved, speedup)]

    fx_acc <- paste0("correct_lag ~ ", paste0(l_condcols, collapse = " + ")) %>% as.formula
    fx_time <- paste0("time_saved ~ ", paste0(l_condcols, collapse = " + ")) %>% as.formula
    fx_speedup <- paste0("speedup ~ ", paste0(l_condcols, collapse = " + ")) %>% as.formula

    rx_acc <- glm(fx_acc, l_regopt$dt_res_cbnd, family = "binomial")
    rx_time <- lm(fx_time, l_regopt$dt_res_cbnd)
    fx_speedup <- lm(fx_speedup, l_regopt$dt_res_cbnd)

    screenreg2(list(rx_acc, rx_time, fx_speedup))

    

    ## generate accuracy regression
    ## generate time regression 


}

## * main
## set up conditions

l_mdls_wid <- setNames(reg_spec_mdls_optmz, map(reg_spec_mdls_optmz, ~chuck(.x, "mdl_id")))



## narrow down even further itermax  for profile = T
c_regopt6 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(1, 30, 2),
    c_evalmax = seq(1, 30, 2),
    c_profile = c(T, F),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    nbr_regspecs = 108,
    nbr_mdl_topt = 400)

l_regopt6 <- run_regopt(c_regopt6, sizex = 2000)

## narrow down even further itermax  for profile = T
c_regopt8 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = 100,
    c_evalmax = 100,
    c_profile = c(T, F),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    c_eigvalcheck = c(T,F),
    c_rankcheck = c("skip", "warning"),
    c_convcheck = c("skip", "warning"),
    nbr_regspecs = 108,
    nbr_mdl_topt = 100)

l_regopt8 <- run_regopt(c_regopt8, sizex = 400)

## narrow down even further itermax  for profile = T
c_regopt9 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = 100,
    c_evalmax = 100,
    c_profile = c(T, F),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    c_eigvalcheck = c(T,F),
    c_rankcheck = c("skip", "warning"),
    c_convcheck = c("skip", "warning"),
    nbr_regspecs = 108,
    nbr_mdl_topt = 100)

l_regopt9 <- run_regopt(c_regopt9, sizex = 700)

## narrow down even further itermax  for profile = T
c_regopt10 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = 100,
    c_evalmax = 100,
    c_profile = c(T, F),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    c_eigvalcheck = c(T,F),
    c_rankcheck = c("skip", "warning"),
    c_convcheck = c("skip", "warning"),
    nbr_regspecs = 108,
    nbr_mdl_topt = 108)

l_regopt10 <- run_regopt(c_regopt10, sizex = 1400)


gd_regopt_eval(l_regopt10, c_regopt10)



l_regopt6$dt_base_res
l_regopt6$dt_modfd_sample_res$profile

l_regopt6$dt_res_cbnd2 <- l_regopt6$dt_base_res[, .(vrbl, regspec_id, time_base = time,
                         lag_optmz_base = lag_optmz)][l_regopt6$dt_modfd_sample_res,
                                                      on = .(vrbl, regspec_id)] %>%
    .[, correct_lag := lag_optmz_base == lag_optmz] %>%
    .[is.na(correct_lag), correct_lag := F]


    
l_regopt6$dt_res_cbnd[, .N, profile]


## l_dtc_regopt$base[1, measure_optmz_base(regspec_id, vrbl)]
## l_dtc_regopt$modfd[1, measure_optmz_modfd(regspec_id, vrbl, iter_max, eval_max, profile)]

## l_dtc_regopt2$dt_res_cbnd %>%
## l_regopt$dt_res_cbnd %>%
l_regopt6$dt_res_cbnd2 %>% .[time < 15] %>% 
    ggplot(aes(x=iter_max, y=eval_max, shape = correct_lag,
               ## color = as.numeric(time)
               color = correct_lag
               )) +
    facet_grid(fixbeta + fixb ~profile) + 
    geom_jitter(width = 1, height = 1, size = 4)
## seems as with profile=T, even at iter/eval_max = 10, 10 I get correct lag estimation 

l_regopt6$dt_res_cbnd2[, .(time_modfd =mean(time), time_base = mean(time_base))]
l_regopt6$dt_base_res[, mean(time)]

## tile of time
l_regopt6$dt_res_cbnd2[, .(time_modfd =mean(time)), .(profile, fixbeta, fixb, iter_max, eval_max)] %>%
    ggplot(aes(x=iter_max, y=eval_max, fill = as.numeric(time_modfd)))  +
    facet_grid(fixbeta + fixb ~profile) +
    geom_tile()



l_regopt4$dt_res_cbnd[profile == T, .(mean_time = mean(time)), .(iter_max, eval_max)] %>%
    ggplot(aes(x=iter_max, y=eval_max, fill = as.numeric(mean_time))) +
    geom_tile()

## most basic regression
glm(correct_lag ~ profile + iter_max + eval_max + fixb + fixbeta,
    ## l_regopt6$dt_res_cbnd2,
    l_regopt6$dt_res_cbnd2[!is.na(lag_optmz)],
    family = "binomial") %>% summary
## profile T, eval_max, fixbetaT, increases
## that fixbeta increases accuracy is super weird: it's a restriction, so it would be ok if it wouldn't make it worse
## probably controlling for some collider here. or something else weird
## disappears when yeeting all those that didn't converge.. probably this happens rarely in fixbetaT conditions.. 
## then fixb decreases fit, kinda expected, but 

## time: 
lm(as.numeric(time) ~ profile + iter_max + eval_max + fixb + fixbeta, l_regopt6$dt_res_cbnd2) %>% summary
## fixbetaT decreases it.. 

## all kinds of interactions, but basically become uninterpretable, especially three-ways
glm(correct_lag ~ profile * fixb * fixbeta + iter_max * eval_max, l_regopt6$dt_res_cbnd2,
    family = "binomial") %>% summary

l_regopt6$dt_res_cbnd2[profile == T & fixbeta == T & fixb == F,
                       .(mean_accuracy = mean(correct_lag), mean_time_savings = mean(time_base - time))]

## summary
l_regopt8$dt_res_cbnd[, .(
              mean_accuracy = mean(correct_lag), mean_time_savings = mean(time_base - time), .N),
              .(profile, fixbeta, fixb, eigval_check, rank_check, conv_check)] %>% print(n=30)













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


## *** old l_regopt results
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


## first exploration with fixbeta/fixb
c_regopt5 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(1, 30, 5),
    c_evalmax = seq(1, 30, 5),
    c_profile = c(T, F),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    nbr_regspecs = 108,
    nbr_mdl_topt = 20)

l_regopt5 <- run_regopt(c_regopt5, sizex = 200)



## try to get asynchromous stuff working, but doesn't work
c_regopt7 <- list(
    l_mdls_wid = quote(l_mdls_wid),
    c_itermax = seq(10, 20, 10),
    c_evalmax = seq(10, 20, 10),
    c_profile = c(T),
    c_fixbeta = c(T,F),
    c_fixb    = c(T,F),
    nbr_regspecs = 108,
    nbr_mdl_topt = 10)

l_regopt7 <- run_regopt(c_regopt7, sizex = 40)

l_regopt7_proc <- r_bg(run_regopt, args = list(c_regopt = c_regopt7, sizex= 40),
                       error = "error")
l_regopt7_proc$get_result()

library(promises)
?promises
# Create a vector of inputs
inputs <- 1:10


## callR: recommended by chatgpt
kapparino <- r_bg(your_function, args= list(x=5))
r_bg_get(kapparino)

kapparino$get_result()
your_function(5)

10



