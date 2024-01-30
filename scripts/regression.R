args <- commandArgs(trailingOnly = T)
options(width = 115)

## * regression

## ** variables and variable combinations (without lagging)

## all_lngtd_vars <- c("tmitr_approx_linear_2020step",
##                     "hnwi_nbr_30M",
##                     "gptinc992j",
##                     "ghweal992j",
##                     "smorc_dollar_fx",
##                     "NY.GDP.PCAP.CDk",
##                     "SP.POP.TOTLm",
##                     "clctr_cnt_cpaer"
##                     )

## base_vars <- c("iso3c", "year")
## crscn_vars <- c("sum_core", "cnt_contemp_1995")

## reg_vars <- c(base_vars, all_lngtd_vars, crscn_vars)

## vrbl_cnbs <- list(
##     all_vars = all_lngtd_vars,
##     no_cult_spending = all_lngtd_vars[all_lngtd_vars != "smorc_dollar_fx"],
##     no_cult_spend_and_mitr = all_lngtd_vars[all_lngtd_vars %!in% c("smorc_dollar_fx", "tmitr_approx_linear_2020step")],
##     controls = c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm"))

## ** sketching functionalization


cleanup_old_r_procs <- function() {
    ## gw_fargs(match.call())
    #' kill old R procs, not used so far
    
    df_ps <- ps() %>% filter(name == "R", pid != Sys.getpid()) %>%
        pull(pid) %>%
        lapply(\(x) ps_kill(ps_handle(x)))

    ## attr(df_ps, "gnrtdby") <- as.character(match.call()[[1]])
    return(df_ps)

}

cleanup_old_stata_procs <- function() {
    ## gw_fargs(match.call())
    #' yeet old (older than 12 secs) stata processes
    #' not needed anymore since now number of iterations are used to terminate stata process

    print("cleaning up old processes...")

    ps_tbl1 <- ps() %>% select(pid, name)
    ps_tbl1$t1 <- Sys.time()

    Sys.sleep(12)

    ps_tbl2 <- ps() %>% select(pid, name)
    ps_tbl2$t2 <- Sys.time()


    ## merges on id: will only list stata processes that already existed 12 secs ago
    ps_tbl <- merge(ps_tbl1, ps_tbl2) %>% atb()
    ps_tbl_overdue <- filter(ps_tbl, name == "stata")

    print(ps_tbl_overdue)

    lapply(ps_tbl_overdue$pid, \(x) tryCatch({ps_kill(ps_handle(x))}, error = \(e){}))
    print(paste0("terminated ", nrow(ps_tbl_overdue), " old stata process(es)"))
          

    ## ps_handle(ps_tbl_overdue$pid)
}




parse_stata_res <- function(stata_res, stata_output_vars, gof_names){
    ## gw_fargs(match.call())
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' parse the stata res into result_list

    stata_res_parsed <- stata_res %>% pivot_longer(cols = names(stata_res)) %>%
        mutate(meaning = c(paste0("coef_", stata_output_vars), paste0("se_", stata_output_vars),
                           paste0("gof_", gof_names)),
               variable = c(stata_output_vars, stata_output_vars, gof_names)) %>%
        select(meaning, value, variable) %>%
        mutate(is_coef = substring(meaning, 1, 5) == "coef_",
               is_se = substring(meaning, 1, 3) == "se_",
               is_gof  = substring(meaning, 1, 4) == "gof_") %>% adf()

    t_values <- filter(stata_res_parsed, is_coef)$value / filter(stata_res_parsed, is_se)$value
    p_values <- pnorm(abs(t_values), lower.tail = F)*2

    coef_df <- data.frame(vrbl_name = stata_output_vars,
                          coef = filter(stata_res_parsed, is_coef)$value,
                          se = filter(stata_res_parsed, is_se)$value,
                          pvalues = p_values)

    gof_df <- data.frame(gof_names = gof_names,
                         gof_value = filter(stata_res_parsed, is_gof)$value)

    res_list <- list(coef_df = coef_df, gof_df = gof_df)
    ## attr(res_list, "gnrtdby") <- as.character(match.call()[[1]])
    return(res_list)
}




save_parsed_res <- function(res_list, idx, fldr_info) {

    ## gw_fargs(match.call())
    #' save the parsed stata res to id

    saveRDS(res_list, file = paste0(fldr_info$REG_RES_DIR, idx))
    ## readRDS(file = paste0(PROJECT_DIR, "data/processed/res_list"))

}










gen_reg_spec <- function(non_thld_lngtd_vars, vrbl_thld_choice, reg_settings) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## gw_fargs(match.call())
    
    #' generate the regression specification: basically just choice of some variables/thresholds and lag lengths

    ## could also just sample here, probably have to
    ## x <- vrbl_thld_choices[sample(nrow(vrbl_thld_choices),1),]

    ## select variable, generate random lag

    ## lngtd_vars <- c(x$hnwi_var,
    ##                 x$inc_ineq_var,
    ##                 x$weal_ineq_var,
    ##                 non_thld_lngtd_vars) %>% atb() %>%
    ##     select(vrbl = value) %>%
    ##     ## group_by(vrbl) %>% 
    ##     mutate(lag = sample(seq(1,5),1))

    lngtd_vars <- data.frame(vrbl = c(vrbl_thld_choice$hnwi_var, vrbl_thld_choice$inc_ineq_var,
                                      vrbl_thld_choice$weal_ineq_var,  non_thld_lngtd_vars))

    
    ## lngtd_vars$lag <- sample(seq(1,5), nrow(lngtd_vars), replace = T)
    ## set lags
    lngtd_vars$lag <- sample(reg_settings$lags, nrow(lngtd_vars), replace = T)

    ## lag TI*TMITR interaction with same lag as TMITR
    ## lngtd_vars[which(lngtd_vars$vrbl == "ti_tmitr_interact"),]$lag <- lngtd_vars[lngtd_vars$vrbl == "tmitr_approx_linear20step", ]$lag
    ## newer data.table based version
    ## lngtd_vars <- adt(lngtd_vars)[vrbl == "ti_tmitr_interact",
    ##                 lag := adt(lngtd_vars)[vrbl == "tmitr_approx_linear20step", lag]] %>% adf()

    ## ## backup
    ## ## lngtd_vars_bu <- copy(lngtd_vars)
    ## lngtd_vars <- lngtd_vars_bu
    
    ## ## test how update join works with different variable combinations

    ## lngtd_vars <- adt(lngtd_vars)[-c(6, 7)]
    ## lngtd_vars[vrbl == "ti_tmitr_interact", lag := 2]
    ## lngtd_vars[vrbl == "pm_density_global", lag := 2]

    ## new constraining procedure with update join
    ## lngtd_vars <- copy(adt(lngtd_vars)) %>%
    ##   ## get the new values for the constrained variables from the determiners
    ##     .[vvs$dt_cstrnd_vrbls[adt(lngtd_vars), on = .(dtrmnr = vrbl),
    ##                       nomatch = NULL][, .(cstrnd, lag_new = lag)],
    ##       lag := lag_new, on = .(vrbl = cstrnd)] ## %>% # constrain values with update join
    ##     ## adf()

    lngtd_vars <- cstrn_vrbl_lags(lngtd_vars)

    reg_spec <- list(lngtd_vrbls = lngtd_vars)
    ## attr(reg_spec, "gnrtdby") <- as.character(match.call()[[1]])

    return(reg_spec)
}

cstrn_vrbl_lags <- function(lngtd_vars) {
    ## gw_fargs(match.call())
    ## constrain the lags of the variables listed in vvs$dt_cstrnd_vrbls
    ## to the lag of their lag determining variables

    ## get the new values for the constrained variables from the determiners
    df_lngtd_vars <- adt(lngtd_vars) %>%
        .[vvs$dt_cstrnd_vrbls[adt(lngtd_vars), on = .(dtrmnr = vrbl),
                              nomatch = NULL][, .(cstrnd, lag_new = lag)],
          lag := lag_new, on = .(vrbl = cstrnd)] %>% ## %>% # constrain values with update join
        adf()
    ## attr(df_lngtd_vars, "gnrtdby") <- as.character(match.call()[[1]])
    
    return(df_lngtd_vars)
    
}



## gen_lag(vrbl = "SP.POP.TOTLm", lag=4)

## gen_lag_df <- function(lngtd_vars, crscn_vars, base_vars) {
##     #' generate the data frame given the configuration of variables and lags (lngtd_vars)

##     lag_df_list <- apply(lngtd_vars, 1, \(x) gen_lag(vrbl=x[["vrbl"]], lag=x[["lag"]]))

##     lag_df <- Reduce(\(x,y) merge(x,y), lag_df_list) %>% atb()

##     df_reg_lags <- atb(merge(df_reg, lag_df))

##     ## can skip base vars here, are provided by lag_df
##     ## all_var_names <- c(crscn_vars, names(lag_df))
##     all_var_names <- unique(c(base_vars, crscn_vars, names(lag_df)))

##     return(df_reg_lags[all_var_names])
## }



gen_cbn_models <- function(cbn_vars, base_vars, ctrl_vars) {
    ## gw_fargs(match.call())
    #' generate the models that follow from a combination 
    #' cbn_vars: lagged variables


    subst_vars <- cbn_vars[cbn_vars %!in% c(ctrl_vars, base_vars)]
    names(subst_vars) <- subst_vars

    ## generate the models with just one of the substantive variables
    single_var_models <- lapply(subst_vars, \(x) c(base_vars, ctrl_vars, x))

    models <- list(controls = c(base_vars, ctrl_vars),
                   full = c(cbn_vars)
                   )

    ## all_cbn_models <- c(single_var_models, models)

    ## atm only use full models
    all_cbn_models <- list(full = c(cbn_vars))
    
    ## attr(all_cbn_models, "gnrtdby") <- as.character(match.call()[[1]])
    

    return(all_cbn_models)
}




    

    

gen_lag_id <- function(reg_spec, vvs) {
    ## gw_fargs(match.call())
    #' generate the variable choice lag part of the id
    #' see which longitudinal variables are included with which lag,
    #' include variables which aren't included with "X" in id 
    
    ## df_idx <- merge(vvs$lngtd_vars_tbbl, 
    ##                 reg_spec$lngtd_vrbls, all.x = T)
    df_idx <- vvs$lngtd_vars_df
    df_idx$lag <- "X"
    
    cols_to_match <- vvs$lngtd_vars_df$lngtd_vars %in% reg_spec$lngtd_vrbls$vrbl
    
    df_idx$lag[cols_to_match] <- as.character(reg_spec$lngtd_vrbls$lag)
        

    names(df_idx) <- c("variable", "value")
    ## df_idx$lag_str = as.character(df_idx$lag)
    ## df_idx$value[is.na(df_idx$value)] <- "X"
    

        ## mutate(lag_str = as.character(lag)) %>%
        ## mutate(lag_str = ifelse(is.na(lag_str), "X", lag_str)) %>%
        ## select(variable=vrbl, value = lag_str)

    ## attr(df_idx, "gnrtdby") <- as.character(match.call()[[1]])
    return(df_idx)
}
    



gen_mdl_id <- function(reg_spec, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' generate unique id for each model

    ## get id: need information which variable is there (X if not there), also of lag of each variable that is there
    
    
    df_idx <- gen_lag_id(reg_spec, vvs)


    vrbl_lag_id <- paste0(df_idx$value, collapse = "")


    ## other_cfgs <- data.frame(rbind(c("cbn_name", reg_spec$cbn_name),
    ##                                c("mdl_name", reg_spec$mdl_name),      
    ##                                c("vrbl_varied", reg_spec$vrbl_varied),
    ##                                c("base_lag_spec", reg_spec$base_lag_spec)))

    other_cfgs <- data.frame(variable = names(reg_spec$cfg), value = unname(unlist(reg_spec$cfg)))
    
    ## names(other_cfgs) <- c("variable", "value")
    ## select(variable = X1, value = X2)

    cfg_id <- paste0(other_cfgs$value, collapse = '--')

    ## mdl_id <- paste0(df_idx$value, collapse = "")
    mdl_id <- paste0(c(vrbl_lag_id, cfg_id), collapse = "--")

    ## df_idx2 <- rbind(df_idx, other_cfgs)
    df_idx$lag_spec <- vrbl_lag_id
    df_idx$cfg_id <- cfg_id
    df_idx$mdl_id <- mdl_id

    other_cfgs$cfg_id <- cfg_id
    other_cfgs$lag_spec <- vrbl_lag_id
    other_cfgs$mdl_id <- mdl_id
    
    return(list(df_idx = atb(df_idx),
                other_cfgs = atb(other_cfgs),
                mdl_id = mdl_id))
}

## t1 = Sys.time()
## ## x <- lapply(seq(200), \(x) gen_lag_id(reg_spec_mdls[[1]], vvs))
## ## x <- mclapply(reg_spec_mdls, \(x) gen_lag_id(x, vvs))
## x <- mclapply(reg_spec_mdls, \(x) gen_mdl_id(x, vvs), mc.cores = 6)
## t2 = Sys.time()
## print(t2-t1)

## gen_mdl_id(reg_spec_mdls[[1]], vvs)

prep_and_set_pid_fldr <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' if necessarily, prep the pid folder (for stata multiprocessing), set process to corresponding pid folder
    
    pid <- Sys.getpid()
    new_dir <- paste0(fldr_info$PID_DIR, pid)
    
    ## present_dirs <- list.dirs(paste0(fldr_info$PID_DIR), recursive = F)
    ## change current_dir generation, hope that doesn't break stuff
    ## print("a")
    present_dirs <- paste0(fldr_info$PID_DIR, list.dirs(fldr_info$PID_DIR, recursive = F, full.names = F))
    
    if (new_dir %!in% present_dirs) {
    
        mkdir_cmd <- paste0("mkdir ", new_dir)
        ## print("b")
        system(mkdir_cmd)

    }
    ## print("c")
    setwd(new_dir)
    ## print("d")

    ## setwd(cur_wd)
    ## print(pid)
    ## print(Sys.getpid())

    return(pid)
    
}

gen_stata_code_xtnbreg <- function(iv_vars, dvfmt, gof_names, stata_output_vars, difficult_str, technique_str) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the code for xtnbreg

    iv_vars_stata <- gsub("\\.", "_", iv_vars)
    res_names <- paste0("r", seq(len(stata_output_vars)*2 + len(gof_names)))
    maxiter <- 50

    if (dvfmt == "rates") {
        exposure_cmd = "exposure(SP_POP_TOTLm_lag0_uscld) "
    } else {
        exposure_cmd = ""
    }


    ## xtnbreg
    stata_code = list(
        ## sleep_cmd = "sleep 10000",
        panel_setup = "xtset iso3c_num year",
        cvrgd_setup = sprintf("set maxiter %s", maxiter),
        ## ## default command
        reg_cmd = paste0("capture xtnbreg nbr_opened ", paste(iv_vars_stata, collapse = " "), 
                         ", re ", exposure_cmd, difficult_str, " technique(", technique_str, ")"),
        ## population averaged part
        ## reg_cmd = paste0("capture xtnbreg nbr_opened ", paste(iv_vars_stata, collapse = " "),
        ##                  ", pa ", exposure_cmd, difficult_str),
        ## cvrg_check= paste0(c("local nbr_itr = e(ic)", sprintf("if `nbr_itr' < %s {", maxiter)), collapse = "\n"),
        cvrg_check = paste0(c("if _rc == 0 {")),
        coef_cmd = "mata: b=st_matrix(\"e(b)\")' \n mata: st_matrix(\"b_stata\", b)",
        se_cmd = "mata: se=sqrt(diagonal(st_matrix(\"e(V)\"))) \n mata: st_matrix(\"se_stata\", se)",
        gof_cmd = "matrix gof = ( e(N), e(ll), e(N_g), e(chi2), e(p), e(df_m))'", 
        cbn_cmd = "matrix stata_return = (b_stata', se_stata', gof')",
        rename_cmd = paste0("matrix colnames stata_return = ", paste0(res_names, collapse = " ")),
        sv_cmd = "svmat stata_return \n keep stata_return* \n drop if missing(stata_return1)",
        cvrg_check_curly_brace = "}",
        ## no_cvrg_return = paste0(c(sprintf("else if `nbr_itr' == %s {", maxiter),  "matrix ncvrg_mat = (`nbr_itr')", "svmat ncvrg_mat", "keep ncvrg_mat*", "drop if missing(ncvrg_mat1)", "}"), collapse = "\n")
        no_cvrg_cond = "else if _rc != 0 {",
        no_cvrg_mat = "matrix ncvrg_mat = (_rc)",
        no_cvrg_return = paste0(c("svmat ncvrg_mat", "keep ncvrg_mat*", "drop if missing(ncvrg_mat1)", "}"),
                                 collapse = "\n")
    )

    stata_src <- paste(stata_code, collapse = "\n")
    return(stata_src)

}



run_xtnbreg <- function(iv_vars, dvfmt, gof_names, dfx, technique_str, difficult_str, fldr_info, verbose) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run xtnbreg command

    xtnbreg_output_vars <- c(iv_vars, c("cons", "ln_r", "ln_s"))
    ## xtnbreg_output_vars <- c(iv_vars, c("cons")) ## population average

    pid = prep_and_set_pid_fldr(fldr_info)

    stata_code_xtnbreg <- gen_stata_code_xtnbreg(iv_vars, dvfmt, gof_names, xtnbreg_output_vars,
                                                 difficult_str, technique_str)
    
    xtnbreg_res_raw <- stata(stata_code_xtnbreg, data.in = dfx, data.out = T, stata.echo = verbose) %>% atb()
    
    if (nrow(xtnbreg_res_raw) > 1) {stop("something wrong with get_stata_result")} ## debug 

    if (names(xtnbreg_res_raw)[1] == "ncvrg_mat1") {

        ret_obj = list(converged = F, pid = pid, log_likelihood = NA)
        
    } else {

        xtnbreg_res_parsed <- parse_stata_res(xtnbreg_res_raw, xtnbreg_output_vars, gof_names)

        ret_obj = list(
            converged = T,
            pid = pid,
            log_likelihood = xtnbreg_res_parsed$gof_df[which(xtnbreg_res_parsed$gof_df$gof_names == "log_likelihood"),]$gof_value,
            res_parsed = xtnbreg_res_parsed)
        
    }
    ## save_parsed_res(xtnbreg_res_parsed, idx = file_id, fldr_info)
    
    return(ret_obj)
}

gen_stata_code_menbreg <- function(iv_vars, dvfmt, gof_names, stata_output_vars) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the stata code for menbreg
    
    iv_vars_stata <- gsub("\\.", "_", iv_vars)

    ## if format of DV is rates: generate exposure command,
    res_names <- paste0("r", seq(len(stata_output_vars)*2 + len(gof_names)))

    if (dvfmt == "rates") {
        exposure_cmd = ", exposure(SP_POP_TOTLm_lag0_uscld)"
    } else {
        exposure_cmd = ""
    }

    maxiter <- 50


    ## menbreg
    stata_code = list(
        ## sleep_cmd = "sleep 10000", 
        cvrgd_setup = sprintf("set maxiter %s", maxiter),
        reg_cmd = paste0("capture menbreg nbr_opened ", paste(iv_vars_stata, collapse = " "),
                         exposure_cmd, " || iso3c_num:"),
        ## cvrg_check= paste0(c("local nbr_itr = e(ic)", sprintf("if `nbr_itr' < %s {", maxiter)), collapse = "\n"),
        cvrg_check = paste0(c("if _rc == 0 {")),
        coef_cmd = "mata: b=st_matrix(\"e(b)\")' \n mata: st_matrix(\"b_stata\", b)",
        gof_cmd = "matrix gof = ( e(N), e(ll), e(N_g), e(chi2), e(p), e(df_m))'",
        se_cmd = "mata: se=sqrt(diagonal(st_matrix(\"e(V)\"))) \n mata: st_matrix(\"se_stata\", se)",
        cbn_cmd = "matrix stata_return = (b_stata', se_stata', gof')",
        rename_cmd = paste0("matrix colnames stata_return = ", paste0(res_names, collapse = " ")),
        sv_cmd = "svmat stata_return \n keep stata_return* \n drop if missing(stata_return1)",
        cvrg_check_curly_brace = "}",
        ## no_cvrg_cond = sprintf("else if `nbr_itr' == %s {", maxiter),
        ## no_cvrg_mat = "matrix ncvrg_mat = (`nbr_itr')",
        no_cvrg_cond = "else if _rc != 0 {",
        no_cvrg_mat = "matrix ncvrg_mat = (_rc)",
        no_cvrg_return = paste0(c("svmat ncvrg_mat", "keep ncvrg_mat*", "drop if missing(ncvrg_mat1)", "}"),
                                 collapse = "\n")
    )


    stata_src <- paste(stata_code, collapse = "\n")
    return(stata_src)
}
    

run_menbreg <- function(iv_vars, dvfmt, gof_names, dfx, fldr_info, verbose) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run menbreg command

    menbreg_output_vars <- c(iv_vars, c("cons", "alpha", "intcpt_var"))

    pid = prep_and_set_pid_fldr(fldr_info)

    stata_code_menbreg <- gen_stata_code_menbreg(iv_vars, dvfmt, gof_names, menbreg_output_vars)
    
    menbreg_res_raw <- stata(stata_code_menbreg, data.in = dfx, data.out = T, stata.echo = verbose) %>% atb()

    
    ## stata_res_raw <- get_stata_result(iv_vars = iv_vars, stata_output_vars = stata_output_vars,
    ##                                   gof_names = gof_names, dfx = dfx, technique_str=technique_str,
    ##                                   difficult_str = difficult_str, verbose = verbose)

    if (nrow(menbreg_res_raw) > 1) {stop("something wrong with stata result")} ## debug 

    if (names(menbreg_res_raw)[1] == "ncvrg_mat1") {

        ret_obj = list(converged = F, pid = pid, log_likelihood = NA)
    } else {

        menbreg_res_parsed <- parse_stata_res(menbreg_res_raw, menbreg_output_vars, gof_names)

        ## save_parsed_res(menbreg_res_parsed, idx = file_id, fldr_info)
    
        ret_obj <- list(
            converged = T,
            pid = pid,
            log_likelihood = menbreg_res_parsed$gof_df[which(menbreg_res_parsed$gof_df$gof_names == "log_likelihood"),]$gof_value,
            res_parsed = menbreg_res_parsed)

    }
    
    return(ret_obj)
}


get_r_gof <- function(rx_glmmtmb, rx_smry){
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' get the goodness of fits stats from glmmTMB run 

    r_gof_prep1 <- get_gof(rx_glmmtmb)
    r_gof_prep2 <- data.frame(gof_names = names(r_gof_prep1), gof_value = as.numeric(r_gof_prep1[1,]))

    
    ## some more gofs from AICtab
    r_gof_prep_aictab <- adf(rx_smry$AICtab)
    r_gof_prep_aictab2 <- data.table(gof_names = rownames(r_gof_prep_aictab), gof_value = r_gof_prep_aictab[,1]) %>%
        .[gof_names %in% c("logLik", "deviance", "df.resid")] %>% adf()
    
    ## add degrees of freedom (of log likelihood?)
    r_gof_df <- data.table(gof_names = "df", gof_value = attr(rx_smry$logLik, "df"))

    ## some additional gof
    disp_sigma <- glmmTMB::sigma(rx_glmmtmb)
    N_g <- rx_smry$ngrps$cond
    N_g_names <- paste0("N_g_", names(N_g))
    
    intcpt_var <- rx_smry$varcor$cond$iso3c[1,1]

    algns_gof <- data.frame(gof_names = c("disp_sigma", N_g_names, "intcpt_var"),
                            gof_value = c(disp_sigma,    N_g,   intcpt_var))

    r_gof_cbn <- Reduce(\(x,y) rbind(x,y), list(r_gof_prep2, r_gof_prep_aictab2, algns_gof, r_gof_df))

    ## rename some gofs to ensure consistency with stata gof names
    r_gof_cbn2 <- r_gof_cbn %>% adt() %>%
        .[gof_names == "nobs", gof_names := "N"] %>%
        .[gof_names == "logLik", gof_names := "log_likelihood"] %>% adf()

    return(r_gof_cbn2)

}

get_r_gof_min <- function(rx_glmmtmb) {
    #' get minimal gof: save some time
    
    loglik <- logLik(rx_glmmtmb)
    loglik_df <- attr(loglik, "df")

    data.table(gof_names = c("log_likelihood", "df"),
               gof_value = c(as.numeric(loglik), loglik_df))
}

    
    

gen_r_f <- function(dvfmt, r_vars, time_ri = F) {
    #' generate r formula for negbin glmmtmb regression model
    
    if (time_ri == T) {
        fx_prep <- sprintf("nbr_opened ~ %s + (1 | iso3c) + (1 | year)", paste0(r_vars, collapse = " + ")) 

    } else if (time_ri == F) {

        fx_prep <- sprintf("nbr_opened ~ %s + (1 | iso3c)", paste0(r_vars, collapse = " + ")) 

    }

    ## generate formula, depending on whether counts or rates are calculated
    if (dvfmt == "counts") {
        
        fx_prep2 <- fx_prep
        
    } else if (dvfmt == "rates") {
        
        fx_prep2 <- paste0(fx_prep, " + offset(log(SP_POP_TOTLm_lag0_uscld))")
        
    }
    
    fx <- as.formula(fx_prep2)
    return(fx)
}


run_glmmtmb_wctrl <- function(dfx, dvfmt, r_vars, verbose, glmmtmb_control) {
    #' run glmmTMB with control for speed checks
    #' glmmtbm_control: generated by glmmTMBControl outside of optimization framework to save generation time
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    fx <- gen_r_f(dvfmt, r_vars, time_ri = T)

    glmmtmb_args <- list(formula =quote(fx), data = quote(dfx), family = quote(nbinom1),
                         verbose=quote(verbose), control = glmmtmb_control)

    start_active <- F
    ## assign starting values, if present
    if ("start" %in% names(glmmtmb_control$mainfunc_params)) {
        glmmtmb_args$start <- glmmtmb_control$mainfunc_params$start
        start_active <- T
    }
    
    ## keep some parameters fixed
    map_active <- F
    if ("map" %in% names(glmmtmb_control$mainfunc_params)) {
        glmmtmb_args$map <- glmmtmb_control$mainfunc_params$map
        map_active <- T
    }

    ## yeet objects from control that are intended to go to main function
    if (start_active | map_active) {
        glmmtmb_args$control$mainfunc_params <- NULL
    }

        
    t1 <- Sys.time()
    rx_glmmtmb <- do.call("glmmTMB", glmmtmb_args)
    t2 <- Sys.time()
    t2-t1

    ## plot(
    ##     rx_glmmtmb$fit$parfull[names(rx_glmmtmb$fit$parfull)=="b"],
    ##     glmmtmb_args$start$b)
    

    ## start_vlus_theta <<- adt(ranef(rx_glmmtmb)) %>% .[, setNames(condval, grp)]
    ## start_vlus_b <<- rx_glmmtmb$fit$parfull[names(rx_glmmtmb$fit$parfull)=="b"]
    ## plot(start_vlus_b, rx_glmmtmb$fit$parfull[names(rx_glmmtmb$fit$parfull)=="b"])
    
    ret_obj <- extract_glmmtmb(rx_glmmtmb, map_active, start_active)

    return(ret_obj)
}




run_glmmtmb <- function(dfx, dvfmt, r_vars, verbose) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run a regression specification from R

    ## fx <- gen_r_f(dvfmt, r_vars)
    
    fx <- gen_r_f(dvfmt, r_vars, time_ri = T)

    ## glmmcrol <- glmmTMBControl(profile = T, optCtrl = list(iter.max = 300))

    ## generate results

    ## fx4 <- nbr_opened ~ Ind.tax.incentives + cnt_contemp_1990 + cnt_contemp_1990_sqrd + 
    ##     hnwi_nbr_5M_lag5 + sptinc992j_p90p100_lag1 + shweal992j_p99p100_lag5 + 
    ##     NY.GDP.PCAP.CDk_lag5 + clctr_cnt_cpaer_lag2 + pm_density_lag4 + 
    ##     pm_density_sqrd_lag4 + pm_density_global_lag5 + pm_density_global_sqrd_lag5 + 
    ##     nbr_closed_cum_global_lag5  + offset(log(SP_POP_TOTLm_lag0_uscld))

    ## iter_max_glbl <- 30
    ## glmmtmb_ctrl <- glmmTMBControl(optCtrl = list(iter.max = iter_max_glbl, eval.max = 200))
    ## glmmtmb_ctrl_default <- glmmTMBControl()
    

    ## t1 <- Sys.time()
    ## rx_glmmtmb_optim2 <- glmmTMB(fx, dfx, family = nbinom1, verbose = verbose,
    ##                              control = glmmtmb_ctrl_default)
    ## t2 <- Sys.time()
    ## rx_glmmtmb_optim1 <- glmmTMB(fx, dfx, family = nbinom1, verbose = verbose)    
    ## t3 <- Sys.time()

    ## super manual comparison to see effect of adding year random intercept, seems super irrelevant
    rx_glmmtmb <- glmmTMB(fx, dfx, family = nbinom1, verbose = verbose)

    ## rx_glmmtmb_ri <- glmmTMB(fx_ri, dfx, family = nbinom1, verbose = verbose)

    ## dt_coef <- rx_glmmtmb %>% summary %>% chuck("coefficients", "cond") %>%
    ##     adt(keep.rownames = "vrbl") %>% .[, .(vrbl, Estimate)]

    ## dt_coef_ri <- rx_glmmtmb_ri %>% summary %>% chuck("coefficients", "cond") %>%
    ##     adt(keep.rownames = "vrbl") %>% .[, .(vrbl, estim_ri = Estimate)]
    
    ## join(dt_coef, dt_coef_ri, on = "vrbl") %>% .[, diff := estim_ri - Estimate]


    ## t2-t1
    ## t3-t2
    
    
    ## rx_glmmtmb2 <- glmmTMB(fx, dfx, family = nbinom1, verbose = verbose)
    ## rx_glmmtmb3 <- glmmTMB(fx, dfx, family = nbinom1, verbose = F, dispformula = ~iso3c)
    ## rx_glmmtmb4 <- glmmTMB(fx4, dfx, family = nbinom1, verbose = F, dispformula = ~iso3c)

    

    ## offset(log(SP_POP_TOTLm_lag0_uscld))
    
    ## df.residual
    ## library(parameters)
    ## library(performance)
    ## compare_models(rx_glmmtmb, rx_glmmtmb2, rx_glmmtmb3, rx_glmmtmb4, select = "se_p")

    ## check_collinearity(rx_glmmtmb)
    ## check_model(rx_glmmtmb)


    ## glmmTMP really necessary, glmer.nb takes for fucking ever 
    ##  rx <- glmer.nb(fx, dfx)
    ## screenreg(list(rx_glmmtmb, rx))

    ## screenreg(rx_glmmtmb, digits = 5, single.row = T)
    ## parse the R res 
    ## x <- adt(coef(rx_glmmtmb)$cond$iso3c)

    ret_obj <- extract_glmmtmb(rx_glmmtmb, F, F)
}


extract_glmmtmb <- function(rx_glmmtmb, map_active, start_active) {
    #' extract information from result of glmmTMB object
    #' functionality was previously in run_glmmtmb, but now need multiple glmmTMB funcs for optimization
    #' map_active : is some parameters are fixed, disregard presence of NAs as indicator of convergence failure
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    ## if map or start are active, summary/coef is broken
    if (!map_active & !start_active) {

        rx_smry <- summary(rx_glmmtmb)
        rx_coefs_prep <- rx_smry %>% coef() %>% .[["cond"]]
        rx_coefs_prep2 <- adt(rx_coefs_prep)

        ## gof that we get from get_gof from library(modelsummary)
        rx_coefs <- data.frame(vrbl_name = rownames(rx_coefs_prep), coef = rx_coefs_prep2$Estimate,
                               se = rx_coefs_prep2$`Std. Error`, pvalues = rx_coefs_prep2$`Pr(>|z|)`)

        rx_gof <- get_r_gof(rx_glmmtmb, rx_smry)

        
    } else {
        ## if map/start are used, only get minimal information about convergence, which is what matters

        rx_gof <- get_r_gof_min(rx_glmmtmb)
        
        ## set up some minimal rx_coefs so that it fits into overall function
        rx_coefs <- data.frame(vrbl_name = character(), coef = numeric(), se = numeric(), pavlues = numeric())

        
    }
    

    ## conditions to test whether model converged depends on whether map_active == T:
    ## if (map_active), then SEs in rx_coef are not indicator of convergence failure
    if (map_active) {
        cvrgnc_failure <- any(is.na(rx_gof$value))
    } else if (!map_active) {
        cvrgnc_failure <- any(is.na(rx_gof$gof_value)) | any(is.na(rx_coefs))
    }

    if (cvrgnc_failure) {
        ## model did not converge properly

        ret_obj <- list(converged = F, log_likelihood = NA)
    } else  {

        rx_res <- list(coef_df = rx_coefs, gof_df = rx_gof)

        ret_obj <- list(
            converged = T,
            log_likelihood = rx_gof[which(rx_gof$gof_names == "log_likelihood"),"gof_value"],
            res_parsed = rx_res)
    }

    return(ret_obj)
    
}


reg_spec_run_dispatch <- function(iv_vars, dfx, regcmd, dvfmt, fldr_info, technique_str, difficult_str, 
                                  glmmtmb_control, verbose) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run regression with reg_cmd (one of xtnbreg, menbreg or glmmTMB): facilitates time comparison
    #' iv_vars: independent variables
    #' dfx: data frame (filtered down to only have iv_vars)
    #' regcmd: one of xtnbreg, menbreg, glmmTMB
    #' technique/difficulty str: additional options for now implemented only in xtnbreg
    #' verbose: how verbose
    #' glmmtmb_control: control object for glmmTMB_wctrl
    

    if (regcmd == "xtnbreg") {
        
        gof_names <- c("N", "log_likelihood", "N_g", "Chi2", "p", "df")
        
        r_regspec <- run_xtnbreg(iv_vars, dvfmt, gof_names, dfx, technique_str, difficult_str, fldr_info, verbose)
           
    } else if (regcmd == "menbreg") {
        ## separate command for menbreg
        
        gof_names <- c("N", "log_likelihood", "N_g", "Chi2", "p", "df")

        r_regspec <- run_menbreg(iv_vars, dvfmt, gof_names, dfx, fldr_info, verbose)

    } else if (regcmd == "glmmTMB") {
        
        r_regspec <- run_glmmtmb(dfx, dvfmt, iv_vars, verbose)

    } else if (regcmd == "glmmTMB_wctrl") {

        r_regspec <- run_glmmtmb_wctrl(dfx, dvfmt, iv_vars, verbose, glmmtmb_control)
    }

    return(r_regspec)
    
}


## run_vrbl_mdl_vars <- function(mdl_vars, df_cbn, cbn_name, mdl_name, reg_specx) {
run_vrbl_mdl_vars <- function(reg_spec, vvs, fldr_info, return_objs = c("converged"), verbose = F, wtf = T,
                              glmmtmb_control = 1) {
    ## gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' run one regression given the model vars
    #' can also return different stuffs
    #' if wtf (write-to-file) ==F, don't write any output 
    
    ## df_cbn <- cbn_df_dict[[reg_spec$cfg$dvfmt]][[reg_spec$cfg$cbn_name]]
    df_cbn <- get(reg_spec$cfg$dvfmt, cbn_df_dict) %>% get(reg_spec$cfg$cbn_name, .)
    
    df_idx <- reg_spec$df_idx
    other_cfgs <- reg_spec$other_cfgs
    file_id <- reg_spec$mdl_id

    dvfmt <- reg_spec$cfg$dvfmt
    iv_vars <- reg_spec$mdl_vars[reg_spec$mdl_vars %!in% vvs$base_vars]

    ## if looking at rates: yeet the population variable (add always the unscaled one)
    if (dvfmt == "rates") {
        iv_vars <-  keep(iv_vars, ~!grepl("^SP\\.POP\\.TOTLm", .x))
    }
    
    ## always have SP_POP_TOTLm_lag0_uscld so that dvfmt = "rates" can easily add it as offset
    dfx <- select(df_cbn, all_of(c(vvs$base_vars, "iso3c_num", "nbr_opened",iv_vars, "SP_POP_TOTLm_lag0_uscld")))

    ## saving reg spec information to debug later
    ## if (wtf) {saveRDS(reg_spec, file = paste0(fldr_info$REG_SPEC_DIR, file_id))}
    ## write model id to start_file to see which models don't converge
    if (wtf) {write.table(file_id, fldr_info$MDL_START_FILE, append = T, col.names = F, row.names = F)}


    regcmd <- reg_spec$cfg$regcmd
    technique_str <- reg_spec$cfg$technique_str
    if (reg_spec$cfg$difficulty) {difficult_str <- "difficult"} else {difficult_str <- ""}

    t1 <- Sys.time()
    
    r_regspec <- reg_spec_run_dispatch(iv_vars, dfx, regcmd, dvfmt, fldr_info, technique_str, difficult_str,
                                       glmmtmb_control, verbose)
    
    t2 <- Sys.time()
    t_diff <- t2-t1
    
    ## converged <- T
    setwd(PROJECT_DIR)

    ## clean up pid dir for stata-based commands
    if ("pid" %in% names(r_regspec)) {
        proc_dir <- paste0(fldr_info$PID_DIR, r_regspec$pid)
        rmdir_cmd <- paste0("rm -r ", proc_dir)
        system(rmdir_cmd)}
    
    if (!r_regspec$converged) {
        df_idx$cvrgd <- 0 # assign cvrgnc failure to objects
        other_cfgs$cvrgd <- 0
        print("convergence failed")
                    
    } else {
        
        if (wtf) {save_parsed_res(r_regspec$res_parsed, file_id, fldr_info)}

        df_idx$cvrgd <- 1
        ## add time passed to other_cfgs
        other_cfgs <- rbind(other_cfgs, data.frame(variable = "t_diff", value = difftime(t2, t1, units = "secs"),
                                                   cfg_id = other_cfgs$cfg_id[1], lag_spec = other_cfgs$lag_spec[1],
                                                   mdl_id = other_cfgs$mdl_id[1]))
        other_cfgs$cvrgd <- 1
    }

    ## save id to df_id to keep track
    if (wtf) {write.table(df_idx, file = fldr_info$REG_RES_FILE_LAGS, append = T, col.names = F, row.names = F)}

    pid <- Sys.getpid()
    if (wtf) {write.table(other_cfgs, file = paste0(fldr_info$REG_RES_FILE_CFGS, pid),
                          append = T, col.names = F, row.names = F)}

    ## write model id to end file to debug convergence failure
    if (wtf) {write.table(file_id, fldr_info$MDL_END_FILE, append = T, col.names = F, row.names = F)}

    return(r_regspec[return_objs])
    
}

## run_cbn <- function(cbn_vars, base_vars, ctrl_vars, cbn_name, reg_spec) {

proc_mdl_info_addgn <- function(reg_spec, mdl_name, mdl_vars) {
    #' add the model information to the new cfg sublist 

    reg_spec$cfg <- c(reg_spec$cfg,
                      list(mdl_name = mdl_name))

    reg_spec$mdl_vars <- mdl_vars
    return(reg_spec)
}
    


gen_spec_mdl_info <- function(reg_spec) {
    #' generate the model specifications (currently just has full models)
    
    
    ## print(paste0("run cbn ", which(names(vrbl_cbns) == reg_spec$cbn_name)))

    ## df_cbn <- cbn_dfs[[reg_spec$cbn_name]]

    ## generate the models
    ## cbn_models <- gen_cbn_models(cbn_vars, base_vars, ctrl_vars)

    cbn_models <- gen_cbn_models(reg_spec$spec_cbn, reg_spec$base_vars, reg_spec$ctrl_vars)
    
     
    ## lapply(names(cbn_models), \(x) run_vrbl_mdl_vars(cbn_models[[x]], df_cbn, cbn_name, mdl_name = x, reg_spec))

    ## specs_mod <- lapply(names(cbn_models), \(x) c(reg_spec,
    ##                                               list(mdl_name = x, mdl_vars = cbn_models[[x]])))
 
    specs_mod <- lapply(names(cbn_models), \(x)
                        proc_mdl_info_addgn(reg_spec, mdl_name = x, mdl_vars = cbn_models[[x]]))
 
   
    ## %>%
    ## rbindlist() %>% atb()
    
    return(specs_mod)

}

proc_mdl_technique_addgs <- function(reg_spec, technique_str) {
    #' add the optimization technique to the model 
    reg_spec$cfg <- c(reg_spec$cfg,
                      list(technique_str = technique_str))
    return(reg_spec)
}


gen_spec_technique_info <- function(reg_spec, technique_strs) {
    #' add technique string to model spec
    
    reg_spec_techniques_added <- lapply(technique_strs, \(x) proc_mdl_technique_addgs(reg_spec, x))
    return(reg_spec_techniques_added)

}

proc_mdl_difficulty_addgs <- function(reg_spec, difficulty) {
    #' add the difficulty to the model 
    reg_spec$cfg <- c(reg_spec$cfg,
                      list(difficulty = difficulty))
    return(reg_spec)
}


gen_spec_difficulty_info <- function(reg_spec, difficulty_switches) {
    #' add difficulty switch to model spec
    
    reg_spec_difficulty_added <- lapply(difficulty_switches, \(x) proc_mdl_difficulty_addgs(reg_spec, x))
    return(reg_spec_difficulty_added)

}


proc_mdl_regcmd_addgns <- function(reg_spec, regcmd) {
    #' add the regression command to model 
    reg_spec$cfg <- c(reg_spec$cfg,
                      list(regcmd = regcmd))
    return(reg_spec)
}

gen_spec_regcmd_info <- function(reg_spec, regcmds) {
    #' add regression commands to model spe

    reg_spec_regcmd_added <- lapply(regcmds, \(x) proc_mdl_regcmd_addgns(reg_spec, x))
    return(reg_spec_regcmd_added)
}

proc_mdl_dvfmt_addgns <- function(reg_spec, dvfmt) {
    #' add the dependent variable format to model 
    reg_spec$cfg <- c(reg_spec$cfg,
                      list(dvfmt = dvfmt))
    return(reg_spec)
}

gen_spec_dvfmt_info <- function(reg_spec, dvfmts) {
    #' add dependent variable format to model spec

    reg_spec_dvfmt_added <- lapply(dvfmts, \(x) proc_mdl_dvfmt_addgns(reg_spec, x))
    return(reg_spec_dvfmt_added)
}



gen_spec_id_info <- function(reg_spec, vvs) {
    #' add the id information to a reg_spec

    id_stuff <- gen_mdl_id(reg_spec, vvs)

    reg_spec_with_id <- c(reg_spec, id_stuff)

    return(reg_spec_with_id)
}

    
proc_cbn_info_addgn <- function(reg_spec, spec_cbn, base_vars, ctrl_vars, cbn_name, vvs) {
    #' modify the reg_spec cbn_info, adding cbn_name to the cfg sub_list
    #' since not possible to assign in lambdas
    
    reg_spec$cfg <- c(reg_spec$cfg,
                      list(cbn_name = cbn_name))

    reg_spec$base_vars <- vvs$base_vars
    reg_spec$spec_cbn <- spec_cbn
    reg_spec$ctrl_vars <- ctrl_vars

    return(reg_spec)
}
  


gen_spec_cbn_info <- function(reg_spec, cbns_to_include, vvs) {
    #' vary a reg-spec across variable combinations -> exclude variables not belonging to a particular combination
    
    

    spec_vars <- apply(reg_spec$lngtd_vrbls, 1, \(x) paste0(x[["vrbl"]], "_lag", x[["lag"]]))

    rel_vars_spec <- c(vvs$base_vars, vvs$crscn_vars, spec_vars)

    spec_cbns <- gen_cbns(rel_vars_spec, vvs$base_vars)

    
    ## generate specification-specific control vars
    ctrl_vars <- setdiff(spec_cbns$cbn_controls, vvs$base_vars)

    ## spec_cbn_names <- names(spec_cbns)
    ## names(spec_cbn_names) <- spec_cbn_names

    


    ## ## only generate the models for cbns_to_include
    ## reg_specs_mod <- lapply(cbns_to_include, \(i) c(reg_spec,
    ##                                                list(spec_cbn=spec_cbns[[i]],
    ##                                                     base_vars = vvs$base_vars,
    ##                                                     ctrl_vars = ctrl_vars,
    ##                                                     cbn_name = i)))

    reg_specs_mod <- lapply(cbns_to_include, \(i)
                            proc_cbn_info_addgn(reg_spec, spec_cbns[[i]], base_vars, ctrl_vars, cbn_name = i, vvs))

   
    ## lapply(spec_cbn_names, \(x) run_cbn(spec_cbns[[x]], base_vars, ctrl_vars, x, reg_spec))
    ## for (i in spec_cbn_names) {

            
    ##     run_cbn(spec_cbns[[i]], base_vars, ctrl_vars, i, reg_spec)


    ##     print(paste0(i, " end"))
    ## }
    return(reg_specs_mod)

}

replace_vlue <- function(lngtd_vrbls, vrbl, lag) {
    #' replace value more effectively in varying the lag length
    
    

    lngtd_vrbls[which(lngtd_vrbls$vrbl == vrbl), "lag"] <- lag

    ## make sure interaction always has same lag
    lngtd_vrbls[which(lngtd_vrbls$vrbl == "ti_tmitr_interact"),]$lag <-
        lngtd_vrbls[lngtd_vrbls$vrbl == "tmitr_approx_linear20step", ]$lag
    
    return(lngtd_vrbls)
}



vary_spec <- function(reg_spec, vvs, vary_vrbl_lag, lags){
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' for a spec, vary each variable along all lags 
    
    
    base_lag_spec <- paste0(gen_lag_id(reg_spec, vvs)$value, collapse = "")

    ## if I want to vary the variable lags, do it here
    if (vary_vrbl_lag) {

        stop("let's hope this branch is no longer used (switched to better constraining process)")

        ## don't vary the ti_tmitr interaction 
        varied_specs <-
            lapply(reg_spec[["lngtd_vrbls"]]$vrbl[reg_spec[["lngtd_vrbls"]]$vrbl != "ti_tmitr_interact"], \(x)
                   lapply(lags, \(t)
                          list(
                              ## "lngtd_vrbls" = mutate(reg_spec[["lngtd_vrbls"]],
                              ##                         lag = ifelse(vrbl == x, t, lag)),
                              "lngtd_vrbls" = replace_vlue(reg_spec$lngtd_vrbls, vrbl = x, lag = t),
                              "lag_len" = t,
                              "cfg" = list("vrbl_varied" = x,
                                           "base_lag_spec" = base_lag_spec
                                           )
                          )
                          )
                   )
        varied_specs_long <- Reduce(\(x, y) c(x,y), varied_specs) %>% unique()

        ## if i don't want to vary variable lag, still add base_lag_spec
    } else {
        
        ## reg_spec$lngtd_vrbls[which(reg_spec$lngtd_vrbls$vrbl == "ti_tmitr_interact"),]$lag <-
        ##     reg_spec$lngtd_vrbls[reg_spec$lngtd_vrbls$vrbl == "tmitr_approx_linear20step", ]$lag

        ## hopefully more functional constrainment of squares/interactions

        reg_spec$lngtd_vrbls <- cstrn_vrbl_lags(reg_spec$lngtd_vrbls)
        

        varied_specs_long <- list(c(reg_spec,
                               list("base_lag_spec" = base_lag_spec)
                               ))
        
    }


    ## varied_specs_long <- varied_specs

    return (varied_specs_long)
}






setup_regression_folders_and_files <- function(batch_version, batch_dir_addgn = "") {
    #' setup folders and files for running regressions
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    

    ## batch_version <- "v20"
    REG_MONKEY_DIR <- "/home/johannes/reg_res/"
    BATCH_DIR <- paste0(REG_MONKEY_DIR, batch_dir_addgn, batch_version, "/")

    REG_RES_DIR <- paste0(BATCH_DIR, "reg_res/")
    REG_RES_FILE_LAGS <- paste0(BATCH_DIR, batch_version, "_lags.csv")
    REG_RES_FILE_CFGS <- paste0(BATCH_DIR, batch_version, "_cfgs.csv")
    REG_SPEC_DIR <- paste0(BATCH_DIR, "specs/")
    MDL_START_FILE <- paste0(BATCH_DIR, batch_version, "_start.csv")
    MDL_END_FILE <- paste0(BATCH_DIR, batch_version, "_end.csv")
    ## generate existing dirs: have to normalizePath (remove //), and paste additional "/" at the end reeee
    existing_batch_dirs <- paste0(normalizePath(list.dirs(REG_MONKEY_DIR, recursive = F)), "/")
    
    if (BATCH_DIR %!in% existing_batch_dirs) { system(paste0("mkdir ", BATCH_DIR))}
    existing_dirs <- paste0(normalizePath(list.dirs(BATCH_DIR, recursive = F)), "/")

    if (REG_RES_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_RES_DIR))}
    if (REG_SPEC_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_SPEC_DIR))}
    PID_DIR <- "/home/johannes/pid_dir/"

    return(list(
        batch_version = batch_version,
        BATCH_DIR = BATCH_DIR,
        REG_MONKEY_DIR = REG_MONKEY_DIR,
        REG_RES_DIR = REG_RES_DIR,
        REG_RES_FILE_LAGS = REG_RES_FILE_LAGS,
        REG_RES_FILE_CFGS = REG_RES_FILE_CFGS,
        REG_SPEC_DIR = REG_SPEC_DIR,
        MDL_START_FILE = MDL_START_FILE,
        MDL_END_FILE = MDL_END_FILE,
        PID_DIR = PID_DIR
    )
    )
}

get_vrblchoice_id <- function(lngtd_vrbls) {
    #' get the vrblchoice_id from lngtd_vrbls
   
    dt_lngtd_vrbls <- adt(lngtd_vrbls)
    hnwi_vrbl <- dt_lngtd_vrbls[grepl("hnwi_nbr", vrbl), vrbl]
    ii_vrbl <- dt_lngtd_vrbls[grepl("ptinc992j", vrbl), vrbl]
    wi_vrbl <- dt_lngtd_vrbls[grepl("hweal992j", vrbl), vrbl]
    ## print(hnwi_vrbl)
    ## print(ii_vrbl)
    ## print(wi_vrbl)
    vrbl_thld_choices[hnwi_var == hnwi_vrbl & inc_ineq_var == ii_vrbl & weal_ineq_var == wi_vrbl, 
                      vrblchoice_id]
}

gen_batch_reg_specs <- function(reg_settings, vvs, vrbl_thld_choices) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the regression specs for a batch 

    
    
    
    ## generate basic spec of lag, variable and threshold choices
    t1 = Sys.time()
    ## NBR_SPECS <- 750


    reg_specs <- lapply(seq(1, nrow(vrbl_thld_choices)), \(x)
                        lapply(seq(1, reg_settings$nbr_specs_per_thld), \(i)
                               gen_reg_spec(vvs$non_thld_lngtd_vars, vrbl_thld_choices[x,], reg_settings))) %>%
        flatten()

    ## reg_specs <- lapply(seq(1,reg_settings$nbr_specs_per_thld), \(x)
    ##                     gen_reg_spec(vvs$non_thld_lngtd_vars, vrbl_thld_choices)) %>%
    ##     unique() #
    
    ## get_vrbl_choice_cd(reg_specs[[3]]$lngtd_vrbls)


    reg_spec_mdls <- vary_batch_reg_spec(reg_specs, reg_settings, vvs)
    ## reg_spec_mdls[[1]]
    
    ## bolt base_reg_spec and vrbl_choice on there 
    reg_spec_mdls2 <- map(reg_spec_mdls,
                          ~`pluck<-`(.x, "cfg", ## extend existing cfg by overwriting
                                     value = c(.x$cfg, # add to cfg base_lag_spec and vrbl_choice
                                               list(base_lag_spec = .x$base_lag_spec,
                                                    vrbl_choice = get_vrblchoice_id(.x$lngtd_vrbls)
                                                    )))) %>%
        map(~.x[names(.x) != "base_lag_spec"]) ## yeet base_lag_spec from top layer (only have it in cfg)

    ## reg_spec_mdls2[[3]]

    ## enumerate the variable choices by combination
    dt_vrbl_choice <- data.table(
        vrbl_choice = map_chr(reg_spec_mdls2, ~chuck(.x, "cfg", "vrbl_choice")),
        cbn_name = map_chr(reg_spec_mdls2, ~chuck(.x, "cfg", "cbn_name"))) %>%
        .[, nbr := 1:.N, .(vrbl_choice, cbn_name)]
    
        
    ## assign vrbl choice combination number to cfg
    reg_spec_mdls3 <- map2(reg_spec_mdls2, dt_vrbl_choice$nbr,
                           ~`pluck<-`(.x, "cfg", "vrbl_choice_cbn_nbr", value =  .y))

    ## reg_spec_mdls3[[303]]

    
    reg_spec_mdls_with_ids <- idfy_reg_specs(reg_spec_mdls3, vvs)
    
    ## reg_spec_mdls3[[3]]
    ## reg_spec_mdls_with_ids[[3]]
    ## nchar(reg_spec_mdls_with_ids[[3]]$mdl_id)

    
    t2 = Sys.time()
    print(t2-t1)


    return(reg_spec_mdls_with_ids)
}




vary_batch_reg_spec <- function(reg_specs, reg_settings, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate variations (lags, combinations, models) of list of reg_specs, also add ID

    

    ## generate variations of basic reg_spec
    reg_spec_varyns <- mclapply(reg_specs, \(x)
                                vary_spec(x, vvs, reg_settings$vary_vrbl_lag, reg_settings$lags),
                                mc.cores = 6) %>% flatten()

    ## vary_spec(reg_specs[[1]], vvs, reg_settings$vary_vrbl_lag, reg_settings$lags)
    ## vary_spec(reg_specs[[3]], vvs, T, c(1,2,3,4))


    
    ## reg_spec_varyns2 <- mclapply(reg_specs, vary_spec, mc.cores = 6) %>% Reduce(\(x,y) c(x,y), .)
    ## add the combination info
    reg_spec_cbns <- mclapply(reg_spec_varyns, \(x)
                              gen_spec_cbn_info(x, reg_settings$cbns_to_include, vvs),
                              mc.cores = 6) %>%
        flatten()

    ## map(reg_spec_cbns, ~.x$cfg$cbn_name) %>% unlist() %>% table()
    

    ## reg_spec_cbns <- lapply(reg_spec_varyns, \(x) gen_spec_cbn_info(x, vvs$base_vars)) %>% flatten()
    ## reg_spec_cbns <- sapply(reg_spec_varyns, \(x) gen_spec_cbn_info(x, base_vars))
    ## mclapply is actually slower here because Reduce() is needed, and reducing tens of thousands of lists single-threadedly is slower than using sapply on single core
    ## guess I could split reg_spec_varyns manually into sections, each called with sapply, overall with mclapply: then I have just mc.cores number of lists, so Reduce should be quick
    ## flatten() from purrr is actually faster lol 

    ## add the model info
    ## reg_spec_mdls <- sapply(reg_spec_cbns, gen_spec_mdl_info)
    
    
    reg_spec_mdls <- mclapply(reg_spec_cbns, gen_spec_mdl_info, mc.cores = 6) %>% flatten()
    ## same issue here: mclapply with Reduce is slow, but with flatten it's faster :)))

    reg_spec_techniques <- mclapply(reg_spec_mdls, \(x)
                                    gen_spec_technique_info(x, reg_settings$technique_strs),
                                    mc.cores = 6) %>% flatten()
    ## gen_spec_technique_info(reg_spec_mdls[[1]])

    reg_spec_difficulties <- mclapply(reg_spec_techniques, \(x)
                                      gen_spec_difficulty_info(x, reg_settings$difficulty_switches),
                                      mc.cores = 6) %>% flatten()

    reg_spec_regcmds <- mclapply(reg_spec_difficulties, \(x)
                                 gen_spec_regcmd_info(x, reg_settings$regcmds),
                                 mc.cores = 6) %>% flatten()

    reg_spec_dvfmts <- mclapply(reg_spec_regcmds, \(x)
                                gen_spec_dvfmt_info(x, reg_settings$dvfmts),
                                mc.cores = 6) %>% flatten()


    return(reg_spec_dvfmts)

}
    
idfy_reg_specs <- function(reg_spec_mdls, vvs){
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' add ids to reg_specs 
    
    reg_spec_mdls_with_ids <- mclapply(reg_spec_mdls, \(x) gen_spec_id_info(x, vvs), mc.cores = 6)
    
    ## gen_spec_id_info(reg_spec_mdls[[1]], vvs)


    return(reg_spec_mdls_with_ids)

}



get_reg_spec_from_id <- function(mdl_id, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    reg_spec <- readRDS(paste0(fldr_info$REG_SPEC_DIR, mdl_id))
    return(reg_spec)
}




## * step-wise optimizer

## ** functions 


## reg_spec <- reg_spec_mdls_optmz[[1]]

## restore_base_lag_spec <- 
modfy_optmz_cfg <- function(reg_spec, cfg_orig, base_lag_spec_orig, loop_nbr, vrblx) {
    #' restore the base_lag_spec in a reg_spec, also add other things to config: loop nbr, variable varied

    ## reg_spec$other_cfgs <- reg_spec$other_cfgs %>%
    ##     mutate(value = ifelse(variable == "base_lag_spec", base_lag_spec_orig, value))
    reg_spec$cfg <- cfg_orig

    reg_spec$cfg$base_lag_spec <- base_lag_spec_orig

    reg_spec$cfg$loop_nbr <- loop_nbr
    reg_spec$cfg$vrbl_optmzd <- vrblx

    return(reg_spec)
}

gl_regspecs_wid <- function(reg_spec, reg_settings, vrblx, loop_nbr) {
    #' generate the lagspecs corresponding to the lags: constrain variables, update lag_specs/mdl_vars/ IDs
    
    
    ## only pick the lngtd_vrbls, and vary vrblx
    reg_specs_vrblx_varied <- lapply(reg_settings$lags, \(x)
                                     reg_spec$lngtd_vrbls %>% 
                                     mutate(lag = ifelse(vrbl==vrblx, x, lag)) %>%
                                     cstrn_vrbl_lags() %>%  # constrain lags here already
                                     list(lngtd_vrbls = .))
    
    stuff_to_keep <- c("cfg", "base_vars", "mdl_vars") # "ctrl_vars", "mdl_vars")


    ## more efficient way of modifying lags (just replace lngtd_vrbls in passed regspec): 0.001 secs
    reg_specs_full_again <- lapply(reg_specs_vrblx_varied, \(x)
                                   `pluck<-`(copy(reg_spec)[stuff_to_keep], "lngtd_vrbls",
                                             value = x$lngtd_vrbls)) %>%
        lapply(., \(x) # redo the base lag spec
               `pluck<-`(x, "cfg", "lag_spec", value = paste0(gen_lag_id(x, vvs)$value, collapse = ""))) %>%
        lapply(., \(x) ## modify mdl_vars
               `pluck<-`(x, "mdl_vars",
                         ## keep cross-sectional variables
                         value = c(keep(x$mdl_vars, ~!grepl("_lag[1-5]", .x)),
                                   ## generate lagged variables based on (constrained) lngtd_vrbls
                                   adt(x$lngtd_vrbls)[vrbl %in% gsub("_lag[1-5]", "", x$mdl_vars)][
                                     , paste0(vrbl, "_lag", lag)])))
    
    ## map(reg_specs_full_again, ~chuck(.x, "mdl_vars")) ## check them    
    
    
    ## modify base_lag_spec back: effectively add loop_nbr and vrblx
    reg_specs_full_again2 <- map(reg_specs_full_again,
                                 ~`pluck<-`(.x, "cfg", value = c(.x$cfg, 
                                                                 list(loop_nbr = loop_nbr,
                                                                      vrblx = vrblx))))

    
    ## add ids: use gen_spec_id_info directly to save mclapply spinup
    reg_specs_w_ids <- map(reg_specs_full_again2, ~gen_spec_id_info(.x, vvs))
    ## reg_specs_w_ids[[3]]
    return(reg_specs_w_ids)

    ## identical(reg_specs_w_ids, reg_specs_w_ids2) ## comparison with old version -> noice
}


gd_presence <- function(reg_specs_w_ids, db_str, vrblx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' see which models have already been run
    #' @param reg_specs_w_ids list of regspecs with IDs, lngtd_vrbls adjusted etc
    #' @param db_str string of sqlite-db with where run models are saved to
    #' @param vrblx the variable being optimized

    
    ## db_mdlcache <- dbConnect(RSQLite::SQLite(), db_str)
    
    dt_lagspecs <- map(reg_specs_w_ids, ~list(cbn_name = chuck(.x, "cfg", "cbn_name"),
                                               lag_spec = chuck(.x, "cfg", "lag_spec"),
                                               loop_nbr = chuck(.x, "cfg", "loop_nbr"),
                                               mdl_id = chuck(.x, "mdl_id"))) %>% rbindlist
    

    
    ## get all the models that are already in cache
    ## if something isn't here, it should be estimated
    cmd_mdlcache <- paste0("lag_spec = ", "'", dt_lagspecs[, lag_spec], "'", collapse = " OR ") %>%
        sprintf("SELECT cbn_name, lag_spec, ll from mdl_cache where (%s) AND cbn_name = '%s'", .,
                dt_lagspecs[1, "cbn_name"])

    db_mdlcache <- gb_mdlcache_locked(db_str, lock = F)

    ## get basic mdlcache information
    dt_mdlcache_raw <- dbGetQuery(db_mdlcache, cmd_mdlcache) %>% adt

    ## dbExecute(db_mdlcache, "COMMIT") # unlock DB 
    dbDisconnect(db_mdlcache) # disconnect to see if that fixes db lock errors.. 


    ## drop models that have been run by different processes
    dt_mdlcache <- dt_mdlcache_raw %>% .[, .(cbn_name, lag_spec, ll)] %>% funique

    ## check if same model produces always same result
    if (dt_mdlcache[, .N, lag_spec][, max(N)] > 1) {print("same model is converging differently")}

    

    

    ## if model was tried but didn't converge, it now has ll == 10000
    dt_mdlcache[is.na(ll), ll := 10000]


    ## saveRDS(reg_spec, file = paste0(fldr_info$REG_SPEC_DIR, file_id))}
    
    dt_presence <- join(dt_lagspecs, dt_mdlcache, on = .c(lag_spec, cbn_name), verbose = 0) %>% 
        .[, missing_before := is.na(ll)] %>% # get those that are missing to save later
        .[, ll := as.numeric(ll)] %>% ## need to ensure that LLs are numeric (otherwise boolean)
        .[ll > 0, ll := NA] %>% # reset LL of convergence failures AFTER setting missing_before
        .[, vrbl_optmzd := vrblx]


    ## dt_presence <- map(l_lagquery_res2,
    ##                    ~.x[c("cbn_name", "lag_spec", "ll", "mdl_id", "loop_nbr")]) %>% rbindlist() %>%
    ##     .[, missing_before := is.na(ll)] %>% # get those that are missing to save later
    ##     .[, ll := as.numeric(ll)] %>% ## need to ensure that LLs are numeric (otherwise boolean) 
    ##     .[, vrbl_optmzd := vrblx]

    return(dt_presence)
}

gb_mdlcache_locked <- function(db_str, lock) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate connetion to sqlite DB that can be written to: is
    #' @db_str: location of DB
    #' @lock: whether connection should be locked: should probably be T when writing
 
    ## first try: if db is locked, it will produce warning
    ## db_mdlcache <- dbConnect(RSQLite::SQLite(), db_str)

    ## check that nobody else is writing to db
    ## while (is(db_mdlcache, "warning")) {
    while (T) {
                
        db_mdlcache <- tryCatch(dbConnect(RSQLite::SQLite(), db_str),
                                warning = function(w) w)

        if (!is(db_mdlcache, "warning")) {
            break
        } else {
            time_to_sleep <- runif(1, 0.1, 0.3)
            print(sprintf("connection failed, now sleeping for %s seconds", round(time_to_sleep, 2)))
            Sys.sleep(time_to_sleep) # in case there is (and db_mdlcache is a warning) try again later
        }
        
    }

    ## if (lock) {
    ##     ## after exiting the loop, set it exclusive
    ##     dbExecute(db_mdlcache, "BEGIN EXCLUSIVE")
    ## }

    dbExecute(conn = db_mdlcache, "PRAGMA foreign_keys=ON")

    return(db_mdlcache)
}


w_lagoptim <- function(reg_settings, dt_presence, db_str) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## write model results to db cache for later easy access

    if (reg_settings$wtf) {

        
        db_mdlcache <- gb_mdlcache_locked(db_str, lock = F)        

        ## check that current results don't violate the sqlite constraints
        

        ## check which cbn-lagspecs are now there (maybe another process has already run the same results)
        ## cmd_presence_b4_writing <- paste0("lag_spec = ", "'",
        ##                                   dt_presence[, lag_spec], "'", collapse = " OR ") %>% 
        ##     sprintf("SELECT cbn_name, lag_spec, ll from mdl_cache where (%s) AND cbn_name = '%s'" , .,
        ##             dt_presence[1, cbn_name])

        ## dt_presence_b4_writing <- dbGetQuery(db_mdlcache, cmd_presence_b4_writing) %>% adt
    
        ## remove those that have already been run elsewhere
        ## dt_to_write <- dt_presence[missing_before == T, .(cbn_name, lag_spec, ll)] %>%
        ##     .[!dt_presence_b4_writing, on = .(cbn_name, lag_spec)]

        dt_to_write <- dt_presence[missing_before == T, .(cbn_name, lag_spec, ll, proc_pid = Sys.getpid())]
        

        ## print DB status
        print(sprintf("nrow table mdl_cache: %s", dbGetQuery(db_mdlcache, "select count(*) from mdl_cache")))
        print(sprintf("nrow table mdl_lag: %s", dbGetQuery(db_mdlcache, "select count(*) from mdl_log")))
        
        ## actually write to DB 
        ## write LL back to file
        dbAppendTable(db_mdlcache, "mdl_cache", dt_to_write)

        ## write log of all the mdls that are run (either directly or indirectly)
        dbAppendTable(db_mdlcache, "mdl_log", dt_presence[, .(mdl_id, cbn_name, lag_spec, loop_nbr, vrbl_optmzd)])

        ## dbExecute(db_mdlcache, "COMMIT") # unlock DB 

        dbDisconnect(db_mdlcache) # close connection

        ## TEST: only write some back
        ## dbAppendTable(db_mdlcache, "mdl_cache", dt_presence[c(2,4), .(cbn_name, lag_spec, ll)])
    }
}

m_regspec_after_lagoptim <- function(reg_spec, vrblx, dt_presence, best_lag) {
    ## modify regspec after lags of a variable have been optimized


    ## keep track of how many models were run 
    reg_spec$nbr_mdls_run <- dt_presence[, sum(missing_before == T)]

    
    ## assign best_lag value back to reg_spec 
    reg_spec$lngtd_vrbls <- reg_spec$lngtd_vrbls %>%
        mutate(lag = ifelse(vrbl == vrblx, best_lag, lag))

    
    ## adjust the lag of ti_tmitr interaction to tmitr
    reg_spec$lngtd_vrbls <- cstrn_vrbl_lags(reg_spec$lngtd_vrbls)

    return(reg_spec)
}


optmz_vrbl_lag <- function(reg_spec, vrblx, loop_nbr, fldr_info, reg_settings,
                           glmmtmb_control, return_obj = "reg_spec_optimd",                           
                           verbose = F) {
    #' optimize the lag of one variable (ususally lag varies from 1 to 5)
    #' @param reg_spec regression specification objects
    #' @vrblx variable to optimize
    #' @fldr_info, @reg_settings technical aspects
    #' @glmmtmb_control additional arguments to pass to glmmtmb_wctrl when reg_spec$cfg$regcmd == "glmmTMB_wctrl"
    #' @return_obj which object to return: by default return the modified reg_spec, but also possible to return
    #' the dt_presence to investigate lag optimization
        
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' optimize the lag of one variable 

    if (verbose) { print(paste0("vrblx: ", vrblx))}

    ## prepare regspecs of lags: adjust constrained variables, add IDs etc
    reg_specs_w_ids <- gl_regspecs_wid(reg_spec, reg_settings, vrblx, loop_nbr) 
    
    if (reg_settings$wtf) {map(reg_specs_w_ids, ~saveRDS(.x, file = paste0(fldr_info$REG_SPEC_DIR, .x$mdl_id)))}

    ## see which models are already there
    db_str <- paste0(fldr_info$BATCH_DIR, "mdl_cache.sqlite")
    dt_presence <- gd_presence(reg_specs_w_ids, db_str, vrblx)
    
    print(sprintf("%s lags are already there", dt_presence[missing_before == F, .N]))

    ## actually run the models
    ## dt_res <- lapply(reg_specs_w_ids[dt_presence[, is.na(ll)]], \(x)
    dt_res <- lapply(reg_specs_w_ids[dt_presence[, missing_before == T]], \(x)
                      run_vrbl_mdl_vars(x, vvs, fldr_info, return_objs = c("log_likelihood", "converged"),
                                        glmmtmb_control = glmmtmb_control,
                                        wtf = reg_settings$wtf)) %>% rbindlist

    ## assign results to dt_presence
    dt_presence[missing_before == T, ll := dt_res$log_likelihood]
    
    
    ## pick best lag
    if (dt_presence[, all(is.na(ll))]) {
        best_lag <- sample(reg_settings$lags, 1)
        print("all lags of current model result in non-convergence, pick lag at random")
    } else {
        best_lag <- reg_settings$lags[which.max(dt_presence$ll)] 
    }
    
    ## save results
    w_lagoptim(reg_settings, dt_presence, db_str)
    

    reg_spec_optimd <- m_regspec_after_lagoptim(reg_spec, vrblx, dt_presence, best_lag)

    return(get(return_obj))
    ## return(reg_spec_optimd)
    
}



## optmz_vrbl_lag(reg_spec_mdls_optmz[[2]], "nbr_opened_cum", loop_nbr = 1, fldr_info_optmz)



optmz_reg_spec_once <- function(reg_spec, loop_nbr, vrbls_to_vary, fldr_info, reg_settings) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' one round of optimization
    #' need for loop, because nbr_skipped_in_row has to be reassinged
    
    ## just assign some random variable to v for debugging
    v <- sample(vrbls_to_vary)[1]


    for (v in sample(vrbls_to_vary)) {
        print(v)
        
        ## cur_lag_id <- gen_lag_id(reg_spec, vvs) %>% pull(value) %>% paste0(collapse = "")

        ## if variable hasn't run yet with current lag spec, run it now 
        ## if (cur_lag_id %!in% reg_spec$run_lag_specs[[v]]) {

        ## t1 = Sys.time()
        reg_spec <- optmz_vrbl_lag(reg_spec, v, loop_nbr, fldr_info, reg_settings)
        ## t2 = Sys.time()
        
        ## print(sprintf("time on entire optimization process once: %s", t2-t1))

## reg_spec2 <- optmz_vrbl_lag(reg_spec, v, loop_nbr, fldr_info, verbose = T)
        ## reg_spec$run_lag_specs[[v]] <- c(reg_spec$run_lag_specs[[v]], cur_lag_id)

        if (reg_spec$nbr_mdls_run > 0) {
            reg_spec$nbr_skipped_in_row <- 0
        } else {

            reg_spec$nbr_skipped_in_row <- reg_spec$nbr_skipped_in_row + 1
        }
        

    }
    
    return(reg_spec)
}

## optmz_reg_spec_once(reg_spec_mdls_optmz[[2]], 1, fldr_info_optmz)



optmz_reg_spec <- function(reg_spec, fldr_info, reg_settings) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' optimize a regression specification by randomly choosing a variable and then picking the best lag
    
    
    ## add lists of lag_specs already run for each variable 
    
    ## reg_spec_vrbls <- reg_spec$lngtd_vrbls$vrbl
    ## names(reg_spec_vrbls) <- reg_spec_vrbls
    ## reg_spec$run_lag_specs <- lapply(reg_spec_vrbls, \(x) c())

    ## add counter of how many regs have been skipped 
    reg_spec$nbr_skipped_in_row <- 0
    l <- 0

    ## select the variables that need to be optimized: skip the ones to be constrained 
    ## setdiff(adt(reg_spec$lngtd_vrbls)[, vrbl], vvs$dt_cstrnd_vrbls[, cstrnd])
    vrbls_to_yeet <- vvs$dt_cstrnd_vrbls[, cstrnd] # exclude variables whose lag is determined by others
    if (reg_spec$cfg$dvfmt == "rates") {
        vrbls_to_yeet <- c(vrbls_to_yeet,
                           "SP.POP.TOTLm", # exclude population 
                           ## also exclude variables not used in that combination
                           setdiff(reg_spec$lngtd_vrbls$vrbl, chuck(vrbl_cbns, reg_spec$cfg$cbn_name))
                           )
    }
    
    vrbls_to_vary <- setdiff(reg_spec$lngtd_vrbls$vrbl, vrbls_to_yeet)

    
    ## run until no more improvement 
    while (T) {
        
        reg_spec <- optmz_reg_spec_once(reg_spec, loop_nbr = l, vrbls_to_vary, fldr_info, reg_settings)

        if (reg_spec$nbr_skipped_in_row > (len(vrbls_to_vary)*len(reg_settings$lags))+1) {break}
        l <- l+1

    }
    return (reg_spec)
}

## ** one out analysis

modfy_regspec_ou <- function(reg_spec, vrblset_oud, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' modify reg_spec for one-out calculations:
    #' one-out one/multiple variable(s) (oud = one-out-ed),
    #' usually just drop one, but also allow option to drop multiple for interactions/squared variable handling
    
    ## reg_spec
    
    ## vrbl_oud <- "hnwi_nbr_1M_lag1"
    
    ou_set_title <- names(vrblset_oud)
    vrbls_oud <- vrblset_oud[[ou_set_title]]
    

    reg_spec_mod <- reg_spec
    
    reg_spec_mod$mdl_vars <- setdiff(reg_spec_mod$mdl_vars, vrbls_oud)

    ## add ou set title and original ID to cfg
    reg_spec_mod$cfg <- c(reg_spec_mod$cfg,
                          list(ou_set_title = ou_set_title))
                               
    
    id_updtd <- gen_mdl_id(reg_spec_mod, vvs)
    ## id_updtd$mdl_id
    ## nchar(id_updtd$mdl_id)

    ## add ID of original reg_spec (all variables): nonou = non-one-out

    other_cfgs_w_nonou_id <- rbind(id_updtd$other_cfgs,
                                   data.frame(variable = "nonou_id",
                                              value = reg_spec$mdl_id,
                                              cfg_id = id_updtd$other_cfgs$cfg_id[1],
                                              lag_spec = id_updtd$other_cfgs$lag_spec[1],
                                              mdl_id = id_updtd$other_cfgs$mdl_id[1]))

    reg_spec_mod$other_cfgs <- other_cfgs_w_nonou_id
    reg_spec_mod$mdl_id <- id_updtd$mdl_id
    reg_spec_mod$df_idx <- id_updtd$df_idx

    return(reg_spec_mod)
   
}

## modfy_regspec_ou(reg_specx, vrbl_oud = "hnwi_nbr_1M_lag4", vvs)

gen_vrbl_oud <- function(vrbl, vrbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' generate the variables to exclude

    vrbl_unlag <- gsub("_lag[1-5]", "", vrbl)

    ## if vrbl is about tax incentives, also exclude the interaction
    if (vrbl == "Ind.tax.incentives" | vrbl_unlag == "tmitr_approx_linear20step") {
        oud <- c(vrbl, keep(vrbls, ~grepl("ti_tmitr_interact", .x)))
    }

    ## matching for squared variables
    ## if there's a version with the squared information, drop that too 
    else if (any(grepl(paste0(vrbl_unlag, "_sqrd"), vrbls))) {
        oud <- c(vrbl, keep(vrbls, ~grepl(paste0(vrbl_unlag, "_sqrd"), .x)))
    }
    else {
        oud <- vrbl
    }
    
    return(oud)
}

gen_regspecs_ou <- function(reg_spec, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' generate the modified reg_specs from one reg_spec (drop each variable once)

    vrbls_to_ou <- setdiff(reg_spec$mdl_vars, vvs$base_vars) %>%
        keep(~!grepl("SP.POP.TOTLm", .x))
    
    
    ## generate one-out sets: yeet squared/interaction variables
    ou_sets <- lapply(vrbls_to_ou, \(x) gen_vrbl_oud(x, vrbls_to_ou))
    names(ou_sets) <- vrbls_to_ou

    ## gen_vrbl_oud(vrbls_to_ou[2], vrbls_to_ou)
    
    ## add some manual sets of variables to exclude: 
    ## - all density
    ## - all density + closing
    ou_sets2 <- c(ou_sets,
                  list(all_dens = keep(vrbls_to_ou, ~grepl("density", .x)),
                       all_dens_close = c(keep(vrbls_to_ou, ~grepl("density", .x)),
                                          keep(vrbls_to_ou, ~grepl("nbr_closed_cum", .x)))))

    ## modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2["all_dens"], vvs)
    
    ## map(ou_sets2, ~modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2["pm_density_global_sqrd_lag2"], vvs)
    ## for some reason imap doesn't eval .y in list
    ## also imap doesn't have more options.. 
    ## imap(ou_sets2, ~modfy_regspec_ou(reg_spec, vrblset_oud = list(.y = .x), vvs))
    
    ## just use conventional query over keys
    regspecs_ou <- map(names(ou_sets2), ~modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2[.x], vvs))

    return(regspecs_ou)

    
}

gen_mdl_id_dt <- function(gof_df_cbn) {
    #' construct some model id
    #' get data for the comparision: mdl_id for joining, LL and df for LLRT
    #' first filter by LL
    
    mdl_id_dt_prep <- gof_df_cbn %>% adt() %>%
        .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
          .SDcols = c("mdl_id", "gof_value")] %>%
        .[, setnames(.SD, "gof_value", "log_likelihood")]
    ## then add df
    mdl_id_dt <- gof_df_cbn %>% adt() %>%
        .[gof_names == "df", .(mdl_id, df = gof_value)] %>% 
        .[mdl_id_dt_prep, on = "mdl_id"]

    return(mdl_id_dt)
}


cbn_splitted_files <- function(grep_pattern, fldr_info) {
    #' combines back together the CFG files
    #' 
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    cfg_files <- list.files(fldr_info$BATCH_DIR) %>%
        keep(~grepl(grep_pattern, .x))

    if (len(cfg_files) > 0) {
        
        cfg_files2 <- cfg_files %>%
        paste0(fldr_info$BATCH_DIR, .)
    
        cbn_cfgs_cmd <- sprintf("cat %s > %s", paste(cfg_files2, collapse = " "), fldr_info$REG_RES_FILE_CFGS)
    
        system(cbn_cfgs_cmd)

        cleanup_cmd <- sprintf("rm %s", paste0(cfg_files2, collapse = " "))
        system(cleanup_cmd)
    }
    
}



gen_VIF_res <- function(iv_vars, dfx) {
    #' generate VIF results from some set of variables

    
    f_allvrbls <- gen_r_f("rates", iv_vars, time_ri = T)
    r_check <- glmmTMB(f_allvrbls, dfx, family = nbinom1)

    check_collinearity(r_check)
}

vif_tester <- function(regspec) {
    #' generate VIF stats for a regspec

    dfx <- chuck(cbn_df_dict, "rates", chuck(regspec, "cfg", "cbn_name"))

    #' makes sense to generate variables here because dt they are filtered down 

    iv_vars <- keep(setdiff(regspec$mdl_vars, vvs$base_vars), ~!grepl("^SP\\.POP\\.TOTLm", .x))
    dt_vifres_all <- gen_VIF_res(iv_vars, dfx)

    ## yeet squared variables and interactions
    set_wosqrd <- c("_sqrd", "_interact")
    iv_vars_wosqrd <- keep(iv_vars, ~!any(sapply(set_wosqrd, \(w) grepl(w, .x))))
    dt_vifres_wosqrd <- gen_VIF_res(iv_vars_wosqrd, dfx)

    ## ## also yeet global density
    ## set_wosqrd_densglb <- c("_sqrd", "_interact", "pm_density_global")
    ## iv_vars_wosqrd_densglb <- keep(iv_vars, ~!any(sapply(set_wosqrd_densglb, \(w) grepl(w, .x))))
    ## dt_vifres_wosqrd_densglb <- gen_VIF_res(iv_vars_wosqrd_densglb, dfx)


    rbind(adt(dt_vifres_all)[, vrblset := "all"], adt(dt_vifres_wosqrd)[, vrblset := "wosqrd"])
    ## adt(dt_vifres_wosqrd_densglb)[, vrblset := "wosqrd_densglb"])

}



gen_VIF_regspec_res <- function(mdl_id, fldr_info) {
    #' generate VIF stats for one regspec/mdl (given model ID)

    ## read from file 
    regspecx <- get_reg_spec_from_id(mdl_id, fldr_info)
    ## set command to glmmTMB
    pluck(regspecx, "cfg", "regcmd") <- "glmmTMB"

    ## generate VIF stats (atm once with all, once without squared/interactions)
    dt_vif_cbn1 <- vif_tester(regspecx)[, mdl_id := mdl_id]
    
    return(dt_vif_cbn1)

}

gen_VIF_allres <- function(top_coefs, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the VIF numbers, write them to file in BATCH_DIR
    
    ## top_coefs[, uniqueN(mdl_id)]
    ## top_coefs[, uniqueN(mdl_id), cbn_name]
    
    ## x <- gen_VIF_regspec_res(top_coefs[, sample(mdl_id, 1)], fldr_info)
    ## ggplot(x, aes(x = VIF, y=Term, fill = vrblset)) +
    ##     geom_col(position = position_dodge2(preserve = "single"))

    ## mdl_cbn_all <- top_coefs[cbn_name == "cbn_all", mdl_id[5]]
    
    ## plan(multicore, workers = 2) ## doesn't handle scheduling after finishing
    ## plan(multisession, workers = 2) # doesn't give new task to free worker either 
    ## plan(cluster, workers = 2)

    ## tester <- function(sleep_time) {
    ##     print(sprintf("pid: %s, sleep time: %s", Sys.getpid(), sleep_time))
    ##     Sys.sleep(sleep_time)
    ## }

    ## t1 = Sys.time()
    ## nothingness <- future_map(c(5, 0.1,0.1,0.1), tester)
    ## t2 = Sys.time()
    ## t2-t1
    
    ## t1 = Sys.time()
    ## mclapply(c(5,0.1,0.1, 0.1), tester, mc.cores = 2, mc.preschedule = F)
    
    ## dt_vifx <- gen_VIF_regspec_res(unique(top_coefs$mdl_id)[1], fldr_info)
    ## dt_vifx[, .(Term, VIF, vrblset)] %>% print(n=30)

    plan(multicore, workers = 5)

    dt_vif_res <- future_map_dfr(unique(top_coefs$mdl_id), ~gen_VIF_regspec_res(.x, fldr_info))
    fwrite(dt_vif_res, paste0(fldr_info$BATCH_DIR, "VIF_res.csv"))
    
    
}


one_out_setup_and_run <- function(batch_version, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ## reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))
    fldr_info <- setup_regression_folders_and_files(batch_version)

    
    # reg_res_files <- read_reg_res_files(fldr_info)

    ## get the best fitting models
    ## mdl_idx <- reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
    ## .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
    ##   .SDcols = "mdl_id"] %>%
    ## .[5, mdl_id]
    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)

    ## mdl_id_dt[, .N, cbn_name]

    ## reconstruct folder info to read the reg_spec of model 
    
    ## set up ou (one-out) folder for new regression results
    ## first get /ou/ folder in v64
    ou_cmd <- paste0("mkdir /home/johannes/reg_res/", batch_version, "/ou/") %>% system()
    ## then setup new folders there
    ## fldr_info_ou <- setup_regression_folders_and_files("v64ou", batch_dir_addgn = "v64/ou/")
    fldr_info_ou <- setup_regression_folders_and_files(batch_version = paste0(batch_version, "ou"),
                                                       batch_dir_addgn = paste0(batch_version, "/ou/"))

    ## get original reg_spec to modify 
    reg_specs_orig <- mclapply(mdl_id_dt[, mdl_id],
                               \(x) get_reg_spec_from_id(x, fldr_info = fldr_info), mc.cores = 4)

    
    ## modify original regspecs to make them work with one-out
        
    regspecs_ou <- mclapply(reg_specs_orig, \(x) gen_regspecs_ou(x, vvs), mc.cores = 4) %>% flatten()
    print(paste0("number of regspecs_ou: ", len(regspecs_ou)))

    ## map_chr(regspecs_ou, ~chuck(.x, "cfg", "cbn_name")) %>% table()

    

    ## gen_regspecs_ou(reg_specs_orig[[3]], vvs)
    ## dx <- data.table(ou_set_title = map_chr(regspecs_ou, ~.x$cfg$ou_set_title))
    ## dx[, .N, ou_set_title][grepl("SP.POP", ou_set_title)]
                                 


    ## dx <- Reduce(rbind, list(
    ##     data.table(idx = map_chr(reg_specs_orig, ~.x$mdl_id), source = "id_orig"),
    ##     data.table(idx = map_chr(regspecs_ou, ~.x$mdl_id), source = "id_ou_regspec"),
    ##     data.table(idx = map_chr(regspecs_ou, ~.x$other_cfgs$mdl_id[1]), source = "id_other_cfgs"),
    ##     data.table(idx = list.files(fldr_info_ou$REG_RES_DIR), source = "ou_dir")))

    ## dx[, max(nchar(idx)), source]

    ## regspecs_ou[[3]]

    ## ## mys_lag_specs <- c("X5XXX3XX3X111151545521", "XXX23XXXX5111151545521")
    ## mys_lag_specs <- c("1XXX3XX3XX551151511521", "1XXX3XXX3X111141545511", "X5XXX3XX3X111151545521")

    ## penl_mys <- keep(regspecs_ou, ~.x$base_lag_spec %in% mys_lag_specs[2:3])
    ## len(penl_mys)
    ## ## penl_mys[[3]]

    ## lapply(penl_mys, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou))

    

    ## ou_debug$df_reg_anls_cfgs_wide %>% adt() %>%
    ##     .[, max(nchar(mdl_id))]

    
    
    ## len(unique(map(reg_specs_orig, ~.x$mdl_id)))
    
    ou_ids <- map_chr(regspecs_ou, ~.x[["mdl_id"]])
    if (len(ou_ids) != len(unique(ou_ids))) {stop("duplicate ou-IDs")}

    ou_files <- list.files(fldr_info_ou$REG_SPEC_DIR)
    
    if (len(setdiff(ou_ids, ou_files)) != 0) {
        ## run modified regspecs
        mclapply(regspecs_ou, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou), mc.cores = 4)
    }

    cbn_splitted_files(grep_pattern = "_cfgs.csv", fldr_info = fldr_info_ou) 

    ## ou_debug <- read_reg_res_files(fldr_info_ou)
    ## ou_debug$df_reg_anls_cfgs_wide %>% adt() %>% .[, .(mean(as.numeric(t_diff)), SD(as.numeric(t_diff)))]

    ## mclapply(regspecs_ou[1:20], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou), mc.cores = 4)

    ## running commands to combine PID-specific files
    
}


read_reg_res <- function(idx, fldr_info) {
    #' read back model results with some id    

    reg_res <- readRDS(paste0(fldr_info$REG_RES_DIR, idx))

    coef_df <- reg_res$coef_df
    coef_df$mdl_id <- idx
    
    gof_df <- reg_res$gof_df
    gof_df$mdl_id <- idx


    return(list(coef_df = coef_df,
                gof_df = gof_df))
    
}

read_reg_res_files_ou <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' reads the basic OU regression result files in
    #' for now pretty lame copy of read_reg_res_files before it was changed to handle cached models
    ## (which OU doesn't produce)

    df_reg_anls_cfgs <- read.csv(paste0(fldr_info$REG_RES_FILE_CFGS), sep = " ", header = F,
                                 col.names = c("variable", "value", "cfg_id", "lag_spec", "mdl_id", "cvrgd")) %>%
        atb()

    df_reg_anls_cfgs_wide <- df_reg_anls_cfgs %>%
        select(variable, value, mdl_id, lag_spec, cvrgd) %>% unique() %>%
        filter(variable != "lag_spec") %>% # filter out lag specs (are separate column)
        pivot_wider(id_cols = c(mdl_id, lag_spec, cvrgd), names_from = variable, values_from = value)


    all_mdl_res <- unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id) %>%
        lapply(\(x) read_reg_res(x, fldr_info))


    coef_df <- mclapply(all_mdl_res, \(x) atb(x[["coef_df"]]), mc.cores = 6) %>% bind_rows()
    gof_df <- mclapply(all_mdl_res, \(x) x[["gof_df"]], mc.cores = 6) %>% bind_rows() %>% atb()

    ## add the model details as variables 
    gof_df_cbn <- merge(gof_df, df_reg_anls_cfgs_wide) %>% atb() %>%
        mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec), ## add vrbl choice
               cbn_name = factor(cbn_name, levels = names(vrbl_cbns)),
               loop_nbr = as.integer(loop_nbr),
               t_diff = as.numeric(t_diff))
    
                              

    return(list(
        df_reg_anls_cfgs_wide = df_reg_anls_cfgs_wide,
        coef_df = coef_df,
        gof_df_cbn = gof_df_cbn
        ))

}




read_reg_res_files <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' reads the basic regression result files in
    

    ## df_reg_anls_lags <- read.csv(paste0(fldr_info$REG_RES_FILE_LAGS), sep = " ", header = F,
    ##                              col.names = c("variable", "value", "lag_spec", "cfg_id", "mdl_id", "cvrgd")) %>%
    ##     atb()

    df_reg_anls_cfgs <- read.csv(paste0(fldr_info$REG_RES_FILE_CFGS), sep = " ", header = F,
                                 col.names = c("variable", "value", "cfg_id", "lag_spec", "mdl_id", "cvrgd")) %>%
        atb()

    df_reg_anls_cfgs_wide <- df_reg_anls_cfgs %>%
        select(variable, value, mdl_id, lag_spec, cvrgd) %>% unique() %>%
        filter(variable != "lag_spec") %>% # filter out lag specs (are separate column)
        pivot_wider(id_cols = c(mdl_id, lag_spec, cvrgd), names_from = variable, values_from = value)

    ## adt(df_reg_anls_cfgs_wide)[variable == "lag_spec" & value != lag_spec]

    ## also read the stuff in from mdllog
    db_str <- paste0(fldr_info$BATCH_DIR, "mdl_cache.sqlite")
    db_mdlcache <- dbConnect(RSQLite::SQLite(), db_str)
    dt_mdllog <- dbGetQuery(db_mdlcache, "SELECT mdl_id, cbn_name, lag_spec, loop_nbr, vrbl_optmzd from mdl_log") %>% adt()

    ## adt(df_reg_anls_cfgs_wide)[, uniqueN(paste0(cbn_name, lag_spec))]
    
    ## can I stretch df_reg_anls_cfgs_wide with dt_mdllog?
    df_reg_anls_cfgs_wide2 <- adt(df_reg_anls_cfgs_wide)[, `:=`(mdl_id = NULL, loop_nbr = NULL,
                                                                vrbl_optmzd = NULL)] %>% # replace old values
        .[dt_mdllog, on = .(cbn_name, lag_spec)]

    ## read_reg_res(df_reg_anls_cfgs$mdl_id[[1]])

    


    ## list of all the model results 
    ## all_mdl_res <- unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id) %>%
    ##     mclapply(\(x) read_reg_res(x, fldr_info), mc.cores = 6)

    ## ids <- unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id)

    ## dx <- data.table(idx = ids) %>%
    ##     .[, id_len := nchar(idx)]
    
    ## dx[, .N, id_len][order(-id_len)] %>% print(n=60)
    
    ## debugging weird ids 
    ## idx <- df_reg_anls_cfgs_wide %>% adt() %>% copy() %>% 
    ##     .[, `:=`(id_ou_len = nchar(mdl_id), ctr = 1:.N)] %>% 
    ##     .[id_ou_len > 1000, id_ou_len]
        
    ## mys_lag_specs <- idx$lag_spec
    
    
    ## df_reg_anls_cfgs_wide %>% adt() %>%
    ##     .[1:3] %>% adf()

    ## idx %>% adf()
    
    
    
    all_mdl_res <- unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id) %>%
        lapply(\(x) read_reg_res(x, fldr_info))


    coef_df <- mclapply(all_mdl_res, \(x) atb(x[["coef_df"]]), mc.cores = 6) %>% bind_rows()
    gof_df <- mclapply(all_mdl_res, \(x) x[["gof_df"]], mc.cores = 6) %>% bind_rows() %>% atb()

    ## "inflate" the coef_df/gof_df with the models that have not actually been run (but identical have been)
    ## first need to add cbn_name, lag_spec (unique identifiers)

    coef_df2 <- df_reg_anls_cfgs_wide2[, .(mdl_id, cbn_name, lag_spec)][adt(coef_df), on = "mdl_id"] %>%
        .[, mdl_id := NULL] %>% # don't need mdl_id anymore, merge back with cbn_name, lag_spec
        df_reg_anls_cfgs_wide2[, .(mdl_id, cbn_name, lag_spec)][
            ., on = .(cbn_name, lag_spec), allow.cartesian = T] %>% 
        .[, .(vrbl_name, coef, se, pvalues, mdl_id)] 

    ## same play with gof_df: first to cbn_name+lagspec, then backlink to all the mdl_ids
    gof_df2 <- df_reg_anls_cfgs_wide2[, .(mdl_id, cbn_name, lag_spec)][adt(gof_df), on = "mdl_id"] %>%
        .[, mdl_id := NULL] %>% 
        df_reg_anls_cfgs_wide2[, .(mdl_id, cbn_name, lag_spec)][
            ., on = .(cbn_name, lag_spec), allow.cartesian = T] %>% 
        .[, .(gof_names, gof_value, mdl_id)] 


    ## add the model details as variables
    ## now with new dfs
    gof_df_cbn <- merge(gof_df2, df_reg_anls_cfgs_wide2) %>% atb() %>%
        mutate(# vrbl_choice = gsub("[1-5]", "0", base_lag_spec), ## add vrbl choice
               cbn_name = factor(cbn_name, levels = names(vrbl_cbns)),
               loop_nbr = as.integer(loop_nbr),
               t_diff = as.numeric(t_diff))
    
                              
    ## technology for reading to sqlite, don't use it yet tho 
    ## db_gof <- dbConnect(RSQLite::SQLite(), "/home/johannes/db_gof.sqlite")

    ## dbExecute(conn = db_gof, "PRAGMA foreign_keys=ON")
    
    ## dbListTables(db_gof)
    
    ## ## df_reg_anls_cfgs_wide
    ## dbRemoveTable(db_gof, "df_reg_anls_cfgs_wide")
    
    ## prep_sqlitedb(dbx=db_gof, dfx=df_reg_anls_cfgs_wide, table_title = "df_reg_anls_cfgs_wide",
    ##               constraints = c("PRIMARY KEY (mdl_id)"), insert_data = T)
    
    ## dbRemoveTable(db_gof, "coef_df")
    ## ## coef_df

    ## prep_sqlitedb(dbx=db_gof, dfx=coef_df, table_title = "coef_df", insert_data = T,
    ##               constraints = c("PRIMARY KEY (vrbl_name, mdl_id)",
    ##                               "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

    ## dbRemoveTable(db_gof, "gof_df_cbn")
    ## ## gof_df_cbn

    ## prep_sqlitedb(dbx=db_gof, dfx=gof_df_cbn, table_title = "gof_df_cbn", insert_data = T,
    ##               constraints = c("PRIMARY KEY (mdl_id, gof_names)",
    ##                               "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

    
    
    ## construct the within-change df


    return(list(
        df_reg_anls_cfgs_wide = df_reg_anls_cfgs_wide2,
        coef_df = coef_df2,
        gof_df_cbn = gof_df_cbn
        ))

}

gen_top_coefs <- function(df_anls_base, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    top_mdls_per_thld_choice <- gof_df_cbn %>% adt() %>%
        .[!is.na(gof_value) & gof_names == "log_likelihood"] %>% # focus on lls
        .[, vrbl_choice := gsub("[1-5]", "0", base_lag_spec)] %>% # again generate vrbl_choice urg
        .[, .SD[which.max(gof_value)], by=.(cbn_name, vrbl_choice)] %>% # pick best fitting model
        .[, .(mdl_id ,log_likelihood = gof_value)]

    top_coefs <- df_anls_base %>% adt() %>% .[top_mdls_per_thld_choice, on ="mdl_id"] %>%
        .[, vrbl_name_unlag := factor(vrbl_name_unlag, levels = rev(names(vvs$vrbl_lbls)))]
        
    
    
    return(top_coefs)
}

add_coef_sig <- function(coef_df,  df_reg_anls_cfgs_wide) {
    #' add significance to coefs

    df_anls_base <- coef_df %>%
        mutate(vrbl_name_unlag = gsub("_lag[1-5]", "", vrbl_name),
               lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5)),
               lag = ifelse(is.na(lag), 0, lag)) %>%
        merge(df_reg_anls_cfgs_wide) %>% atb() %>%
        mutate(t_value = coef/se, 
               sig = ifelse(pvalues < 0.05, 1, 0))

    return(df_anls_base)

}

gen_vrblx_vec <- function(vrblx, iv_vars) {
    #' check if vrblx has a squared term, or is part of interaction
    #' if so, append those terms to vrblx

    if (gsub("_lag", "_sqrd_lag", vrblx) %in% iv_vars) {
        vrblx <- c(vrblx, gsub("_lag", "_sqrd_lag", vrblx))
    }

    ## check if we have taxdctbl*tmitr interaction: then also add interaction to 
    if (all(grepl("tmitr_approx", vrblx) & any(grepl("ti_tmitr_interact", iv_vars)))) {
        vrblx <- c(vrblx, iv_vars[grepl("ti_tmitr_interact", iv_vars)])
    }
    
    return(vrblx)
    
}

recalc_interactions <- function(dtxx, vrblx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## need to deal with squares/interactions: maybe just recompute them all?
    
    if (len(vrblx) == 2) {
        if (grepl("_sqrd", vrblx[2])) {
            ## if variable is squared, recalculate the square
            dtxx[, (vrblx[2]) := (get(vrblx[1])^2)]
        } else if (grepl("interact", vrblx[2])) {
            ## if variable is txdctblt*tmitr interaction, recalculate it
            dtxx[, (vrblx[2]) := (get(vrblx[[1]]) * Ind.tax.incentives)]
        }
    }
    return(dtxx)

}

pred_collector <- function(dtxx, rx, dt_id) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' collect all kind of prediction values from a dataframe with adjusted values
    
    ## for 2k4, predict outside with se.fit = T (don't have to predict twice then)
    if ("pred" %!in% names(dtxx)) {

        dtxx$pred <- exp(predict(rx, dtxx))
    }
    
    ## dtxx[, .(pred, pred2, exp(pred2))]
    ## rnorm(n=10, mean = 5, sd = 3)

   ## rnorm(n = 5, mean = c(3,10), sd = c(1,1))


    list(
        dt_id = dt_id, 
        nbr_opened = sum(dtxx$nbr_opened),
        pred = sum(dtxx$pred),
        gini = dtxx[, map(.SD, sum), iso3c, .SDcols = c("pred", "nbr_opened")] %>%
            .[, diff := nbr_opened - pred] %>% .[, Gini(diff - min(diff))]
        )
    


}




pred_given_const_vrbl <- function(vrblx, rx, dfx, iv_vars) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    #' predict openings in dfx from model rx holding vrblx constant

    ## check squares: if squared version of variable is in iv_vars, add that squared version to vrblx
    ## t1 <- Sys.time()
    vrblx <- gen_vrblx_vec(vrblx, iv_vars)
    
    ## set vrblx to their first year value 
    dtx_consvrbl <- dfx %>% adt() %>%
        ## .[, pred_orig := exp(predict(rx, .))] %>%  # predict with original variables before setting to constant
        ## don't need that in every variable
        .[, year_id := 1:.N, iso3c] %>%
        .[year_id != 1, (vrblx) := NA] %>% # .[, .(iso3c, year, hnwi_nbr_1M_lag4)] %>%
        .[, (vrblx) := map(vrblx, ~nafill(get(.x), type = "locf")), iso3c] # map over columns to fill up
    
    ## emmeans(rx, ~ pm_density_lag1 | hnwi_nbr_1M_lag1)

    ## more restricted version: only those CYs with data from 2000
    dtx_consvrbl_2k_prep <- dfx %>% adt() %>% 
        .[, `:=`(min_year = min(year)), iso3c] %>% 
        .[min_year <= 2000 & year >= 2000] %>%
        .[, (paste0(vrblx, "_bu")) := map(vrblx, ~get(.x))] # backup vrblx

    
    dtx_consvrbl_2k <- dtx_consvrbl_2k_prep %>% copy() %>% 
        .[year != 2000, (vrblx) := NA] %>%
        .[, (vrblx) := map(vrblx, ~nafill(get(.x), type = "locf")), iso3c]
    
    ## constant value given on average of first 4 years
    dtx_consvrbl_2k4 <- dtx_consvrbl_2k_prep %>% copy() %>% 
        .[year >= 2004, (vrblx) := NA] %>%
        .[, (vrblx) := mean(get(vrblx[1]), na.rm = T), iso3c]

    ## dtx_consvrbl_2k4 %>% print(n=300)

    ## fix the squares/interactions if necessary
    dtx_consvrbl_2k4 <- recalc_interactions(dtx_consvrbl_2k4, vrblx)
    ## dtx_consvrbl_2k4[, vrblx, with = F] %>% print(n=300)


    ## just subtract 1
    dtx_minusone_2k <- dfx %>% adt() %>%
        .[, `:=`(min_year = min(year)), iso3c] %>% 
        .[min_year <= 2000 & year >= 2000] %>%
        .[, (vrblx[1]) := (get(vrblx[1])+1)] # only subtract from first: square/interaction has to be recomputed

    ## fix the squares/interactions if necessary
    dtx_minusone_2k <- recalc_interactions(dtx_minusone_2k, vrblx)

    
    ## dtx_minusone_2k[, .(iso3c, year, tmitr_approx_linear20step_lag5, ti_tmitr_interact_lag5)]
    ## dtx_consvrbl[, .(iso3c, year, tmitr_approx_linear20step_lag5, ti_tmitr_interact_lag5)]
    
        
    ## ## predict nbr of openings for entire dataset and only for those countries with data from 2000 onwards
    ## dtx_consvrbl$pred <- exp(predict(rx, dtx_consvrbl))
    ## dtx_consvrbl_2k$pred <- exp(predict(rx, dtx_consvrbl_2k))
    ## dtx_consvrbl_2k4$pred <- exp(predict(rx, dtx_consvrbl_2k4))
    ## dtx_minusone_2k$pred <- exp(predict(rx, dtx_minusone_2k))

    ## melt(id.vars = "dt_id")

    ## vrblx_coef <- summary(rx) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl") %>%
    ##     .[vrbl == vrblx, Estimate]
        
    ## predict for IV -1
    


    ## ## first predict IV with average, then use that to predict nbr_opened
    ## r_vx <- glmmTMB(get(vrblx) ~ year + (1 + year | iso3c),
    ##                 adt(dfx)[dtx_consvrbl_2k[, .(iso3c, year)], on = .(iso3c, year)])

    ## dtx_consvrbl_2k$vrblx_pred <- predict(r_vx, adt(dfx)[dtx_consvrbl_2k[, .(iso3c, year)], on = .(iso3c, year)])

    ## dtx_consvrbl_2k[iso3c %in% c("DEU", "USA", "KOR", "IND", "CHN", "BEL")] %>%
    ##     melt(id.vars = c("iso3c", "year"), measure.vars = c("vrblx_pred", paste0(vrblx, "_bu"))) %>%
    ##     ggplot(aes(x=year, y=value, color = iso3c, linetype = variable)) + geom_line() + geom_point()
    
    
    ## dtx_consvrbl_2k[is.na(pred)]

    ## ## look at countries with biggest diff between observed and predicted
    ## dtx_consvrbl_2k %>% copy() %>%
    ##     .[, .(diff = sum(nbr_opened) - sum(pred)), iso3c] %>%
    ##     .[order(abs(diff), decreasing = T)] %$% sum(diff)

    ## look composition: countries that are disproportionately responsible for change
    ## dtx_consvrbl_2k$pred_orig <- exp(predict(rx, adt(dfx)[dtx_consvrbl_2k[, .(iso3c, year)], on = .(iso3c, year)]))

    ## dtx_consvrbl_2k[, .(pred_orig = sum(pred_orig)
    ## do.call("pred_collector", pred_args[[3]])

    ## predict(rx, se.fit = T)
    


    ## for 2k4, predict with standard errors
    ## dtx_consvrbl_2k4[, c("pred_log", "se") := map(predict(rx, .SD, se.fit = ), ~.x)] %>%
    ##     .[, pred := exp(pred_log)]

    ## dtx with standard errors
    ## dtx_wse <- dtx_consvrbl_2k4[, .(iso3c, year, pred = pred_log, se, vrbl = vrblx[1])]

    ## hacky version to disable SE in counterfactual generation without having to change underlying code:
    ## just return same code, just no SEs
    dtx_consvrbl_2k4[, c("pred_log") := predict(rx, .SD, se.fit = F)] %>%
        .[, pred := exp(pred_log)]
    
    dtx_wse <- dtx_consvrbl_2k4[, .(iso3c, year, pred = pred_log, vrbl = vrblx[1])]
    
    ## t2 <- Sys.time()
    ## t2-t1
    
    pred_args <- list(
        list(dtxx = dtx_consvrbl, rx = rx, dt_id = "all"),
        list(dtxx = dtx_consvrbl_2k, rx = rx, dt_id = "2k"),
        list(dtxx = dtx_consvrbl_2k4, rx = rx, dt_id = "2k4"),
        list(dtxx = dtx_minusone_2k, rx = rx, dt_id = "minusone"))

    ## collect point results
    dtx_vlus <- map(pred_args, ~do.call("pred_collector", .x)) %>% rbindlist() %>%
        .[, vrbl := vrblx[1]]
    
    ## dtx_crydiff <- dtx_consvrbl_2k[, map(.SD, sum), iso3c, .SDcols = c("pred", "nbr_opened")] %>%
    ##     .[, `:=`(diff = nbr_opened - pred, vrblx = vrblx[1])] # %>% print(n=200) #%>% .[, sum(diff)]
    ##     ## .[, diff - min(diff)] %>% Gini()
    
    

    ## l_pred_vlus <- list(vrblx = vrblx[1], # only first in case of squared/interactions
    ##      ## pred_orig_N = sum(dtx_consvrbl$pred_orig)
    ##      nbr_opened_all = sum(dtx_consvrbl$nbr_opened),
    ##      pred_all_N = sum(dtx_consvrbl$pred),
    ##      pred_all_prop = dtx_consvrbl[, .(prop_opnd = sum(nbr_opened)/sum(pred)), iso3c][, 1/mean(prop_opnd)],
    ##      nbr_opened_2k = sum(dtx_consvrbl_2k$nbr_opened),
    ##      pred_2k_N = sum(dtx_consvrbl_2k$pred),
    ##      ## inverse of mean of ratio of sum(obs) and sum(pred)
    ##      ## idea would be sum(pred)/sum(obs), but sum(obs) can be 0; predictions not 0 tho
    ##      ## values below one: more predicted under constant variable than observed
    ##      ## -> if variable stays at beginning, there will be more PMs
    ##      ## AZE: 4.45 times more founded than predicted -> inverse 1/4 = 0.22
    ##      ## if vrbl would have stayed constant, there would have been only 22% of the PMs founded than were founded
    ##      pred_2k_prop = dtx_consvrbl_2k[, .(prop_opnd = sum(nbr_opened)/sum(pred)), iso3c][, 1/mean(prop_opnd)],
    ##      pred_minusone = sum(dtx_minusone_2k$pred),
    ##      gini_diff = dtx_crydiff[, Gini(diff - min(diff))]
    ##      )

    l_res <- list(
        ## l_pred_vlus = l_pred_vlus,
        dtx_vlus = dtx_vlus,
        dtx_wse = dtx_wse        
        ## dtx_crydiff = dtx_crydiff
    )

    return(l_res)

}
        

pred_sdrange <- function(vrblx, rx, dfx, iv_vars) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate predictions of modifying entire dataset by 1SD

    vrblx <- gen_vrblx_vec(vrblx, iv_vars)

    ## maybe first make grid, then merge to that, then calculate actual value? 
    dtx_grid <- adt(dfx) %$% 
        expand.grid(iso3c = unique(iso3c), year = unique(year),
                    ## updown = floor(min(get(vrblx))):ceiling(max(get(vrblx))),
                    updown = (floor(min(get(vrblx[1])))-ceiling(max(get(vrblx[1])))):
                        (ceiling(max(get(vrblx[1]))) - floor(min(get(vrblx[1]))))) %>% adt()
    
    ## this range is not enough: need updown big enough to move even highest value to bottom/lowest values up
    ## should be better now
    
    dtx_grid[, .N, updown]

    
    dtx_gridfull <- adt(dfx)[dtx_grid, on = .(iso3c, year)] %>%
        .[, c(vvs$base_vars, iv_vars, "updown", "SP_POP_TOTLm_lag0_uscld"), with = F] %>% na.omit() %>% 
        ## na.omit() %>% .[, .N, updown] ## check presense: seems to be good
        ## .[, (vrblx) := (get(vrblx) + updown)] %>% head() %>% adf()
        .[, `:=`(minvrblx = min(get(vrblx[1])), maxvrblx = max(get(vrblx[1])))] %>%
        ## .[, .(minvrblx, maxvrblx)] %>%
        ## .[, .(iso3c, year, get(vrblx),  maxvrblx, minvrblx, updown)] %>% 
        .[, (vrblx) := fifelse(updown >= 0,
                               pmin(get(vrblx[1]) + updown, maxvrblx),
                               pmax(get(vrblx[1]) + updown, minvrblx), # if below 1, ceiling to negative value
                               )]
        ## .[, .(iso3c, year, get(vrblx),  maxvrblx, minvrblx, updown)]
        ## summary()
        ## .[, mean(vrblx_mod), updown]
    
        ## .[updown %in% c(-7, 7), .N, vrblx_mod]
    
    if (len(vrblx) == 2) {
        if (grepl("_sqrd", vrblx[2])) {
            ## if variable is squared, recalculate the square
            dtx_gridfull[, (vrblx[2]) := (get(vrblx[1])^2)]
        } else if (grepl("interact", vrblx[2])) {
            ## if variable is txdctblt*tmitr interaction, recalculate it
            dtx_gridfull[, (vrblx[2]) := (get(vrblx[[1]]) * Ind.tax.incentives)]
        }
    }



    ## dtx_gridfull[updown == 3, .(iso3c, year, get(vrblx))]
    sdrange_res <- map(split(dtx_gridfull, dtx_gridfull$updown), ~sum(exp(predict(rx, .x))))

    

    dt_sdrange <- data.table(updown = as.numeric(names(sdrange_res)), pred = unlist(sdrange_res), vrblx = vrblx[1])
    
    ## dt_sdrange %>% ggplot(aes(x=updown, y=pred)) + geom_line()
    return(dt_sdrange)

}



gen_preds_given_mdfd_vrbls <- function(idx, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' general prediction of DV under changed IVs
    #' for now only has each IV being constant overall

    ## example, wrap that into own function later
    regspecx <- get_reg_spec_from_id(idx, fldr_info)
    
    dfx <- chuck(cbn_df_dict, "rates", chuck(regspecx, "cfg", "cbn_name"))
    iv_vars <- keep(setdiff(regspecx$mdl_vars, vvs$base_vars), ~!grepl("^SP\\.POP\\.TOTLm", .x))

    ## generate formula
    fx <- gen_r_f("rates", iv_vars)
    

    ## generate model
    
    rx <- glmmTMB(fx, dfx, family = nbinom1)
    
    ## fit model by country
    ## ehh idk this kinda assumes every slope is random
    ## rx_deu <- glmmTMB(fx, adt(dfx)[iso3c == "DNK"], family = nbinom2)

    ## fx2 <- sprintf("nbr_opened ~ %s + offset(log(SP_POP_TOTLm_lag0_uscld))",
    ##                paste0(iv_vars, collapse = " + ")) %>% as.formula()
    ## rx2 <- glmmTMB(fx2, dfx, family = nbinom2)

    ## compare_models(rx,rx2)
    ## compare_performance(rx,rx2)

    ## generate the variables that are to be modifed: filter with vvs: need to remove lag first.. 
    iv_vars_unlag <- gsub("_lag[1-5]", "", iv_vars) %>%
        setdiff(vvs$density_vars) %>%
        setdiff(vvs$crscn_vars) %>%
        keep(~!grepl("_sqrd", .x) & !grepl("_interact", .x))
        
    ## recover lag
    vrbls_lagged <- adt(regspecx$lngtd_vrbls)[data.table(vrbl = iv_vars_unlag), on = "vrbl"] %>%
        .[, paste0(vrbl, "_lag", lag)]

    
    ## ## vrblx <- "hnwi_nbr_1M_lag4"
    ## vrblx <- "hnwi_nbr_30M_lag2"
    ## ## vrblx <- "smorc_dollar_fxm_lag3"
    ## vrblx <- "sptinc992j_p90p100_lag3"
    ## ## vrblx <- "shweal992j_p90p100_lag4"
    ## ## vrblx <- "tmitr_approx_linear20step_lag5"
    ## ## sd(dfx$sptinc992j_p90p100_lag3)
    

    ## collect all values
    l_predres_cons <- map(vrbls_lagged, ~pred_given_const_vrbl(.x, rx, dfx, iv_vars))

    ## collect summary stats
    dt_predres_cons <- map(l_predres_cons, ~chuck(.x, "dtx_vlus")) %>% rbindlist() %>%
        .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]

    dt_predres_wse <- map(l_predres_cons, ~chuck(.x, "dtx_wse")) %>% rbindlist() %>%
        .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]

    

    ## dt_predres_cons %>% copy() %>%
    ##     .[dt_id %!in% c("minusone", "all")] %>% 
    ##     .[, diff := nbr_opened - pred] %>% 
    ##     ggplot(aes(x=diff, y=vrbl, color = dt_id)) + geom_point()

    ## map(l_predres_cons, ~chuck(.x, "dtx_crydiff")) %>% rbindlist() %>%
    ##     ## .[iso3c %in% c("KOR", "USA", "DEU")] %>%
    ##     .[abs(diff) > 3] %>% 
    ##     ## .[order(diff)] %>% .[, iso3c := factor(iso3c)] %>% 
    ##     ggplot(aes(x=diff, y=vrblx, color = iso3c, label = iso3c)) + geom_point() + geom_text_repel()



    ## ## actually run the predictions
    ## l_predres <- list(
    ##     dt_predres_cons = map(vrbls_lagged, ~pred_given_const_vrbl(.x, rx, dfx, iv_vars) %>%
    ##                                             chuck("l_pred_vlus")) %>% rbindlist() %>%
    ##         .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]
    ##     ## dt_predres_sdrange = map(vrbls_lagged, ~pred_sdrange(.x, rx, dfx, iv_vars)) %>% rbindlist() %>%
    ##     ##     .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]
    ## )

    
    ## l_predres$dt_predres_sdrange %>%
    ##     ggplot(aes(x=updown, y=pred, color = vrblx)) + geom_line()

    ## margins(rx)
    ## x <- marginal_effects(rx) %>% adt()

    ## x[, map(.SD, mean)] %>% melt()


    ## return(l_predres$dt_predres_cons)
    return(list(
        dt_predres_cons = dt_predres_cons,
        dt_predres_wse = dt_predres_wse))
    
    

}


gen_cntrfctl <- function(gof_df_cbn, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get best models
    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)

    idx <- mdl_id_dt$mdl_id[5]
    t1 <- Sys.time()
    jj <- gen_preds_given_mdfd_vrbls(idx, fldr_info)
    t2 <- Sys.time()
    t2-t1
    
    l_cntrfctl_res <- mclapply(mdl_id_dt$mdl_id, \(x) gen_preds_given_mdfd_vrbls(x, fldr_info), mc.cores = 5)

    dt_cntrfctl_cons <- map(l_cntrfctl_res, ~chuck(.x, "dt_predres_cons")) %>% rbindlist()

    ## collect model-country-year specific predictions + SEs
    dt_cntrfctl_wse <-  map(l_cntrfctl_res, ~chuck(.x, "dt_predres_wse")) %>% rbindlist()
     
    fwrite(dt_cntrfctl_cons, paste0(fldr_info$BATCH_DIR, "cntrfctl_cons.csv"))

    fwrite(dt_cntrfctl_wse, paste0(fldr_info$BATCH_DIR, "cntrfctl_wse.csv"))

    ## dt_cntrfctl_res <- fread(paste0(fldr_info$BATCH_DIR, "cntrfctl_res.csv"))
    ## dt_cntrfctl_res[mdl_id == idx]

    

}



gen_cryexmpls <- function(top_coefs) {
    #' pick some countries for each variable


    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;


    top_coefs %>% copy() %>% .[, .(vrbl_name = unique(vrbl_name))]
    

    ## get up to two countries with highest correlation between rate_opened and vrblx
    dt_topcor_crys <- dtx_consvrbl_2k_prep %>% copy() %>%
        ## .[, pred_orig := exp(predict(rx, dtx_consvrbl_2k_prep))] %>% 
        .[, c(vvs$base_vars, "nbr_opened", vrblx, "SP_POP_TOTLm_lag0_uscld"), with = F] %>%
        .[, `:=`(rate_opened = nbr_opened/SP_POP_TOTLm_lag0_uscld, sum_opnd = sum(nbr_opened)), iso3c] %>% 
        ## .[, resid := (nbr_opened - pred_orig)^2] %>% # average
        ## .[, `:=`(resid_avg = mean(resid), sum_opnd = sum(nbr_opened)), iso3c] %>%
        .[, cor := cor.test(rate_opened, get(vrblx)) %>% chuck("estimate"), iso3c] %>% 
        ## .[, `:=`(pred_vrblx = exp(vrblx_coef * get(vrblx)))] %>% # multiplier of vrblx?
        ## prediction of other variables without vrblx: 
        ## .[, `:=`(pred_wovrblx = (pred_orig/SP_POP_TOTLm_lag0_uscld)/exp(vrblx_coef)*SP_POP_TOTLm_lag0_uscld)] %>%
        ## .[, asdf := pred_vrblx * pred_wovrblx] %>% 
        ## .[, `:=`(fit_vrblx = 
        .[sum_opnd >= 5] %>% 
        .[cor > 0.25] %>%
        .[, .(iso3c, cor)] %>% unique() %>% head(2) 
    
    ## .[order(resid_avg)] %>%
    ## .[, iso3c := factor(iso3c, unique(iso3c))]
    

    ## %>% 
    ##     melt(id.vars = c("iso3c", "year"),
    ##          measure.vars = c("nbr_opened", vrblx)) %>%
    ##     ggplot(aes(x=year, y=value, color = variable)) +
    ##     geom_smooth(se=F, span = 0.2) +
    ##     ## geom_line() + 
    ##     facet_grid(~iso3c)

}



reg_helper_rscor <- function(dtxx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' collect model statistics of random slopes and slope/intercept correlations

    ## print(paste0(dtxx$cbn_name[1], "_", dtxx$variable[1]))
    
    resx <- glmmTMB(value ~ year + (1 + year | iso3c), dtxx)

    
    ## summary dt to collect main coef and se
    dt_sumry <- summary(resx) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl")

    ## collect single numbers: slpcons correlation, main coef/se
    dt_scalars <- data.table(cor_slpcons = resx %>% summary() %>% chuck("varcor", "cond", "iso3c") %>%
                                 attr(which="correlation") %>% .[1,2],
                             coef = dt_sumry[vrbl == "year", Estimate],
                             se = dt_sumry[vrbl == "year", `Std. Error`],
                             cbn_name = dtxx$cbn_name[1], vrbl = dtxx$vrbl[1]
                             )

    dt_crycoefs <- resx %>% coef() %>% chuck("cond", "iso3c") %>%
        adt(keep.rownames = "iso3c") %>% .[, setnames(.SD, "(Intercept)", "cons")] %>%
        .[, `:=`(cbn_name = dtxx$cbn_name[1], vrbl = dtxx$vrbl[1])]

    return(
        list(dt_scalars = dt_scalars, 
             dt_crycoefs = dt_crycoefs
             )
    )

}



gen_res_velps <- function(cbn_dfs_rates, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate data for yearly development of longitudinal variables
        
    dtx <- imap_dfr(cbn_dfs_rates[1:3], ~adt(.x)[, cbn_name := .y])

    ## get the variables to describe
    vrbl_tdesc <- names(dtx) %>% keep(~grepl("lag0", .x)) %>%
        keep(~!grepl("_sqrd", .x)) %>% ## yeet squares
        keep(~!grepl("interact", .x)) %>% ## yeet interactions
        keep(~!grepl("TOTLm_", .x)) ## yeet population

    ## melt into long
    dtx_splong <- dtx %>% melt(id.vars = c(vvs$base_vars, "cbn_name"),
                               measure.vars = vrbl_tdesc, variable.name = "vrbl") %>%
        .[, nbr_nas := sum(is.na(value)), .(cbn_name, vrbl)] %>%
        .[nbr_nas == 0] %>% ## filter out variables with missings (for datasets)
        .[, vrbl := gsub("_lag[0-5]", "", vrbl)]

    ## ## log some variables
    ## vrbls_tolog <- keep(unique(dtx_splong$vrbl), ~grepl("hnwi_nbr_", .x) | grepl("smorc", .x) |
    ##                                                  .x == "NY.GDP.PCAP.CDk")

    ## ## actually log variables: first move to positive range, than log, than scale again
    ## dtx_splong[vrbl %in% vrbls_tolog, value := scale(log((value - min(value)) + 0.001)), .(vrbl, cbn_name)]

    ## ## check that logging werks
    ## dtx_splong[vrbl == "hnwi_nbr_5M" & cbn_name == "cbn2" & iso3c %in% c("DEU", "FRA", "USA", "CHE")] %>%
    ##     atb() %>% viz_lines(y="value")


    ## split into separate dts
    dtx_split <- copy(dtx_splong) %>%
        .[, year := year - min(year)] %>% # set min to 1995 (or min year for proper correlations)
        split(by = c("vrbl", "cbn_name"), drop = T)

    ## generate overall results
    l_velpres <- lapply(dtx_split, reg_helper_rscor)

    
    ## collect results into separate dts
    dt_velp_crycoefs <- map_dfr(l_velpres, ~chuck(.x, "dt_crycoefs"))
        ## .[vrbl %!in% c("nbr_closed_cum_global", "pm_density_global")] %>%
        ## vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        ## .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]

    
    dt_velp_scalars <- map_dfr(l_velpres, ~chuck(.x, "dt_scalars"))
        ## .[vrbl %!in% c("nbr_closed_cum_global", "pm_density_global")] %>%
        ## vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        ## .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]

    ## gen_plt_velp(dt_velp_crycoefs, dt_velp_scalars)

    ## write to file
    fwrite(dt_velp_crycoefs, paste0(fldr_info$BATCH_DIR, "dt_velp_crycoefs.csv"))
    fwrite(dt_velp_scalars, paste0(fldr_info$BATCH_DIR, "dt_velp_scalars.csv"))


    ## keep for testing
    ## dtx_splong %>% copy() %>%
    ##     .[, year := year - min(year)] %>%
    ##     .[cbn_name == "cbn_all" & vrbl =="hnwi_nbr_1M"] %>%
    ##     reg_helper_rscor()
}

## gen_res_velps(cbn_dfs_rates, fldr_info_optmz)


gen_nolag <- function(fldr_info, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
   

    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)


}

compare_r_stata <- function(gof_df_cbn) {


    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)

    idx <- mdl_id_dt$mdl_id[15]
        
    regspecx <- get_reg_spec_from_id(idx, fldr_info)

    dt_glmmTMB <- run_vrbl_mdl_vars(regspecx, vvs, fldr_info, verbose = F,
                                    wtf =F, return_objs = c("res_parsed")) %>%
        chuck("res_parsed", "coef_df") %>% adt() %>% .[, src := "glmmTMB"] %>%
        .[vrbl_name == "(Intercept)", vrbl_name := "cons"]
    
    
    regspecx2 <- copy(regspecx)
    pluck(regspecx2, "cfg", "regcmd") <- "xtnbreg"

    dt_xtnbreg <- run_vrbl_mdl_vars(regspecx2, vvs, fldr_info, verbose = F,
                                    wtf =F, return_objs = c("res_parsed")) %>%
        chuck("res_parsed", "coef_df") %>% adt() %>% .[, src := "xtnbreg"]

    rbind(dt_xtnbreg, dt_glmmTMB) %>%
        melt(id.vars = c("vrbl_name", "src"), variable.name = "measure") %>% .[measure == "coef"] %>%
        dcast.data.table(vrbl_name ~ src) %>% na.omit() %>% 
        .[, `:=`(diff = glmmTMB - xtnbreg, ratio1 = glmmTMB/xtnbreg, ratio2 = xtnbreg/glmmTMB)] %>%
        .[ratio1 > 1.5 | ratio2 > 1.5]

}



postestimation <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    print("reading files")
    reg_res_files <- read_reg_res_files(fldr_info)    
    
    gof_df_cbn <- reg_res_files$gof_df_cbn
    df_reg_anls_cfgs_wide <- reg_res_files$df_reg_anls_cfgs_wide
    
    ## gen_plt_cvrgnc(gof_df_cbn)
    print("generating counterfactual")

    ## compare_r_stata(gof_df_cbn)
    
    gen_cntrfctl(gof_df_cbn, fldr_info)
    ## gen_cntrfctl(reg_anls_base$gof_df_cbn, fldr_info)

    ## gen_cryexmpls(top_coefs)

    print("generating development info")
    ## generate data for yearly developments
    gen_res_velps(cbn_dfs_rates, fldr_info)
  
    

    print("gen VIFs")
    
    ## generate top coefs, use it for VIF data
    df_anls_base <- add_coef_sig(reg_res_files$coef_df, reg_res_files$df_reg_anls_cfgs_wide)
    top_coefs <- gen_top_coefs(df_anls_base, gof_df_cbn)
    gen_VIF_allres(top_coefs, fldr_info)

    print("generate one-out tests")

    one_out_setup_and_run(fldr_info$batch_version, gof_df_cbn)
    ## test_dharma()


}


setup_db_mdlcache <- function(fldr_info) {
    #' set up model cache, but only if it not already exists (otherwise would overwrite)
    #' so far assumes that cbn_name + lag_spec is unique (no other things allowed to vary)
    db_str <- paste0(fldr_info$BATCH_DIR, "mdl_cache.sqlite")

    if (basename(db_str) %!in% list.files(fldr_info$BATCH_DIR)) {
        db_mdlcache <- dbConnect(RSQLite::SQLite(), db_str)
        dt_cacheschema <- data.table(cbn_name = "asdf", lag_spec = "asdf", ll = 11.1, proc_pid = 55L)

        prep_sqlitedb(db_mdlcache, dt_cacheschema, "mdl_cache",
                      constraints = "PRIMARY KEY (cbn_name, lag_spec, proc_pid)")
        ## add log: all the models that are run
        dt_mdllog_schema <- data.table(mdl_id = "id", cbn_name = "cbnx", lag_spec = "lag_spec", loop_nbr = 1L,
                                       vrbl_optmzd = "vrbl")
        prep_sqlitedb(db_mdlcache, dt_mdllog_schema, "mdl_log", constraints = "PRIMARY KEY (mdl_id)")
    }
}


## ** main

if (identical(args, character(0))) {
    stop("functions are done")
}


if (is.null(args[[1]])) {
    stop("functions are DONE")
}

PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_reg.R"))



vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=1)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 5,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v09",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_df_dict$counts)[1],
    mdls_to_include = c("full"),
    wtf = T
)



reg_spec_mdls_optmz <- gen_batch_reg_specs(reg_settings_optmz, vvs, vrbl_thld_choices_optmz)
print(len(reg_spec_mdls_optmz))
## lapply(reg_spec_mdls_optmz, \(x) x[["mdl_vars"]])


fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz$batch_version)

setup_db_mdlcache(fldr_info_optmz)


mclapply(reg_spec_mdls_optmz, \(x) optmz_reg_spec(x, fldr_info_optmz, reg_settings_optmz),
         mc.cores = 5, mc.preschedule = F)

stop("it's time to stop")
print("models have been run, now combining files")

cbn_splitted_files("_cfgs.csv[0-9]", fldr_info_optmz)

print("files have been combined, now run postestimation")

## run the one-out analysis


postestimation(fldr_info_optmz)

## one_out_setup_and_run("v67")


## optmz_reg_spec(reg_spec_mdls_optmz[[1]], fldr_info_optmz, reg_settings_optmz)


## mclapply(reg_spec_mdls_optmz, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_optmz), mc.cores = 6)

## this stop should never be commented out 
stop("regression is DONE")



## ** garage for inspection

## *** basic test
regspec_x <- reg_spec_mdls_optmz[[2]]

reg_settings_garage <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value = F)
optmz_reg_spec(reg_spec_mdls_optmz[[3]], fldr_info_optmz, reg_settings_garage)



## *** some other test
regspec_x <- get_reg_spec_from_id("XXX5XX3X5X553335511111115--cbn1--full--nr--TRUE--glmmTMB--rates--XXX3XX2X1X222231543344221--hn200iigwi99--3--XXX5XX3X5X553335511111115--14--NY.GDP.PCAP.CDk", fldr_info_optmz)

optmz_reg_spec(regspec_x, fldr_info_optmz, reg_settings_garage)



## ** look at growth rate numbers

cbn_dfs_rates$cbn2 %>% adt %>% .[, reg6 := rcd_iso3c_reg6(iso3c)] %>% atb %>%
    viz_lines(y="NY.GDP.PCAP.KD.ZG_lag5", facets = "reg6", max_lines = 6)


## ** test different optimization settings for speed






## resx2 <- copy(resx)

## ** testing callgraph, but still too buggy
## library(jtls)

## c_dirs <- gc_dirs(dir_proj = "/home/johannes/Dropbox/phd/papers/org_pop/")



## testf <- function() {
##     test_obj <- 10
##     attr(test_obj, "gnrtdby") <- as.character(match.call()[[1]])
##     return(test_obj)
## }


## testobj <- testf()
## testobj2 <- testf()
## jtls::gwd_clgrph()
## gl_clgr_objs()


## ** debugging lack of convergence

## *** v85
## termlog inspection
dt_termlog <- fread(paste0(fldr_info_optmz$BATCH_DIR, "termlog.txt"), header = F, col.names = c("garbage", "msg"))

dt_termlog %>%
    .[!grepl("already there", msg)] %>% 
    .[msg %!in% names(vvs$vrbl_lbls)]

## *** v78

## run ~150 unoptimized models, but all converge
ii <- 12
while (T) {
    x <- reg_spec_mdls_optmz[[ii]]

    x$cfg$regcmd <- "glmmTMB"
    resx <- run_vrbl_mdl_vars(x, vvs, fldr_info_optmz, verbose = F, wtf = F)

    print(ii)
    if (!resx$converged) {break}
    ii <- ii+1
    
}

## try running single model in detail; don't find failure there neither
optmz_reg_spec(reg_spec_mdls_optmz[[2]], fldr_info_optmz, reg_settings_optmz)

## search the results
mdl_ids_fail <- reg_anls_base$df_reg_anls_cfgs_wide %>% adt() %>% #.[, .N, cvrgd]
    .[cvrgd == 0, mdl_id]

regspec_fail <- get_reg_spec_from_id(mdl_id_fail, fldr_info)

run_vrbl_mdl_vars(regspec_fail, vvs, fldr_info_optmz, verbose = F, wtf = F)

cvrg_res <- map(mdl_ids_fail[1:30], ~get_reg_spec_from_id(.x, fldr_info) %>%
                      run_vrbl_mdl_vars(., vvs, fldr_info_optmz, verbose = F, wtf = F))


## *** v38

## non_cvrgd_spec <- get_reg_spec_from_id(
##     "XX5XXX2XX3111135211--cbn_no_cult_spending_and_mitr--full--XX2XXX4XX4555532345--1--hnwi_nbr_30M",
##     fldr_info_optmz)


## non_cvrgd_spec$cfg$difficulty <- T
## non_cvrgd_spec$cfg$technique_str <- "nr"
## non_cvrgd_spec$regcmd <- "xtnbreg"


## run_vrbl_mdl_vars(non_cvrgd_spec, vvs, fldr_info_optmz, verbose = T)

## *** v41

## dt_cfgs <- fread("/home/johannes/reg_res/v41/v41_cfgs.csv")
## dt_cfgs[V6 == 0] %>% adf()

non_cvrgd_spec <- get_reg_spec_from_id(
    "XX4XX3X3XX111125211--cbn_no_cult_spending_and_mitr--full--nr--TRUE--XX3XX2X3XX443221213--1--NY.GDP.PCAP.CDk",
    fldr_info_optmz)

non_cvrgd_spec$regcmd <- "xtnbreg"

t1 = Sys.time()
run_vrbl_mdl_vars(non_cvrgd_spec, vvs, fldr_info_optmz, verbose = F)
t2 = Sys.time()

print(t2-t1)

## *** v44

glmm_na_spec <- get_reg_spec_from_id(
    "XX5X3XX3XX441155232--cbn_no_cult_spending--full--nr--TRUE--glmmTMB--XX5X2XX5XX114113144--2--hnwi_nbr_30M",
    fldr_info_optmz)

run_vrbl_mdl_vars(glmm_na_spec, vvs, fldr_info_optmz, verbose = T)

optmz_reg_spec(glmm_na_spec, fldr_info_optmz, reg_settings_optmz)

## *** v47

## debug errors in v47: some models still throw errors -> FIND them

started <- fread(paste0(fldr_info_optmz$MDL_START_FILE), col.names = "mdl_id", header = F)
finished <- fread(paste0(fldr_info_optmz$MDL_END_FILE), col.names = "mdl_id", header = F)

setdiff(started$mdl_id, finished$mdl_id)
setdiff(finished$mdl_id, started$mdl_id)

error_spec <- get_reg_spec_from_id(
    started[!finished, on = "mdl_id"][c(3)], fldr_info_optmz)

run_vrbl_mdl_vars(error_spec, vvs, fldr_info_optmz, verbose = T)

## ** test mc.preschedule = F

testx <- function() {
    print(Sys.getpid())
    Sys.sleep(runif(1, 0, 5))}

nbr_cores <- 3
t1 = Sys.time()
mclapply(seq(1, nbr_cores* 5), \(x) testx(), mc.cores = nbr_cores)
t2 = Sys.time()
mclapply(seq(1, nbr_cores* 5), \(x) testx(), mc.cores = nbr_cores, mc.preschedule = F)
t3 = Sys.time()

print(t3-t2)
print(t2-t1)
