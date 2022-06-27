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

library(RStata)

options(RStata.StataPath = "/usr/local/stata14/stata")
options(RStata.StataVersion = 14)

cleanup_old_r_procs <- function() {
    #' kill old R procs, not used so far
    
    ps() %>% filter(name == "R", pid != Sys.getpid()) %>%
        pull(pid) %>%
        lapply(\(x) ps_kill(ps_handle(x)))

}

cleanup_old_stata_procs <- function() {
    #' yeet old (older than 12 secs) stata processes

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


get_stata_result <- function(iv_vars, stata_output_vars, gof_names, dfx, verbose) {
    #' run the xtnbreg regression with stata() given independent vars,
    #' also give stata_output_vars since also needed in parse_stata_res

    ## the different stata matrices have some overlapping (row/col) names, is not allowed for svmat
    ## -> generate generic names (actual names throw weird errors)

    iv_vars_stata <- gsub("\\.", "_", iv_vars)
    
    res_names <- paste0("r", seq(len(stata_output_vars)*2 + len(gof_names)))

    ## for gof and stata_return matrix turn into wide and transpose to avoid backslashes:
    ## backslash would be easier in stata syntax, but is messy in plain text

    # print(Sys.getpid())    
    

    stata_code = list(
        panel_setup = "xtset iso3c_num year",
        reg_cmd = paste0("xtnbreg nbr_opened ", paste(iv_vars_stata, collapse = " "), ", re"),
        coef_cmd = "mata: b=st_matrix(\"e(b)\")' \n mata: st_matrix(\"b_stata\", b)",
        se_cmd = "mata: se=sqrt(diagonal(st_matrix(\"e(V)\"))) \n mata: st_matrix(\"se_stata\", se)",
        gof_cmd = "matrix gof = ( e(N), e(ll), e(N_g), e(chi2), e(p), e(df_m))'", 
        cbn_cmd = "matrix stata_return = (b_stata', se_stata', gof')",
        rename_cmd = paste0("matrix colnames stata_return = ", paste0(res_names, collapse = " ")),
        sv_cmd = "svmat stata_return \n keep stata_return* \n drop if missing(stata_return1)")


    stata_src <- paste(stata_code, collapse = "\n")

    
    
    stata_res <- stata(stata_src, data.in = dfx, data.out = T, stata.echo = verbose) %>% atb()

    return(stata_res)
}





parse_stata_res <- function(stata_res, stata_output_vars, gof_names){
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
    return(res_list)
}




save_parsed_res <- function(res_list, idx) {
    #' save the parsed stata res to id

    saveRDS(res_list, file = paste0(REG_RES_DIR, idx))
    ## readRDS(file = paste0(PROJECT_DIR, "data/processed/res_list"))

}


gen_lag <- function(vrbl, lag) {

    
    #' lag vrbl by lag years

    lag <- as.numeric(lag)
    lag_name <- paste0(vrbl, "_lag", lag)

    df_lag <- df_reg %>% select(iso3c, year, !!vrbl) %>%
        group_by(iso3c) %>%
        mutate(!!lag_name := lag(get(vrbl), lag)) %>%
        select(iso3c, year, !!lag_name)
    return(df_lag)
}



gen_vrbl_thld_choices <- function(hnwi_vars, inc_ineq_vars, weal_ineq_vars) {
    #' generate threshold (hnwi, shares 


    vrbl_choices <- expand.grid(hnwi_var = hnwi_vars,
                                inc_ineq_var = inc_ineq_vars,
                                weal_ineq_var = weal_ineq_vars, stringsAsFactors = F) %>%
        atb()
}




gen_reg_spec <- function(non_thld_lngd_vars) {
    
    #' generate the regression specification: basically just choice of some variables/thresholds and lag lengths

    ## could also just sample here, probably have to
    x <- vrbl_thld_choices[sample(nrow(vrbl_thld_choices),1),]

    ## select variable, generate random lag

    ## lngtd_vars <- c(x$hnwi_var,
    ##                 x$inc_ineq_var,
    ##                 x$weal_ineq_var,
    ##                 non_thld_lngtd_vars) %>% atb() %>%
    ##     select(vrbl = value) %>%
    ##     ## group_by(vrbl) %>% 
    ##     mutate(lag = sample(seq(1,5),1))

    lngtd_vars <- data.frame(vrbl = c(x$hnwi_var, x$inc_ineq_var,  x$weal_ineq_var,  non_thld_lngtd_vars))
    lngtd_vars$lag <- sample(seq(1,5), nrow(lngtd_vars), replace = T)

    ## lag TI*TMITR interaction with same lag as TMITR
    lngtd_vars[which(lngtd_vars$vrbl == "ti_tmitr_interact"),]$lag <- lngtd_vars[lngtd_vars$vrbl == "tmitr_approx_linear20step", ]$lag

    reg_spec <- list(lngtd_vrbls = lngtd_vars)

    return(reg_spec)
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

gen_cbns <- function(rel_vars) {
    #' generate the combinations
    #' should be own function to allow the combination generation when generating the dfs (broad) and within specification

    vrbl_cbns <- list(
        cbn_all=rel_vars,
        cbn_no_cult_spending = rel_vars[!grepl("smorc_dollar_fx", rel_vars)],
        cbn_no_cult_spending_and_mitr = rel_vars[!grepl("smorc_dollar_fx|tmitr_approx_linear20step|ti_tmitr_interact", rel_vars)],
        cbn_controls = rel_vars[rel_vars %in% c(base_vars) | grepl("SP.POP.TOTLm|NY.GDP.PCAP.CDk", rel_vars)])

    return(vrbl_cbns)
}


gen_cbn_models <- function(cbn_vars, base_vars, ctrl_vars) {
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
    
    return(all_cbn_models)
}



    
gen_cbn_dfs <- function(lngtd_vars, crscn_vars, vrbl_cnbs) {
    #' generate the dfs that correspond to variable combinations
    #' checks whether a country-year has coverage for all the lags for all the variables required by combination
    #' needs lngtd_vars and crscn_vars to set which to variables need to be named as lag

    

    cvrg_lags <- lapply(lngtd_vars, \(x)
                        lapply(seq(1,5), \(i)
                               gen_lag(vrbl=x, lag=i) %>%
                               select(iso3c, year, value =paste0(x, "_lag", i)) %>%
                               mutate(lag=i, vrbl = x, lag_col = "_lag")) %>%
                        Reduce(\(x,y) rbind(x,y), .)) %>%
        Reduce(\(x,y) rbind(x,y), .)



    cvrg_crscn <- lapply(crscn_vars, \(x) select(df_reg, iso3c, year, value = all_of(x)) %>%
                                      mutate(vrbl = x, lag_col = "", lag=""))

    ## convert lag to string to allow having empty string for lag of cross-sectional variables
    cvrg_lags_crscn <- rbind(cvrg_lags %>% mutate(lag = as.character(lag)) %>%
                             select(iso3c, year, vrbl, value, lag_col, lag),
                             cvrg_crscn) %>% atb()


    ## generate whether a country is covered by a combination
    cbn_cvrg <- lapply(vrbl_cbns, \(cbn_vars)
                       cvrg_lags_crscn %>%
                       filter(vrbl %in% cbn_vars) %>% 
                       group_by(iso3c, year, vrbl) %>%
                       summarize(all_valid = all(!is.na(value))) %>%
                       group_by(iso3c, year) %>%
                       summarize(country_year_valid = all(all_valid)) %>%
                       filter(country_year_valid == T)
                       )


    df_all_lags <- cvrg_lags_crscn %>%
        mutate(vrbl_lag = paste0(vrbl, lag_col, lag)) %>%
        select(iso3c, year, vrbl_lag, value) %>%
        pivot_wider(names_from = vrbl_lag, values_from = value)

    cbn_dfs <- lapply(cbn_cvrg, \(x) atb(merge(select(all_of(x), iso3c, year), df_all_lags)))

    cbn_dfs <- lapply(cbn_dfs, \(x) mutate(x, across(all_of(setdiff(names(x), base_vars)), scale_wo_attr)) %>%
                                    mutate(iso3c_num = as.numeric(factor(iso3c))))

    ## sd(cbn_dfs$cbn_all$ti_tmitr_interact_lag1)
    ## mean(cbn_dfs$cbn_all$ti_tmitr_interact_lag1)
    ## sd(cbn_dfs$cbn_all$tmitr_approx_linear20step_lag1)
    ## mean(cbn_dfs$cbn_all$tmitr_approx_linear20step_lag1)


    ## re-calculate the interaction of tax incentives and top marginal income tax rates:
    ## effectively makes it bigger, so coefs should be smaller

    ## first calculate interaction values

    ti_tmitr_interactions <- lapply(cbn_dfs, \(x)
           lapply(seq(1,5), \(lagx)
                  x %>% 
                  mutate(!!paste0("ti_tmitr_interact_lag", lagx) :=
                             get(paste0("tmitr_approx_linear20step_lag", lagx)) * sum_core) %>% 
                  select(!!paste0("ti_tmitr_interact_lag", lagx))) %>%
           Reduce(\(x,y) cbind(x,y), .) %>% atb())
           
    ## then replace old and with new interaction values

    cbn_dfs_names <- names(cbn_dfs)
    names(cbn_dfs_names) <- cbn_dfs_names

    cbn_dfs <- lapply(cbn_dfs_names, \(x) 
           cbind(cbn_dfs[[x]] %>% select(-all_of(names(ti_tmitr_interactions[[x]]))),
                 ti_tmitr_interactions[[x]]) %>% atb())



    ## add iso3c_num to make stata happy 
    ## cbn_dfs <- lapply(cbn_dfs, \(x) mutate(x, iso3c_num = as.numeric(factor(iso3c))))

    ## add nbr_opened
    cbn_dfs <- lapply(cbn_dfs, \(x) merge(x, select(df_reg, iso3c, year, nbr_opened), all.x = T) %>% atb())

    
    return(cbn_dfs)
}

gen_lag_id <- function(reg_spec) {
    #' generate the variable choice lag part of the id
    
    df_idx <- merge(tibble(vrbl = lngtd_vars), 
          reg_spec$lngtd_vrbls, all.x = T) %>%
        mutate(lag_str = as.character(lag)) %>%
        mutate(lag_str = ifelse(is.na(lag_str), "X", lag_str)) %>%
        select(variable=vrbl, value = lag_str)

    return(df_idx)
}
    



gen_mdl_id <- function(reg_spec) {
    #' generate unique id for each model

    ## get id: need information which variable is there (X if not there), also of lag of each variable that is there
    

    ## df_idx <- merge(tibble(vrbl = lngtd_vars), 
    ##       reg_spec$lngtd_vrbls, all.x = T) %>%
    ##     mutate(lag_str = as.character(lag)) %>%
    ##     mutate(lag_str = ifelse(is.na(lag_str), "X", lag_str)) %>%
    ##     select(variable=vrbl, value = lag_str)
    df_idx <- gen_lag_id(reg_spec)


    vrbl_lag_id <- paste0(df_idx$value, collapse = "")


    other_cfgs <- data.frame(rbind(c("cbn_name", reg_spec$cbn_name),
                                   c("mdl_name", reg_spec$mdl_name),      
                                   c("vrbl_varied", reg_spec$vrbl_varied),
                                   c("base_lag_spec", reg_spec$base_lag_spec))) %>%
        select(variable = X1, value = X2)

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
                other_cfgs = atb(other_cfgs)))
}
        

timeout_stata <- function(iv_vars, stata_output_vars, gof_names, dfx, file_id, verbose) {
    #' run stata command, time it out if taking too long
    
    
    ## setwd(PROJECT_DIR)

    cur_wd <- getwd()
    
    pid <- Sys.getpid()
    new_dir <- paste0(PID_DIR, pid)
    
    present_dirs <- list.dirs(paste0(PID_DIR), recursive = F)
    
    if (new_dir %!in% present_dirs) {
    
        mkdir_cmd <- paste0("mkdir ", new_dir)
        system(mkdir_cmd)

    }

    setwd(new_dir)

    stata_res_raw <- get_stata_result(iv_vars = iv_vars, stata_output_vars = stata_output_vars,
                                      gof_names = gof_names, dfx = dfx, verbose = verbose)

    ## setwd(cur_wd)
    ## print(pid)
    ## print(Sys.getpid())

    if (nrow(stata_res_raw) > 1) {stop("something wrong")} ## debug 

    stata_res_parsed <- parse_stata_res(stata_res_raw, stata_output_vars, gof_names)

    save_parsed_res(stata_res_parsed, idx = file_id)
    
    return(list(
        result = T,
        pid = pid))
    ## return("asdf")
}

    
    
    


    
## run_vrbl_mdl_vars <- function(mdl_vars, df_cbn, cbn_name, mdl_name, reg_specx) {
run_vrbl_mdl_vars <- function(reg_spec, verbose = F) {
    #' run one regression given the model vars
    
    
    df_cbn <- cbn_dfs[[reg_spec$cbn_name]]


    id_res <- gen_mdl_id(reg_spec)

    df_idx <- id_res$df_idx
    other_cfgs <- id_res$other_cfgs
          
    file_id <- df_idx$mdl_id[1]

    ## saving reg spec information to debug later 
    saveRDS(reg_spec, file = paste0(REG_SPEC_DIR, file_id))
    ## write model id to start_file to see which models don't converge
    write.table(file_id, MDL_START_FILE, append = T, col.names = F, row.names = F)


    iv_vars <- reg_spec$mdl_vars[reg_spec$mdl_vars %!in% base_vars]

    ## print("---")

    ## print(iv_vars)
    
    stata_output_vars <- c(iv_vars, c("cons", "ln_r", "ln_s"))
    gof_names <- c("N", "log_likelihood", "N_g", "Chi2", "p", "df")

    dfx <- select(df_cbn, all_of(c(base_vars, "iso3c_num", "nbr_opened",iv_vars)))

    ## Sys.sleep(runif(1)/10)
    
    ## use trycatch with fscaret to terminate stata command if it doesn't converge
    converged <- suppressWarnings(tryCatch({
        fscaret::timeout(timeout_stata(iv_vars, stata_output_vars, gof_names, dfx, file_id, verbose = verbose),
                         seconds = 5)},
        error=function(e) {list(result = NULL, pid = Sys.getpid())}))
                             
    ## converged <- withTimeout(timeout_stata(iv_vars, stata_output_vars, gof_names, dfx, file_id, verbose = verbose),
    ##                          timeout = 1)

    ## converged <- T
    proc_dir <- paste0(PID_DIR, converged$pid)
    
    rmdir_cmd <- paste0("rm -r ", proc_dir)
    system(rmdir_cmd)

    
    if (is.null(converged$result)) {
        df_idx$cvrgd <- 0 # if converged is null, it means the convergence failed
        other_cfgs$cvrgd <- 0
        print("convergence failed")
    
        cleanup_old_stata_procs()
        
    
    } else {
        df_idx$cvrgd <- 1
        other_cfgs$cvrgd <- 1
    }

    ## save id to df_id to keep track
    write.table(df_idx, file = REG_RES_FILE_LAGS, append = T, col.names = F, row.names = F)
    write.table(other_cfgs, file = REG_RES_FILE_CFGS, append = T, col.names = F, row.names = F)

    ## write model id to end file to debug convergence failure
    write.table(file_id, MDL_END_FILE, append = T, col.names = F, row.names = F)
    
}

## run_cbn <- function(cbn_vars, base_vars, ctrl_vars, cbn_name, reg_spec) {
gen_spec_mdl_info <- function(reg_spec) {
    #' run a combination
    
    
    ## print(paste0("run cbn ", which(names(vrbl_cbns) == reg_spec$cbn_name)))

    ## df_cbn <- cbn_dfs[[reg_spec$cbn_name]]

    ## generate the models
    ## cbn_models <- gen_cbn_models(cbn_vars, base_vars, ctrl_vars)

    cbn_models <- gen_cbn_models(reg_spec$spec_cbn, reg_spec$base_vars, reg_spec$ctrl_vars)
    
     
    ## lapply(names(cbn_models), \(x) run_vrbl_mdl_vars(cbn_models[[x]], df_cbn, cbn_name, mdl_name = x, reg_spec))

    specs_mod <- lapply(names(cbn_models), \(x) c(reg_spec,
                                                  list(mdl_name = x, mdl_vars = cbn_models[[x]])))
    
    ## %>%
    ## rbindlist() %>% atb()
    
    return(specs_mod)

}


    

gen_spec_cbn_info <- function(reg_spec, base_vars) {
    #' run a specification

    

    spec_vars <- apply(reg_spec$lngtd_vrbls, 1, \(x) paste0(x[["vrbl"]], "_lag", x[["lag"]]))

    rel_vars_spec <- c(base_vars, crscn_vars, spec_vars)

    spec_cbns <- gen_cbns(rel_vars_spec)

    
    ## generate specification-specific control vars
    ctrl_vars <- setdiff(spec_cbns$cbn_controls, base_vars)

    spec_cbn_names <- names(spec_cbns)
    names(spec_cbn_names) <- spec_cbn_names

    
    
    reg_specs_mod <- lapply(spec_cbn_names, \(i) c(reg_spec,
                                                   list(spec_cbn=spec_cbns[[i]],
                                                        base_vars = base_vars,
                                                        ctrl_vars = ctrl_vars,
                                                        cbn_name = i)))

                                                      
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



vary_spec <- function(reg_spec){
    #' for a spec, vary each variable along all lags 
    
    base_lag_spec <- paste0(gen_lag_id(reg_spec)$value, collapse = "")

    varied_specs <-
        lapply(reg_spec[["lngtd_vrbls"]]$vrbl[reg_spec[["lngtd_vrbls"]]$vrbl != "ti_tmitr_interact"],
               \(x)
               lapply(seq(1,5),
                      \(t)
                      list(
                          ## "lngtd_vrbls" = mutate(reg_spec[["lngtd_vrbls"]],
                          ##                         lag = ifelse(vrbl == x, t, lag)),

                          "lngtd_vrbls" = replace_vlue(reg_spec$lngtd_vrbls, vrbl = x, lag = t),

                           
                           "vrbl_varied" = x,
                           "lag_len" = t,
                           "base_lag_spec" = base_lag_spec

                           )))


    
    varied_specs_long <- Reduce(\(x, y) c(x,y), varied_specs) %>% unique()
    ## varied_specs_long <- varied_specs

    return (varied_specs_long)
}




## make sure select isn't masked
select <- dplyr::select
lag <- dplyr::lag
timeout <- fscaret::timeout

base_vars <- c("iso3c", "year")
crscn_vars <- c("sum_core", "cnt_contemp_1995")
hnwi_vars <- sapply(hnwi_cutoff_vlus, \(x) paste0("hnwi_nbr_", sanitize_number(x)))
inc_ineq_vars <- c("sptinc992j_p90p100", "sptinc992j_p99p100", "gptinc992j")
weal_ineq_vars <- c("shweal992j_p90p100", "shweal992j_p99p100", "ghweal992j")
density_vars <- c("nbr_opened_cum", "nbr_opened_cum_sqrd")



## non_thld_lngtd_vars <- c("tmitr_approx_linear20step", "ti_tmitr_interact", "smorc_dollar_fxm", "NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "clctr_cnt_cpaer")

ctrl_vars <- c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "clctr_cnt_cpaer", "cnt_contemp_1995")
ctrl_vars_lngtd <- ctrl_vars[ctrl_vars %!in% crscn_vars]
ti_vars <- c("tmitr_approx_linear20step", "ti_tmitr_interact")
cult_spending_vars <- c("smorc_dollar_fxm")

non_thld_lngtd_vars <- c(ti_vars, cult_spending_vars, ctrl_vars_lngtd, density_vars)
lngtd_vars <- c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars)
all_rel_vars <- unique(c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars, crscn_vars))


vrbl_cbns <- gen_cbns(all_rel_vars)

cbn_dfs <- gen_cbn_dfs(lngtd_vars, crscn_vars, vrbl_cbns)

vrbl_thld_choices <- gen_vrbl_thld_choices(hnwi_vars, inc_ineq_vars, weal_ineq_vars)

## ** running with hopefully better ids


batch_version <- "v19"
REG_MONKEY_DIR <- "/home/johannes/ownCloud/reg_res/"
REG_RES_DIR <- paste0(REG_MONKEY_DIR,  batch_version, "/")
REG_RES_FILE_LAGS <- paste0(REG_MONKEY_DIR, batch_version, "_lags.csv")
REG_RES_FILE_CFGS <- paste0(REG_MONKEY_DIR, batch_version, "_cfgs.csv")
REG_SPEC_DIR <- paste0(REG_MONKEY_DIR, batch_version, "_specs/")
MDL_START_FILE <- paste0(REG_MONKEY_DIR, batch_version, "_start.csv")
MDL_END_FILE <- paste0(REG_MONKEY_DIR, batch_version, "_end.csv")
## generate existing dirs: have to normalizePath (remove //), and paste additional "/" at the end reeee
existing_dirs <- paste0(normalizePath(list.dirs(REG_MONKEY_DIR, recursive = F)), "/")
if (REG_RES_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_RES_DIR))}
if (REG_SPEC_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_SPEC_DIR))}
PID_DIR <- "/home/johannes/pid_dir/"
   

library(purrr)
## generate basic spec of lag, variable and threshold choices
t1 = Sys.time()
NBR_SPECS <- 30
reg_specs <- lapply(seq(1,NBR_SPECS), \(x) gen_reg_spec(non_thld_lngtd_vars)) %>% unique() #
## generate variations of basic reg_spec
reg_spec_varyns <- mclapply(reg_specs, vary_spec, mc.cores = 6) %>% flatten()
## reg_spec_varyns2 <- mclapply(reg_specs, vary_spec, mc.cores = 6) %>% Reduce(\(x,y) c(x,y), .)
## add the combination info
reg_spec_cbns <- mclapply(reg_spec_varyns, \(x) gen_spec_cbn_info(x, base_vars), mc.cores = 6) %>% flatten()
## reg_spec_cbns <- sapply(reg_spec_varyns, \(x) gen_spec_cbn_info(x, base_vars))
## mclapply is actually slower here because Reduce() is needed, and reducing tens of thousands of lists single-threadedly is slower than using sapply on single core
## guess I could split reg_spec_varyns manually into sections, each called with sapply, overall with mclapply: then I have just mc.cores number of lists, so Reduce should be quick
## flatten() from purrr is actually faster lol 
## add the model info
## reg_spec_mdls <- sapply(reg_spec_cbns, gen_spec_mdl_info)
reg_spec_mdls <- mclapply(reg_spec_cbns, gen_spec_mdl_info, mc.cores = 6) %>% flatten()
## same issue here: mclapply with Reduce is slow, but with flatten it's faster :)))
t2 = Sys.time()
print(t2-t1)



## run_vrbl_mdl_vars(reg_spec_mdls[[2]])
## gen_mdl_id(reg_spec_mdls[[2]])

mclapply(reg_spec_mdls,run_vrbl_mdl_vars, mc.cores = 6)


## *** decently fast analysis of tmitr_interaction having same value as main value
jj <- lapply(reg_spec_mdls, \(x) t(x$lngtd_vrbls[x$lngtd_vrbls$vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact"),])[2,]) %>% Reduce(\(x,y) rbind(x,y), .) %>% atb(.name_repair = ~c("v1", "v2"))

jj <- mclapply(reg_spec_mdls, \(x) t(x$lngtd_vrbls[x$lngtd_vrbls$vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact"),])[2,], mc.cores = 6)
kk <- do.call(rbind, jj) %>% atb(.name_repair = ~c("v1", "v2"))
filter(kk, v1 != v2)


## ps() %>% filter(name == "R", pid != Sys.getpid()) %>% pull(ps_handle) %>% lapply(ps_kill)

## ** tax incentive interaction debugging
reg_spec <- reg_spec_mdls[[1]]



t1 <- Sys.time()
x <- lapply(reg_spec_mdls[1:100], \(x) x$lngtd_vrbls)
            ## \(x) filter(x$lngtd_vrbls, vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact")))
            ## %>%
            ## pivot_wider(names_from = vrbl, values_from = lag))
## %>% rbindlist()
t2 <- Sys.time()
print(t2-t1)

filter(x, tmitr_approx_linear20step != ti_tmitr_interact)
## not always the same 


## ** pid bugfixing

lapply(reg_spec_mdls[1:10],run_vrbl_mdl_vars)

timeout_test <- function() {
    cur_dir <- getwd()
    pid <- Sys.getpid()
    new_dir = paste0(PROJECT_DIR, "pid_dir/", pid)

    system(paste0("mkdir ", new_dir))
    setwd(new_dir)
    

    x <- 1+1
    print(x)

    setwd(cur_dir)
    
}

timeout(timeout_test(), seconds = 2)


## ** multi_lag_testing

test_mdl1 <- reg_spec_mdls[[1]]

test_mdl1$mdl_vars <- c(test_mdl1$mdl_vars, "sptinc992j_p99p100_lag2", "sptinc992j_p99p100_lag3", "sptinc992j_p99p100_lag4", "sptinc992j_p99p100_lag5", "smorc_dollar_fxm_lag1", "smorc_dollar_fxm_lag3", "smorc_dollar_fxm_lag4", "smorc_dollar_fxm_lag5")

run_vrbl_mdl_vars(test_mdl1, verbose = T)
## huh that works pretty well.... maybe all my work was for nothing?
## maybe should have been more 

test_mdl2 <- reg_spec_mdls[[3]]
test_mdl2$mdl_vars <- c(test_mdl2$mdl_vars, "sptinc992j_p99p100_lag2", "sptinc992j_p99p100_lag3", "sptinc992j_p99p100_lag4", "sptinc992j_p99p100_lag5", "hnwi_nbr_1B_lag2", "hnwi_nbr_1B_lag3", "hnwi_nbr_1B_lag4")
run_vrbl_mdl_vars(test_mdl2, verbose = T)

## full model doesn't run
## also not when removing clctr_cnt_cpaer_lags
## also not when also removing additional population lags
## also not when removing additional GDP lags
## also not when removing additional shweal lags
## runs after also removing additional hnwi_lags
## runs with 1 and 2 additional hnwi_lags, breaks down when adding third additional lag
## -> seems my hunch was completely correct

## ** clustering

get_df_clust <- function() {
    #' generate the dataframe used for clustering 
    df_clust_prep <- df_reg %>%
        filter(year > 1995) %>% 
        select(iso3c, year, NY.GDP.PCAP.CDk, sptinc992j_p99p100, shweal992j_p99p100, sum_core, cnt_contemp_1995,
               hnwi_nbr_30M, SP.POP.TOTLm) %>%
        mutate(cnt_contemp_1995 = cnt_contemp_1995/SP.POP.TOTLm,
               hnwi_nbr_30M = hnwi_nbr_30M/SP.POP.TOTLm) %>%
        select(-SP.POP.TOTLm) %>% 
        na.omit()


    df_clust <- df_clust_prep %>%
        pivot_wider(id_cols = iso3c, names_from = year, values_from = setdiff(names(df_clust_prep), base_vars)) %>%
        na.omit() ## ugly

    return(df_clust)
}



dists <- dist(df_clust)

run_cluster <- function(dists, method) {

    clusts <- hclust(dists, method = method)
    ## plot(clusts)
    table(cutree(clusts, k=8))
}


clust_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")

lapply(clust_methods, \(x) run_cluster(dists, x))


## * scrap 
## ** first sloppy version 

REG_RES_DIR <- "/home/johannes/ownCloud/reg_res/v5/"
REG_RES_FILE <- "/home/johannes/ownCloud/reg_res/v5.csv"



## generate 100 specs
reg_specs <- lapply(seq(1,600), \(x) gen_reg_spec(non_thld_lngtd_vars)) %>% unique()

## for each of the 100, generate the spec variations -> 3.7k total 
all_spec_variations <- lapply(reg_specs, \(x) vary_spec(x))

## flatten the 3.7k 
all_specs_flat <- Reduce(\(x, y) c(x,y), all_spec_variations) %>% unique()


## t1 = Sys.time()
## lapply(all_specs_flat, \(x) run_spec(x, base_vars))
## t2 = Sys.time()


## gets stuck after ~70 models, which isn't even a complete spec per thread..
t1 = Sys.time()
mclapply(all_specs_flat, \(x) run_spec(x, base_vars), mc.cores = 6)
t2 = Sys.time()

print(t2-t1)


