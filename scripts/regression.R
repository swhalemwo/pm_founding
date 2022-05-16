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

    lngtd_vars <- c(x$hnwi_var,
                    x$inc_ineq_var,
                    x$weal_ineq_var,
                    non_thld_lngtd_vars) %>% atb() %>%
        select(vrbl = value) %>%
        group_by(vrbl) %>% 
        mutate(lag = sample(seq(1,5),1))

    ## lag TI*TMITR interaction with same lag as TMITR
    lngtd_vars[which(lngtd_vars$vrbl == "ti_tmitr_interact"),]$lag <- filter(lngtd_vars, vrbl == "tmitr_approx_linear_2020step")$lag

    return(lngtd_vars)
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
        cbn_no_cult_spending_and_mitr = rel_vars[!grepl("smorc_dollar_fx|tmitr_approx_linear_2020step|ti_tmitr_interact", rel_vars)],
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

    all_cbn_models <- c(single_var_models, models)
    return(all_cbn_models)
}



    
gen_cbn_dfs <- function(lngtd_vars, crscn_vars, vrbl_cnbs) {
    #' generate the dfs that correspond to variable combinations
    #' checks whether a country-year has coverage for all the lags for all the variables required by combination
    #' needs lngtd_vars and crscn_vars to set which to variables need to be named as lag

    cvrg_lags <- lapply(lngtd_vars, \(x) lapply(seq(1,5), \(i) gen_lag(vrbl=x, lag=i) %>%
                                                               select(iso3c, year, value =paste0(x, "_lag", i)) %>%
                                                               mutate(lag=i, vrbl = x, lag_col = "_lag")) %>%
                                         Reduce(\(x,y) rbind(x,y), .)) %>%
        Reduce(\(x,y) rbind(x,y), .)

    cvrg_crscn <- lapply(crscn_vars, \(x) select(df_reg, iso3c, year, value = x) %>%
                                      mutate(vrbl = x, lag_col = "", lag=""))

    ## convert lag to string to allow having empty string for lag of cross-sectional variables
    cvrg_lags_crscn <- rbind(cvrg_lags %>% mutate(lag = as.character(lag)) %>%
                             select(iso3c, year, vrbl, value, lag_col, lag),
                             cvrg_crscn) %>% atb()


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

    cbn_dfs <- lapply(cbn_cvrg, \(x) atb(merge(select(x, iso3c, year), df_all_lags)))

    return(cbn_dfs)
}

run_vrbl_mdl_vars <- function(mdl_vars, df_cbn) {
    #' run one regression given the model vars

    ## mdl_vars <- vrbl_cnbs[["no_cult_spending_and_mitr"]]
    
    ## right hand side of formula

    ## f_rhs <- paste0(mdl_vars[mdl_vars %!in% ctrl_vars], collapse = " + ") %>%
    f_rhs <- paste0(mdl_vars[mdl_vars %!in% base_vars], collapse = " + ") %>%        
        paste0(., " + (1|iso3c)")
                                                                           
    f <- paste0("nbr_opened ~ ", f_rhs)

    ## print(f)

    ## now run f on df_cbn

    return(list(formula=f, nrow=nrow(df_cbn)))

}

run_cbn <- function(cbn_vars, base_vars, ctrl_vars, cbn_name) {
    #' run a combination

    df_cbn <- cbn_dfs[[cbn_name]]

    ## generate the models
    cbn_models <- gen_cbn_models(cbn_vars, base_vars, ctrl_vars)
 
    lapply(cbn_models, \(x) run_vrbl_mdl_vars(x, df_cbn)) %>% rbindlist() %>% atb()
   
}


    

run_spec <- function(reg_spec, base_vars) {
    #' run a specification

    spec_vars <- apply(reg_spec, 1, \(x) paste0(x[["vrbl"]], "_lag", x[["lag"]]))

    rel_vars_spec <- c(base_vars, crscn_vars, spec_vars)

    spec_cbns <- gen_cbns(rel_vars_spec)
    
    ## generate specification-specific control vars
    ctrl_vars <- setdiff(spec_cbns$cbn_controls, base_vars)

    spec_cbn_names <- names(spec_cbns)
    names(spec_cbn_names) <- spec_cbn_names

    lapply(spec_cbn_names, \(x) run_cbn(spec_cbns[[x]], base_vars, ctrl_vars, x))

}

base_vars <- c("iso3c", "year")
crscn_vars <- c("sum_core", "cnt_contemp_1995")

hnwi_vars <- sapply(hnwi_cutoff_vlus, \(x) paste0("hnwi_nbr_", sanitize_number(x)))
inc_ineq_vars <- c("sptinc992j_p90p100", "sptinc992j_p99p100", "gptinc992j")
weal_ineq_vars <- c("shweal992j_p90p100", "shweal992j_p99p100", "ghweal992j")

vrbl_thld_choices <- gen_vrbl_thld_choices(hnwi_vars, inc_ineq_vars, weal_ineq_vars)

non_thld_lngtd_vars <- c("tmitr_approx_linear_2020step", "ti_tmitr_interact", "smorc_dollar_fxm", "NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "clctr_cnt_cpaer")

reg_spec <- gen_reg_spec(non_thld_lngtd_vars)
lngtd_vars <- c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars)

all_rel_vars <- unique(c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars, crscn_vars))

vrbl_cbns <- gen_cbns(all_rel_vars)

cbn_dfs <- gen_cbn_dfs(lngtd_vars, crscn_vars, vrbl_cbns)


run_spec(reg_spec, base_vars)


## df_reg_lags <- gen_lag_df(reg_spec, crscn_vars, base_vars)





## generate the combinations with a bunch of grepling 






screenreg(glmer.nb(f, data = df_reg_lags))
