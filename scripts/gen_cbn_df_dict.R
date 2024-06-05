
gen_vrbl_vectors <- function() {
    #' generate the variable vectors (group of variables): use only one large global (?) object

    

    base_vars <- c("iso3c", "year")
    ## crscn_vars <- c("sum_core", "cnt_contemp_1995")
    crscn_vars <- c("Ind.tax.incentives", # "NPO.tax.exemption",
                    "cnt_contemp_1990", "cnt_contemp_1990_sqrd")

    ## for now manually exclude the 1B threshold

    hnwi_vars <- sapply(hnwi_cutoff_vlus[1:4], \(x) paste0("hnwi_nbr_", sanitize_number(x)))
    inc_ineq_vars <- c("sptinc992j_p90p100", "sptinc992j_p99p100", "gptinc992j")
    weal_ineq_vars <- c("shweal992j_p90p100", "shweal992j_p99p100", "ghweal992j")
    ## density_vars <- c("nbr_opened_cum", "nbr_opened_cum_sqrd")
    density_vars <- c("pm_density", "pm_density_sqrd", # "nbr_closed_cum",
                      "pmdens_neigh", "pmdens_neigh_sqrd",
                      "pm_density_global", "pm_density_global_sqrd", "nbr_closed_cum_global")


    ## non_thld_lngtd_vars <- c("tmitr_approx_linear20step", "ti_tmitr_interact", "smorc_dollar_fxm", "NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "clctr_cnt_cpaer")

    ctrl_vars <- c("NY.GDP.PCAP.CDk",
                   "NY.GDP.PCAP.KD.ZG",
                   "SP.POP.TOTLm",
                   "clctr_cnt_cpaer",
                   "cnt_contemp_1990",
                   "cnt_contemp_1990_sqrd",
                   "hief",
                   "SM.POP.TOTL.ZS")
    
    ctrl_vars_lngtd <- ctrl_vars[ctrl_vars %!in% crscn_vars]
    ti_vars <- c("tmitr_approx_linear20step", "ti_tmitr_interact")
    cult_spending_vars <- c("smorc_dollar_fxm", "smorc_dollar_fxm_sqrd")

    non_thld_lngtd_vars <- c(ti_vars, cult_spending_vars, ctrl_vars_lngtd, density_vars)
    lngtd_vars <- c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars)
    all_rel_vars <- unique(c(hnwi_vars, inc_ineq_vars, weal_ineq_vars, non_thld_lngtd_vars, crscn_vars))

    vrbl_lbls_cnts <- c("nbr_opened" = "Number of Private Museums opened",
                        ## "sum_core" = "Tax incentives",
                        "cons" = "cons",
                   "hdi" = "Human Development Index", 
                   "NPO.tax.exemption" = "Tax exemption of non-profits",
                   "Ind.tax.incentives" = "Tax deductibility of donations",
                   "tmitr_approx_linear20step" = "Marginal Income Tax Rate (%)",
                   "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax deductibility",
                   ## "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax deductibility",
                   "hnwi_nbr_1M" = "# HNWIs with net worth >= 1M",
                   "hnwi_nbr_5M" = "# HNWIs with net worth >= 5M",
                   "hnwi_nbr_30M" = "# HNWIs with net worth >= 30M",
                   "hnwi_nbr_200M" = "# HNWIs with net worth >= 200M",
                   "hnwi_nbr_1B" = "# HNWIs with net worth >= 1B",
                   "sptinc992j_p90p100" = "Income share of top 10%",
                   "sptinc992j_p99p100" = "Income share of top 1%",
                   "gptinc992j" = "Gini of pre-tax income",
                   "ghweal992j"= "Gini of net wealth",
                   "shweal992j_p90p100" = "Wealth share of top 10%",
                   "shweal992j_p99p100" = "Wealth share of top 1%",
                   "smorc_dollar_fxm" = "Gvt cultural spending (millions)",
                   "smorc_dollar_fxm_sqrd" = "Gvt cultural spending (millions, sqrd)",
                   "NY.GDP.PCAP.CDk" = "GDP per capita (thousands)",
                   "NY.GDP.PCAP.KD.ZG" = "GDP growth (%)",
                   "SP.POP.TOTLm" = "Population (millions)",
                   "cnt_contemp" = "# openings of contemporary art museums", 
                   "cnt_contemp_1985" = "# Museums of contemporary art in 1985",
                   "cnt_contemp_1995" = "# of modern/contemp. art museums in 1995",
                   "cnt_contemp_1995_squared" = "# of contemp. art museums (1995) (sqrd)",
                   "cnt_contemp_1990" = "# of modern/contemp. art museums in 1990",
                   "cnt_contemp_1990_sqrd" = "# of contemp. art museums (1990) (sqrd)",
                   "clctr_cnt_cpaer" = "# Collectors in Artnews collector list",
                   "nbr_opened_cum" = "cumulative openings (legitimacy)",
                   "nbr_opened_cum_sqrd" = "cumulative openings squared (competition)",
                   "pm_density" = "PM density (country)",
                   "pm_density_sqrd" = "PM density squared (country)", 
                   "nbr_closed_cum" = "# PM closings (cumulative, country)",
                   "pmdens_neigh" = "PM density neighbors",
                   "pmdens_neigh_sqrd" = "PM density squared (neighbors)",
                   "pm_density_global" =  "PM density (global)",
                   "pm_density_global_sqrd" = "PM density squared (global)", 
                   "nbr_closed_cum_global" = "# PM closings (cumulative, global)",
                   "ln_s" = "ln(s)",
                   "ln_r" = "ln(r)"
                   )

    vrbl_lbls <- c("nbr_opened" = "Number of Private Museums opened",
                   "nbr_opened_prop" = "Private Museum openings", 
                   "cons" = "Intercept",
                   "(Intercept)" = "Intercept",
                   ## "sum_core" = "Tax incentives",
                   "hdi" = "Human Development Index", 
                   "NPO.tax.exemption" = "Tax exemption of non-profits",
                   "Ind.tax.incentives" = "Tax deductibility of donations",
                   "tmitr_approx_linear20step" = "Top Marginal Income Tax Rate (%)",
                   ## "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax deductibility of donations",
                   "ti_tmitr_interact" = "Tax deductibility * Top Marg. Inc. TR ",
                   ## "hnwi_nbr_1M" = "# HNWIs with net worth >= 1M",
                   ## "hnwi_nbr_5M" = "# HNWIs with net worth >= 5M",
                   ## "hnwi_nbr_30M" = "# HNWIs with net worth >= 30M",
                   ## "hnwi_nbr_200M" = "# HNWIs with net worth >= 200M",
                   ## "hnwi_nbr_1B" = "# HNWIs with net worth >= 1B",
                   "smorc_dollar_fxm" = "Gvt cultural spending (millions)",
                   "smorc_dollar_fxm_sqrd" = "Gvt cultural spending^2",
                   "sptinc992j_p90p100" = "Income share of top 10% (*100)",
                   "sptinc992j_p99p100" = "Income share of top 1% (*100)",
                   "gptinc992j" = "Gini of pre-tax income (*100)",
                   "shweal992j_p90p100" = "Wealth share of top 10% (*100)",
                   "shweal992j_p99p100" = "Wealth share of top 1% (*100)",
                   "ghweal992j"= "Gini of net wealth (*100)",
                   "hnwi_nbr_1M" = "HNWIs (net worth >= 1M)",
                   "hnwi_nbr_5M" = "HNWIs (net worth >= 5M)",
                   "hnwi_nbr_30M" = "HNWIs (net worth >= 30M)",
                   "hnwi_nbr_200M" = "HNWIs (net worth >= 200M)",
                   ## "hnwi_nbr_1B" = "HNWIs (net worth >= 1B)",
                   "NY.GDP.PCAP.CDk" = "GDP per cap. (thousands)",
                   "NY.GDP.PCAP.KD.ZG" = "GDP growth (%)",
                   "SP.POP.TOTLm" = "Population (millions)",
                   "cnt_contemp" = "Nbr. openings of contemporary art museums per cap(M)", 
                   ## "cnt_contemp_1985" = "Nbr. Museums of contemporary art in 1985 per cap(M)",
                   ## "cnt_contemp_1995" = "Nbr. of modern/contemp. art museums in 1995 per cap(M)",
                   ## "cnt_contemp_1995_squared" = "Nbr. of contemp. art museums 1995 per cap (M,squared)",
                   "cnt_contemp_1990" = "modern/contemp. art museums (1990)",
                   "cnt_contemp_1990_sqrd" = "modern/contemp. art museums^2 (1990)",
                   "clctr_cnt_cpaer" = "Collectors in Artnews top 200 collector list",
                   ## "nbr_opened_cum" = "Nbr. cumulative openings per 1M pop. (legitimacy)",
                   ## "nbr_opened_cum_sqrd" = "Nbr. cumulative openings per per 1M pop. squared (competition)",
                   "pm_density" = "PM density (country)",
                   ## "pm_density_sqrd" = "PM density squared (country)",
                   "pm_density_sqrd" = "PM density^2 (country)",
                   "pmdens_neigh" = "PM density (neighbors)",
                   "pmdens_neigh_sqrd" = "PM density^2 (neighbors)",
                   ## "nbr_closed_cum" = "Nbr. PM closings per cap. (cumulative, country)",
                   "pm_density_global" =  "PM density (global)",
                   "pm_density_global_sqrd" = "PM density^2 (global)", 
                   "nbr_closed_cum_global" = "Nbr. PM closings (cumulative, global)",
                   "hief" = "Ethnic Fractionalization",
                   "SM.POP.TOTL.ZS" = "Migrant stock",
                   "ln_s" = "ln(s)",
                   "ln_r" = "ln(r)",
                   all_dens = "density variables",
                   all_dens_close = "density variables + closings"
                   )

    cbn_lbls <- c("cbn1" = "DS all IVs",      
                  "cbn2" = "DS --CuSp",       
                  "cbn3" = "DS --CuSp/TMITR")


    regcmd_lbls <- c(xtnbreg = "xtnbreg",
                     glmmTMB = "glmmTMB",
                     menbreg = "menbreg")

    hyp_lbls <- c(
        "h1a" = "H1: more Tax exemption of non-profits -> more PMs",
        "h1b" = "H1: more Tax deductibility of donations -> more PMs",
        "h2a" = "H2a: more Gvt spending -> less PMs",
        "h2b" = "H2b: more Gvt spending -> more PMs",
        "h3a" = "H3: more income inequality -> more PMs",
        "h3b" = "H3: more wealth inequality -> more PMs",
        "h4"  = "H4: more (U/V)HNWIs -> more PMs")

    ## original labels, now getting shortened
    ## krnl_lbls <- c(
    ##     "h1a" = "H1: Tax exemption of non-profits",
    ##     "h1b" = "H1: Tax deductibility of donations",
    ##     "h2" = "H2: Gvt cultural spending",
    ##     "h3a" = "H3: Income inequality variables",
    ##     "h3b" = "H3: Wealth inequality variables",
    ##     "h4"  = "H4: (U/V)HNWIs",
    ##     "zcontrols" = "Controls") 
    
    krnl_lbls <- c(
        "h1a" = "H1:\nNPO Tax exemption",
        ## "h1b" = "H1:\nTax deductibility\nof donations",
        "h1b" = "H1:\nTax incentives",
        "h2"  = "H2:\nCultural spending",
        "h3a" = "H3:\nIncome inequality",
        "h3b" = "H3:\nWealth inequality",
        "h4"  = "H4:\n(U/V)HNWIs",
        "zcontrols" = "Controls") 

    ## hypothesis memberships
    hyp_mep <- c(
        c("NPO.tax.exemption"         = "h1a"),
        c("tmitr_approx_linear20step" = "h1b"),
        c("ti_tmitr_interact"         = "h1b"),
        c("Ind.tax.incentives"        = "h1b"),
        c("smorc_dollar_fxm"          = "h2"),
        c("smorc_dollar_fxm_sqrd"     = "h2"),
        c("gptinc992j"                = "h3a"),
        c("sptinc992j_p90p100"        = "h3a"),
        c("sptinc992j_p99p100"        = "h3a"),
        c("ghweal992j"                = "h3b"),
        c("shweal992j_p90p100"        = "h3b"),
        c("shweal992j_p99p100"        = "h3b"),
        c("hnwi_nbr_1M"               = "h4"),
        c("hnwi_nbr_5M"               = "h4"),
        c("hnwi_nbr_30M"              = "h4"),
        c("hnwi_nbr_200M"             = "h4"),
        c("cons"                      = "cons"),
        c("(Intercept)"                 = "cons"),
        c("cnt_contemp_1990"          = "zcontrols"),
        c("cnt_contemp_1990_sqrd"     = "zcontrols"),
        c("NY.GDP.PCAP.CDk"           = "zcontrols"),
        c("NY.GDP.PCAP.KD.ZG"         = "zcontrols"),
        c("clctr_cnt_cpaer"           = "zcontrols"),
        c("pm_density"                = "zcontrols"),
        c("pm_density_sqrd"           = "zcontrols"),
        c("pmdens_neigh"              = "zcontrols"),
        c("pmdens_neigh_sqrd"         = "zcontrols"),
        c("pm_density_global"         = "zcontrols"),
        c("pm_density_global_sqrd"    = "zcontrols"),
        c("nbr_closed_cum_global"     = "zcontrols"),
        c("all_dens"                  = "zcontrols"), ## also add some ou-sets
        c("all_dens_close"            = "zcontrols"),
        c("hief"                      = "zcontrols"),
        c("SM.POP.TOTL.ZS"            = "zcontrols"))
        
        
          
    hyp_mep_dt <- data.table(vrbl = names(hyp_mep), hyp = hyp_mep) %>%
        .[, vrbl := factor(vrbl, levels = rev(names(vrbl_lbls)))] # modify variable order

    
    ## constrain lag of variables: used for interactions and squared variables
    dt_cstrnd_vrbls <- list(
        list(cstrnd = "ti_tmitr_interact", dtrmnr = "tmitr_approx_linear20step"),
        list(cstrnd = "smorc_dollar_fxm_sqrd", dtrmnr = "smorc_dollar_fxm"),
        list(cstrnd = "pm_density_sqrd", dtrmnr = "pm_density"),
        list(cstrnd = "pmdens_neigh_sqrd", dtrmnr = "pmdens_neigh"),
        list(cstrnd = "pm_density_global_sqrd", dtrmnr = "pm_density_global")) %>% rbindlist()



    if (!all(all_rel_vars %in% names(vrbl_lbls))) {
        stop("not all relevant variables have a variable label")
    }

    return(list(
        base_vars = base_vars,
        crscn_vars = crscn_vars,
        hnwi_vars = hnwi_vars,
        inc_ineq_vars = inc_ineq_vars,
        weal_ineq_vars = weal_ineq_vars,
        density_vars = density_vars,
        ctrl_vars = ctrl_vars,
        ctrl_vars_lngtd = ctrl_vars_lngtd,
        ti_vars = ti_vars,
        cult_spending_vars = cult_spending_vars,

        non_thld_lngtd_vars = non_thld_lngtd_vars,
        lngtd_vars = lngtd_vars,
        lngtd_vars_df = data.frame(lngtd_vars),
        all_rel_vars = all_rel_vars,
        vrbl_lbls = vrbl_lbls,
        vrbl_lbls_cnts = vrbl_lbls_cnts,
        cbn_lbls = cbn_lbls,
        regcmd_lbls = regcmd_lbls,
        hyp_lbls = hyp_lbls,
        krnl_lbls = krnl_lbls,
        hyp_mep_dt = hyp_mep_dt,
        dt_cstrnd_vrbls = dt_cstrnd_vrbls
    )
    )
}

gen_cbns <- function(rel_vars, base_vars) {
    ## gw_fargs(match.call())
    #' generate the combinations
    #' should be own function to allow the combination generation when generating the dfs (broad) and within specification

    vrbl_cbns <- list(
        cbn1=rel_vars,
        cbn2 = rel_vars[!grepl("smorc_dollar_fx", rel_vars)],
        cbn3 = rel_vars[!grepl("smorc_dollar_fx|tmitr_approx_linear20step|ti_tmitr_interact", rel_vars)],
        cbn_controls = rel_vars[rel_vars %in% c(base_vars) | grepl("SP.POP.TOTLm|NY.GDP.PCAP.CDk", rel_vars)])

    ## attr(vrbl_cbns, "gnrtdby") <- as.character(match.call()[[1]])
    return(vrbl_cbns)
    
}


gen_vrbl_thld_choices <- function(hnwi_vars, inc_ineq_vars, weal_ineq_vars) {
    ## gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate threshold (hnwi, shares 


    vrbl_choices <- expand.grid(hnwi_var = hnwi_vars,
                                inc_ineq_var = inc_ineq_vars,
                                weal_ineq_var = weal_ineq_vars, stringsAsFactors = F) %>%
        atb()

    dt_vrbl_choices <- vrbl_choices %>% adt() %>%
        .[, `:=`(hnwi_code = str_extract(hnwi_var, "[0-9]+"),
                 inc_ineq_code = fifelse(substring(inc_ineq_var,1,1) == "g", "g",
                                         str_extract(inc_ineq_var, "(\\d)+(?=p)")),
                 weal_ineq_code = fifelse(substring(weal_ineq_var,1,1) == "g", "g",
                                          str_extract(weal_ineq_var, "(\\d)+(?=p)")))] %>% 
        .[, .(hnwi_var, inc_ineq_var, weal_ineq_var,
              vrblchoice_id = sprintf("hn%sii%swi%s", hnwi_code, inc_ineq_code, weal_ineq_code))]

    ## attr(dt_vrbl_choices, "gnrtdby") <- as.character(match.call()[[1]])
    return(dt_vrbl_choices)


}

gen_cbn_dfs <- function(df_regx, lngtd_vars, crscn_vars, vrbl_cnbs, base_vars) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## gw_fargs(match.call())
    #' generate the dfs that correspond to variable combinations
    #' checks whether a country-year has coverage for all the lags for all the variables required by combination
    #' needs lngtd_vars and crscn_vars to set which to variables need to be named as lag

    

    cvrg_lags <- lapply(lngtd_vars, \(x)
                        lapply(seq(0,5), \(i)
                               gen_lag(dfx = df_regx, vrbl=x, lag=i) %>%
                               select(iso3c, year, value =paste0(x, "_lag", i)) %>%
                               mutate(lag=i, vrbl = x, lag_col = "_lag")) %>%
                        Reduce(\(x,y) rbind(x,y), .)) %>%
        Reduce(\(x,y) rbind(x,y), .)

    ## filter(cvrg_lags, iso3c %in% c("CHE", "USA"), vrbl == "hnwi_nbr_5M", lag == 0) %>% adf()

    cvrg_crscn <- lapply(crscn_vars, \(x) select(df_regx, iso3c, year, value = all_of(x)) %>%
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

    ## split up? 
    cbn_dfs <- lapply(cbn_cvrg, \(x) atb(merge(select(all_of(x), iso3c, year), df_all_lags)))

    ## attr(cbn_dfs, "gnrtdby") <- as.character(match.call()[[1]])

    return(cbn_dfs)
}


scale_cbn_dfs <- function(cbn_dfs, base_vars, df_regx) {
    ## gw_fargs(match.call())
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' scale cbn dfs

    ## add unscaled Population at lag 0: needed for rates/exposures(use Stata naming scheme)
    cbn_dfs2 <- lapply(cbn_dfs, \(x) mutate(x, SP_POP_TOTLm_lag0_uscld = SP.POP.TOTLm_lag0))
           
    ## scale variables

    vrbls_to_not_scale <- c("Ind.tax.incentives", "NPO.tax.exemption", "SP_POP_TOTLm_lag0_uscld")
    
    cbn_dfs3 <- lapply(cbn_dfs2, \(x) mutate(x,
                                           across(all_of(setdiff(names(x), c(vrbls_to_not_scale, base_vars))),
                                                  scale_wo_attr)) %>%
                                    mutate(iso3c_num = as.numeric(factor(iso3c))))

    ## sd(cbn_dfs$cbn_all$ti_tmitr_interact_lag1)
    ## mean(cbn_dfs$cbn_all$ti_tmitr_interact_lag1)
    ## sd(cbn_dfs$cbn_all$tmitr_approx_linear20step_lag1)
    ## mean(cbn_dfs$cbn_all$tmitr_approx_linear20step_lag1)


    ## re-calculate the interaction of tax incentives and top marginal income tax rates:
    ## effectively makes it bigger, so coefs should be smaller

    ## first calculate interaction values
    
    ti_tmitr_interactions <- lapply(cbn_dfs3, \(x)
           lapply(seq(0,5), \(lagx)
                  x %>% 
                  mutate(!!paste0("ti_tmitr_interact_lag", lagx) :=
                             get(paste0("tmitr_approx_linear20step_lag", lagx)) * Ind.tax.incentives) %>% 
                  select(!!paste0("ti_tmitr_interact_lag", lagx))) %>%
           Reduce(\(x,y) cbind(x,y), .) %>% atb())
           
    ## then replace old and with new interaction values

    cbn_dfs_names <- names(cbn_dfs3)
    names(cbn_dfs_names) <- cbn_dfs_names

    cbn_dfs4 <- lapply(cbn_dfs_names, \(x) 
           cbind(cbn_dfs3[[x]] %>% select(-all_of(names(ti_tmitr_interactions[[x]]))),
                 ti_tmitr_interactions[[x]]) %>% atb())


    ## update the squared terms: recalculate them on the basis of the squared variables

    cbn_dfs5 <- lapply(cbn_dfs4, \(dtx) # loop over dfs of co
                       adt(dtx)[, .SD^2, # get vrbls that have sqrd, and square their non-squared, scaled form
                                .SDcols = gsub("_sqrd", "", keep(names(dtx), ~grepl("_sqrd", .x)))] %>%
           setnames(old = gsub("_sqrd", "", keep(names(dtx), ~grepl("_sqrd", .x))), # give newly squared vrbls
                    new = keep(names(dtx), ~grepl("_sqrd", .x))) %>% # proper squared names 
           cbind(adt(dtx)[, .SD, .SDcols = setdiff(names(dtx), names(.))], .) %>% atb()) # replace old by new sqrd

 
    ## inspection that transformation went well 
    ## dtx2 <- cbn_dfs5$cbn_all %>% adt()
    ## melt(dtx2, id.vars = c("iso3c", "year")) %>%
    ## .[, .(mean = mean(value), sd = sd(value)), variable] %>% print(n=200)
        
    ## vrbl_sqrd <- "pm_density_global_sqrd_lag0"
    ## vrbl_base <- "pm_density_global_lag0"
    
    ## mean(dtx2[[vrbl_base]]) # 0
    ## sd(dtx2[[vrbl_base]]) # 1
    ## mean(dtx2[[vrbl_sqrd]]) # should be 1
    ## sd(dtx2[[vrbl_sqrd]]) # sd_calc (=sd_check)
    ## mean((dtx2[[vrbl_base]])^2) # should be 1
    ## sd((dtx2[[vrbl_base]]^2)) # sd_check (=sd_calc)

    ## add iso3c_num to make stata happy 
    ## cbn_dfs <- lapply(cbn_dfs, \(x) mutate(x, iso3c_num = as.numeric(factor(iso3c))))

    ## add nbr_opened
    cbn_dfs6 <- lapply(cbn_dfs5, \(x) merge(x, select(df_regx, iso3c, year, nbr_opened), all.x = T) %>% atb())

    ## attr(cbn_dfs6, "gnrtdby") <- as.character(match.call()[[1]])
    return(cbn_dfs6)
}


gen_lag <- function(dfx, vrbl, lag) {
    ## gw_fargs(match.call())
    
    #' lag vrbl by lag years

    lag <- as.numeric(lag)
    lag_name <- paste0(vrbl, "_lag", lag)

    df_lag <- dfx %>% select(iso3c, year, !!vrbl) %>%
        group_by(iso3c) %>%
        mutate(!!lag_name := dplyr::lag(get(vrbl), lag)) %>%
        select(iso3c, year, !!lag_name)

    ## attr(df_lag, "gnrtdby") <- as.character(match.call()[[1]])
    return(df_lag)
}

## if (identical(args, character(0))) {
##     stop("functions are done")
## }


## if (is.null(args[[1]])) {
##     stop("functions are DONE")
## }




