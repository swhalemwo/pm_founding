## * generate rates

proc_csnt_cnts_to_rts <- function(df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' process the constant count variables (for now only MOW museum counts at some year) to rates
    #' rates are calculated with the corresponding MOW year 
    #' then rates stays constant across years for every country:
    #' is intended to measure the development of PMs at 1995

    
    ## get constant count variables: first check if they start with cnt, then if they have a number
    ## relies on !current! naming format of MOW variables: e.g. cnt_contemp_2020
    cstnt_cnt_vrbls <- keep(names(df_reg), ~ grepl("^cnt_", .x)) %>% keep(~grepl("\\d", .x))

    ## make separate df to not fuck up too much 
    dt_reg_cstnt_cnts <- df_reg %>% select(iso3c, year, SP.POP.TOTLm, all_of(cstnt_cnt_vrbls)) %>% adt()
    
    ## dt_reg_cstnt_cnts[iso3c == "ARM"]

    ## maybe melting and casting is more comfy
    ## yup then only keep relevant years, and later join to main df -> blowing up 

    dt_reg_cstnt_cnts_melt <- melt(dt_reg_cstnt_cnts, id.vars = c("iso3c", "year", "SP.POP.TOTLm"),
                                   value.name = "muem_cnt") %>%
        .[, cnt_year := as.numeric(str_extract(variable, "\\d+"))]

    
    ## get all the mow data in long format
    mow_prep <- unique(dt_reg_cstnt_cnts_melt[, .(iso3c, variable, muem_cnt, cnt_year)])
    
    pop_prep <- df_reg %>% select(iso3c, year, SP.POP.TOTLm) %>% adt()
    ## join population data to mow data (so that NAs in pop data are not yeeted)
    pop_cbn <- pop_prep[mow_prep, on = .(iso3c, year = cnt_year)]

    ## assessing badness of lacking population resulting in less countries for cnt_contemp_1995
    ## pop_cbn[is.na(SP.POP.TOTLm) & muem_cnt != 0 & year == 1995]
    ## pop_cbn[is.na(SP.POP.TOTLm) & muem_cnt != 0, .N, variable]q
    ## seems only to be the case for contemporary for 1985
    ##-> no country that has contemp museums in 1995 has missing population data

    ## fill up CYs that have no POP data but have 0 museums with muem_rt = 0
    pop_cbn_fld <- pop_cbn %>% copy() %>% .[, muem_rt := muem_cnt/SP.POP.TOTLm] %>%
        .[is.na(SP.POP.TOTLm) & muem_cnt == 0, muem_rt := 0] %>%
        ## update squared rate: first un-square (taking square root) of cnt, then re-calc rate, then square
        .[grepl("_squared", variable), muem_rt := (sqrt(muem_cnt)/SP.POP.TOTLm)^2]
    
        
    ## pop_cbn_fld[iso3c %in% c("KOR", "DEU") & grepl("cnt_contemp_2010", variable)]

    ## armenia 1985 still gone but around 200 are not, and none in 1995 contemporary

    mow_rts <- dcast.data.table(pop_cbn_fld[, .(iso3c, variable, muem_rt)], iso3c ~ variable, value.var = "muem_rt")
    
    ## mow_rts$cnt_contemp_2020 %>% hist(breaks = 80)
    ## look at distribution: nice lovely skewed
    ## hist(mow_rts$cnt_contemp_1995, breaks = 30)
    ## hist(mow_rts[cnt_contemp_1995 < 3]$cnt_contemp_1995, breaks = 50)
    ## mow_rts[cnt_contemp_1995 > 3]
    ## mow_rts$cnt_contemp_1995
    ## compare with first try: no more NAs in cnt_contemp_1995

    return(mow_rts)
    
}



gen_df_reg_rts <- function(df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the df_reg with rates instead of counts

    ## list the count variables manually... i love getting into tech debt
    cnt_vrbls1 <- c("pm_density", "nbr_closed_cum",
                    ## "pm_density_global", "pm_density_global_sqrd", "nbr_closed_cum_global", 
                    "cnt_contemp", "cnt_art", "cnt_all",
                    "clctr_cnt_cpaer", "clctr_cnt_all", "smorc_dollar_fxm",
                    "hnwi_nbr_1M", "hnwi_nbr_5M", "hnwi_nbr_30M", "hnwi_nbr_200M", "hnwi_nbr_1B")
        
    x_rt <- adt(df_reg) %>%
        .[, lapply(.SD, \(x) x/SP.POP.TOTLm), by = .(iso3c, year),
          .SDcols = cnt_vrbls1]

    # generate squared variables (square the rates), 
    sqrd_vrbls <- c("pm_density_sqrd", "smorc_dollar_fxm_sqrd")
    dt_sqrd <- x_rt %>% copy() %>% .[, lapply(.SD, \(x) x^2), by=c("iso3c","year"),
                                     .SDcols = gsub("_sqrd", "", sqrd_vrbls)]
    names(dt_sqrd) <- c("iso3c", "year", sqrd_vrbls) # need to set names manually

    ## first merge the count and squared variables
    dt_cbn <- x_rt[on=dt_sqrd]
    
    ## viz_lines(filter(atb(dt_cbn), iso3c %!in% c("MCO", "LIE")), y="pm_density")
    ## map(cbn_dfs_rates, ~filter(.x, iso3c=="ISL") %>% nrow())'
    ## map(cbn_dfs_rates, ~filter(.x, iso3c=="ISL")$nbr_opened %>% sum())
    

    mow_rts <- proc_csnt_cnts_to_rts(df_reg)

    check_df_name_unqns(list(x_rt, mow_rts, dt_sqrd), skip_var_names = c("iso3c", "year"))

    ## then merge count+sqrd coutns + mow constant rates
    rts_cbn <- mow_rts[dt_cbn, on = "iso3c"]
    ## rts_cbn[, .N, is.na(cnt_contemp_1990)]
    
    ## rts_cbn[iso3c %in% c("DEU", "KOR") & year == 2020, .(iso3c, pm_density, pm_density_sqrd)]
    

    rt_names <- setdiff(names(rts_cbn), c("iso3c", "year"))

    df_reg_rts <- df_reg %>% select(-rt_names) %>% # get the non-population scaled variables 
        inner_join(atb(rts_cbn), on = c(iso3c, year)) ## combine them with the population scaled vrbls
    
    ## check that all columns in df_reg are also in df_reg_rts
    if (len(intersect(names(df_reg_rts), names(df_reg))) != len(names(df_reg))) {
        print("something wrong with rescaling to rates")}

    


    return(df_reg_rts)

    
    



    ## exploring NA differences between pct_cutoff_30M and hnwi_nbr_30M, is due to NAs in SP.POP.TOTLm
    ## but only in Eritrea, 9 years total -> nobody cares bro
    ## x1 <- adt(df_reg) %>% .[pct_cutoff_30M == 0]
    ## x2 <- adt(df_reg) %>% .[hnwi_nbr_30M == 0]

    ## x1[!x2, on =c("iso3c", "year"), .(iso3c, year, hnwi_nbr_30M, pct_cutoff_30M, SP.POP.TOTLm)]



    ## explore the different ways of calculating hnwi rates
    ## x <- adt(df_reg) %>% .[, .(hnwi_pcap1 = pct_cutoff_1M * 1e4,
    ##                            hnwi_pcap2 = hnwi_nbr_1M/SP.POP.TOTLm,
    ##                            iso3c, year, SP.POP.TOTLm, pct_cutoff_1M)] %>%
    ##     .[, `:=`(diff_sum = hnwi_pcap1 - hnwi_pcap2, diff_mlt = hnwi_pcap1/hnwi_pcap2)]

    ## cor(x$hnwi_pcap1, x$hnwi_pcap2, use = "complete.obs")
    ## hist(x$diff_sum, breaks = 100)
    ## hist(x$diff_mlt, breaks = 100)
    ## hist(x$hnwi_pcap1)
    ## hist(df_reg$pct_cutoff_1M)

    ## x[diff_sum < -1]
    


    ## explore different ways of calculating rates of squared variables,
    ## decide to go for first squaring and then calculating the population rate of that square
    ## x <- df_reg %>%
    ##     select(iso3c, year, nbr_opened, nbr_opened_cum, nbr_opened_cum_sqrd, SP.POP.TOTLm) %>%
    ##     mutate(nbr_opened_ssd = nbr_opened_cum_sqrd/SP.POP.TOTLm,
    ##            nbr_opened_sqrd_rt = (nbr_opened_cum/SP.POP.TOTLm)^2)

    ## plot(x$nbr_opened_ssd, x$nbr_opened_sqrd_rt)
    ## cor(x$nbr_opened_ssd, x$nbr_opened_sqrd_rt, use = "complete.obs")    

    ## df_reg$nbr_opened_cum
    
    ## x_rt$nbr_opened_cum


    ## .[, region := countrycode(iso3c, "iso3c", "un.region.name")]


    
    ## viz_lines(atb(x_rt), y="nbr_opened_cum", facets = "region")
    ## viz_lines(atb(x_org), y="nbr_opened_cum", facets = "iso3c")

}



## hist(df_reg_rts$hnwi_nbr_1M, breaks = 100)
## hist(df_reg$hnwi_nbr_1M, breaks = 100)
