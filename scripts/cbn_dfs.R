
get_df_reg <- function(df_anls) {
    #' combine together df_anls, inequality, tax incentive, mow, hnwi

    df_anls$NY.GDP.PCAP.CDk <- df_anls$NY.GDP.PCAP.CD/1000

    ## cumulative number of opened
    df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$iso3c, FUN = cumsum)


    df_anls$nbr_opened_cum_sqrd <- (df_anls$nbr_opened_cum^2)/100
    ## have to divide by 100 otherwise R glmer.nb complains

    ## PMs opened per 1m people -> rate
    df_anls$nbr_opened_prop <- df_anls$nbr_opened/(df_anls$SP.POP.TOTL/1e6)

    ## filter(df_anls, nbr_opened_prop > 1)
    ## iceland, monaco, cyprus LUL


    ## inequalities
    df_ineq <- get_all_ineqs()

    ## tax incentives
    df_taxinc <- get_taxinc_dfs()

    ## mow

    mow_res <- get_mow_dfs()
    mow_crssctn <- mow_res$mow_crssctn
    mow_cntns <- mow_res$mow_cntns

    ## need to fill NAs of MOW with 0s


    df_hnwi <- get_hnwi_pcts()

    ## combine everything
    df_reg <- as_tibble(Reduce(function(x,y,...) merge(x,y, all.x = TRUE),
                               list(df_anls, df_hnwi, df_ineq, df_taxinc, mow_crssctn, mow_cntns)))

    ## fill up MOW data, maybe other databases later (IDA, Artfacts)
    fill_up_cols <- unique(c(names(mow_cntns), names(mow_crssctn)))
    
    df_reg[fill_up_cols][is.na(df_reg[fill_up_cols])] <- 0
    

    return(df_reg)
}
