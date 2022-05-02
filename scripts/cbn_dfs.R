## * combine dfs

check_dupl_names <- function(df1_name, df2_name) {
    #' diagnosing whether some names are not unique
    #' non-unique names are surrounded by ----; collision appears in preceding df pair
    
    names1 <- names(dfs_to_combine[[df1_name]])
    names2 <- names(dfs_to_combine[[df2_name]])
    
    print(paste0(df1_name, "-", df2_name))

    names_intersect <- intersect(names1, names2)

    if (!setequal(names_intersect, c("iso3c", "year")) & !setequal(names_intersect, c("iso3c"))) {
        ## print(setdiff(names_intersect, c("iso3c", "year")))
        ## print(names_intersect)

        print("---------")
        print(names_intersect)
        ## print(names1)
        ## print(names2)
        print("---------")
        
    }

    NULL
    ## else {
    ##     ## print("no overlap")
    ## }
}

    





get_df_reg <- function(df_anls) { #
    #' combine together df_anls, inequality, tax incentive, mow, hnwi

    df_anls$NY.GDP.PCAP.CDk <- df_anls$NY.GDP.PCAP.CD/1000
    df_anls$SP.POP.TOTLm <- df_anls$SP.POP.TOTL/1e6

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

    

    ## marginal tax rates 
    df_mtr <- construct_mtrs()

    ## artnews '
    df_artnews <- generate_artnews_data()
    

    ## cultural spending
    df_cult <- gen_cult_spending()
    
    
    ## need to fill NAs of MOW with 0s

    df_hnwi <- get_hnwi_pcts()

    ## combine everything
    dfs_to_combine <- list(df_anls=df_anls, df_hnwi=df_hnwi, df_ineq=df_ineq, df_taxinc=df_taxinc,
                           mow_crssctn=mow_crssctn, mow_cntns=mow_cntns,df_mtr=df_mtr,
                           df_artnews=df_artnews, df_cult=df_cult)

    
    
    ## checking that names are unique
    all_col_names <- unname(unlist(sapply(dfs_to_combine, names)))
    nbr_cols_unq <- len(unique(all_col_names)) - 2 ## subtract 2 for iso3c and year
    nbr_cols_ttl <- sum(sapply(dfs_to_combine, \(x) ncol(x[,c(names(x) %!in% c("iso3c", "year"))])))


    if (nbr_cols_unq != nbr_cols_ttl) {
        df_combns <- as.data.frame(t(combn(names(dfs_to_combine), 2)))
        apply(df_combns, 1, \(x) check_dupl_names(x[["V1"]], x[["V2"]]))
        stop("you have duplicate column names")
    }
    

    df_reg <- as_tibble(Reduce(function(x,y,...) merge(x,y, all.x = TRUE), dfs_to_combine))

    ## fill up MOW data, maybe other databases later (IDA, Artfacts)
    fill_up_cols <- unique(c(names(mow_cntns), names(mow_crssctn), names(df_artnews)))
    
    df_reg[fill_up_cols][is.na(df_reg[fill_up_cols])] <- 0
    

    return(df_reg)
}
