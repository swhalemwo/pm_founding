## ** pretty printing/export function for WID completeness check tables
    wealth_res_df2 <- wealth_res_df2[,c('variable', 'variable_label', 'PMs_covered_raw', 'cry_cvrg_geq3', 'nbr_of_crys_geq3', 'nbr_of_crys_geq1pm')]

    names(wealth_res_df2) <- c("variable", "meaning", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")



        wealth_res_df2,
        label="wid_cpltns",
        caption = "coverage variables for top decile in WID",
        align = c("p{2cm}", "l", "p{5.5cm}","p{1.5cm}", rep("p{2.25cm}", 3)),
        digits=0)



    print(
        xtbl,
        include.rownames = F,
        file = paste0(TABLE_DIR, "wid_wealth_cpltns.tex"),
        )

## ** old WID completeness checks

## *** WDI completeness checks
## **** income

x <- tbl(con, "wdi") %>%
    filter(varx == "hweal") %>%
    group_by(percentile) %>%
    summarise(length(percentile))





## could add something like how complete the countries are that are not fully complete to assess how difficult imputation will be 





REDO_WID_CPLTNS_CHK <- FALSE
if (REDO_WID_CPLTNS_CHK){


    check_wid_cpltns("wealg")
    check_wid_cpltns("labsh")
    check_wid_cpltns("wealp")
    check_wid_cpltns("weali")
    check_wid_cpltns("wealc")
    check_wid_cpltns("wealh")
    check_wid_cpltns("sfiinc992i", "p90p100")
    check_wid_cpltns("sfiinc992j", "p90p100")

    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

    shr_variables_p90 <- dbGetQuery(con, "select distinct(variable) from wid_v2 where percentile='p90p100' and first_letter='s'")[[1]]
    wid_cpltns_res_90 <- lapply(as.list(shr_variables_p90), check_wid_cpltns, "p90p100")
    wid_df_res_90 <- do.call(rbind.data.frame, wid_cpltns_res_90)
    wid_df_res_90_tbl <- wid_df_res_90[,-c(which(names(wid_df_res_90) == "most_affected_crys"))]
    names(wid_df_res_90_tbl)[c(2:5)] <- lapply(names(wid_df_res_90_tbl)[c(2:5)], function(x) paste0(x, "_90"))


    shr_variables_p99 <- dbGetQuery(con, "select distinct(variable) from wdi where percentile='p99p100' and first_letter='s'")[[1]]
    wid_cpltns_res_99 <- lapply(as.list(shr_variables_p99), check_wid_cpltns, "p90p100")
    wid_df_res_99 <- do.call(rbind.data.frame, wid_cpltns_res_99)
    wid_df_res_99_tbl <- wid_df_res_99[,-c(which(names(wid_df_res_99) == "most_affected_crys"))]
    names(wid_df_res_99_tbl)[c(2:5)] <- lapply(names(wid_df_res_99_tbl)[c(2:5)], function(x) paste0(x, "_99"))

    wid_tbl <- merge(wid_df_res_90_tbl, wid_df_res_99_tbl, by=c("variable"))


    ## manual check that 99% percentile is as good as 90% percentile 
    wid_tbl[,c("variable", "PMs_covered_raw_90", "PMs_covered_raw_99",
               "cry_cvrg_geq3_90", "cry_cvrg_geq3_99",
               "nbr_of_crys_geq3_90", "nbr_of_crys_geq3_99",
               "nbr_of_crys_geq1pm_90", "nbr_of_crys_geq1pm_99")]
    ## but don't go further into putting it into table
                  

    wid_df_res_90_tbl <- wid_df_res_90_tbl[rev(order(wid_df_res_90_tbl$PMs_covered_raw))[c(1:10)],]
    names(wid_df_res_90_tbl) <- c("variable", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")

    wid_df_res_90_tbl$variable <- recode(wid_df_res_90_tbl$variable, 
           "sptinc992j" = "pretax income (equal-split adults = based on household)",
           "sdiinc992j" = "post-tax income (equal-split adults)",
           "scainc992j"= "post-tax disposable income (equal-split adults)",
           "sfiinc992t"= "fiscal income (threshold)",
           "sfiinc992j"= "fiscal income (equal-split adults)",
           "sfiinc992i"= "fiscal income (individuals)",
           "shweal992j"= "net wealth (equal-split adults)",
           "sptinc992i"= "pretax income (individuals)",
           "sdiinc992i"= "post-tax income (individuals)",
           "sfiinc999t"= "fiscal income (threshold)")
    
    xtbl <- xtable(
        wid_df_res_90_tbl,
        label="wid_cpltns",
        caption = "coverage variables for top decile in WID",
        align = c("l", "p{5.5cm}","p{2cm}", rep("p{3cm}", 3)),
        digits=0)

    

    print(
        xtbl,
        include.rownames = F,
        file = paste0(TABLE_DIR, "wid_cpltns.tex"),
        )

}

## **** wealth 

wid_cpltns_check_wrapper <- function(combo){
    ## print(combo)
    return(check_wid_cpltns(combo$variable, combo$percentile))
}


REDO_WID_WEALTH_CHECKS <- FALSE
if (REDO_WID_WEALTH_CHECKS){

    wid_wealth_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wdi WHERE ilike(varx, '%weal%' )"
    wid_wealth_vars <- as_tibble(dbGetQuery(con, wid_wealth_vars_cmd))
    unique(wid_wealth_vars)

    check_wid_cpltns("mgweal999i", "p0p100")

    combos <- split(wid_wealth_vars, seq(nrow(wid_wealth_vars)))


    check_wid_cpltns(combos[[1]][[1]][1], combos[[1]][[1]][2])
    wid_cpltns_check_wrapper(combos[[1]])

    wealth_res <- mclapply(combos, wid_cpltns_check_wrapper, mc.cores = 6)
    wealth_res_df <- as_tibble(apply(Reduce(function(x,y,...) rbind(x,y,...), wealth_res), 2, unlist))
    ## sloppy converting numbers back to numeric
    wealth_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")] <- apply(wealth_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")], 2, as.numeric)


    wealth_res_df2 <- as.data.frame(filter(wealth_res_df, PMs_covered_raw > 250))[,-c(which(names(wealth_res_df) %in% c("most_affected_crys", "percentile")))]

    wealth_res_df2$variable_label <- recode(wealth_res_df2$variable,
                                            "apweal999i" = "average individual wealth of combined sector (households, NPISH)",
                                            "anweal999i" = "average individual wealth of national economy",
                                            "wwealn999i" = "wealth-to-income ratio of national economy",
                                            "mpweal999i" = "total wealth of combined sector",
                                            "apweal992i" = "average individual wealth of combined sector (above 20 y/o)",
                                            "mgweal999i" = "total net wealth of general government",
                                            "wwealg999i" = "wealth-to-income ratio of national economy",
                                            "wwealp999i" = "net private wealth to net national income ratio",
                                            "anweal992i" = "average individual wealth of national economy (above 20 y/o)",
                                            "agweal999i" = "average individual net wealth of general government",
                                            "mnweal999i" = "total individual wealth of national economy",
                                            "agweal992i" = "average individual net wealth of general government")

}
