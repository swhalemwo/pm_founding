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

## ** variable completeness checks across percentiles




## **** middle classes
## ***** pre-checking the variables to do, not much decrease
wid_inc_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wid_v2 WHERE ilike(varx, '%inc%' )"
wid_inc_vars <- as_tibble(dbGetQuery(con, wid_inc_vars_cmd))
unique(wid_inc_vars)

perc_splits <- as_tibble(do.call(rbind.data.frame, strsplit(wid_inc_vars$percentile, "p")))[,c(2,3)]
names(perc_splits) <- c("perc_low", "perc_high")

wid_inc_vars$perc_low <- as.numeric(perc_splits$perc_low)
wid_inc_vars$perc_high <- as.numeric(perc_splits$perc_high)
wid_inc_vars

filter(wid_inc_vars, perc_high == 100)

filter(wid_inc_vars, perc_high < 100)$percentile

inc_table <- table(wid_inc_vars$percentile)
inc_table[rev(order(inc_table))][c(0:100)]

wid_inc_vars$diff <- mutate(wid_inc_vars, diff=perc_high-perc_low)$diff
hist(wid_inc_vars$diff, breaks=50)

ggplot(filter(wid_inc_vars, perc_high < 100 & diff < 30), aes(y=perc_high, x=perc_low)) +
    geom_jitter(width=3, height=3, size=0.2)

## most percentiles are about very small steps
## -> need even additional step of seeing how well variables are covered in terms of percentiles
##

list_of_ranges <- list(c(1,40), c(30,40), c(80,90))


x <- head(wid_inc_vars)$percentile

split_percs <- function(percentile){
    
    print('------')
    print(length(percentile))
    perc_splits2 <- do.call(rbind.data.frame, strsplit(percentile, "p"))[c(2,3)]

    
    names(perc_splits2) <- c("low", "high")
    perc_splits2 <- apply(perc_splits2, 2, as.numeric)

    print(perc_splits2)

    nrox <- nrow(perc_splits2)
    groups <- seq(nrox)
    print(nrox)
    print(groups)
    if (length(percentile) ==1) {
        list_of_ranges <- list(perc_splits2)
    } else {
        list_of_ranges <- split(perc_splits2, groups)
    }
    return (list_of_ranges)
    }
    
split_percs("p99p100")

split_percs(c("p99p100","p1p2"))



check_ranges <- function(list_of_ranges){
    #' see how well a variable is covered
    cvrg <- length(which(c(0:100) %in% unique(unlist(lapply(list_of_ranges, function(x) (c(x[1]:x[2])))))))
    ## print(cvrg)
    nbr_ranges <- length(list_of_ranges)
    ## print(nbr_ranges)
    avg_len <- mean(unlist(lapply(list_of_ranges, function(x) x[2]-x[1])))
    ## print(avg_len)
    return(list(cvrg = cvrg,
           nbr_ranges = nbr_ranges,
           avg_len = avg_len))
    }

list_of_ranges <- split_percs(wid_inc_vars$percentile[c(0:5)])
check_ranges(list_of_ranges)

        
check_cvrg <- function(percentile){
    list_of_ranges <- split_percs(percentile)
    cvrg_res <- check_ranges(list_of_ranges)
    ## print(list_of_ranges)
    print(cvrg_res)
    ## return(paste0('c', 'b'))
    return(paste0(cvrg_res, collapse="--"))
    
}

check_cvrg(list("p0p30"))


aggregate(percentile ~ variable, filter(wid_inc_vars, variable == "sptinc992j", diff < 20), check_cvrg)

wid_inc_var_cvrg <- as_tibble(aggregate(percentile ~ variable, filter(wid_inc_vars, diff < 20), check_cvrg))

## splitting results back, assigning to results
wid_inc_var_cvrg_split <- apply(do.call(rbind, strsplit(wid_inc_var_cvrg$percentile, '--')), 2, as.numeric)
wid_inc_var_cvrg$cvrg <- wid_inc_var_cvrg_split[,1]
wid_inc_var_cvrg$nbr_ranges <- wid_inc_var_cvrg_split[,2]
wid_inc_var_cvrg$avg_len <- wid_inc_var_cvrg_split[,3]
wid_inc_var_cvrg <- wid_inc_var_cvrg[,-c(2)]
filter(wid_inc_var_cvrg, cvrg > 80)$cvrg
## down from 62 to 39


filter(wid_inc_vars, variable %in% filter(wid_inc_var_cvrg, cvrg > 80)$variable)
## hmm ok not that great a reduction: from 12.4k to 10.7k

## fuck i'll still have to check all the subscale variables separately
## but can still help to do variable completeness first to reduce number of sub-variables I have to run
## also probably similar infrastructure

## ***** actual running the checks
    
combos_inc <- split(wid_inc_vars, seq(nrow(wid_inc_vars)))
inc_res <- mclapply(combos_inc, wid_cpltns_check_wrapper, mc.cores = 4)

inc_res_df <- as_tibble(apply(Reduce(function(x,y,...) rbind(x,y,...), inc_res), 2, unlist))
    ## sloppy converting numbers back to numeric

inc_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")] <- apply(inc_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")], 2, as.numeric)

## write.csv(inc_res_df, file = paste0(TABLE_DIR, "inc_res_df.csv"))

ggplot(inc_res_df, aes(x=variable, y=PMs_covered_raw)) +
    geom_boxplot() 
    ## geom_jitter(width = 0.1, size=0.5)
## ok there are like 11 variables that have good coverage
## hope they have good coverage in terms of percentiles covered

inc_res_tbl <- aggregate(PMs_covered_raw ~ variable, inc_res_df, mean)
ttl_cvrg_good <- filter(inc_res_tbl, PMs_covered_raw > 400)$variable

pctl_cvrg_good <- filter(wid_inc_var_cvrg, cvrg > 80)$variable

intersect(ttl_cvrg_good, pctl_cvrg_good)
## all about pre-tax income
## but there I have average, share and thresholds -> should be sufficient to get some estimates of middle class


check_wid_cpltns("sptinc992j", "p60p70")    

                    
## **** check whether coverage depends on percentile chosen, doesn't really

pctl_checker <- function(percentile){
    #' next level wrapper function: just use shr variables
    wdi_cpltns_x <- lapply(as.list(shr_variables), check_wdi_cpltns, percentile)
    wdi_res_df <- do.call(rbind.data.frame, wdi_cpltns_x)
    wdi_res_df$percentile <- percentile
    
    return(wdi_res_df)
}

## pctl_checker("p99.5p100")


if (REDO_WDI_CPLTNS_CHK){
    ## could have been that some variables are good at some exotic percentages, but they aren't
    ## sptinc992j still the only variable with decent coverage 

    percentiles <- as_tibble(dbGetQuery(con, "select percentile, count(percentile) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    pctls_cry <- as_tibble(dbGetQuery(con, "select percentile, count(distinct(country)) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    ## just select a bunch of percentiles for coverage checks?
    ## nah that would be stupid, there could be some good coverage in some that i'd miss
    ## use all that have first percentile above 80 and second == 100

    pctls_cry$pct1 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][2]})))
    pctls_cry$pct2 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][3]})))

    pctls_relevant <- filter(pctls_cry, pct1 >=80 & pct2==100)$percentile



    pctl_cpltns_res <- mclapply(pctls_relevant, pctl_checker, mc.cores = 6)
    
    pctl_cpltns_df <- as_tibble(Reduce(function(x,y,...) rbind(x,y,...), pctl_cpltns_res))

    as.data.frame(filter(pctl_cpltns_df, PMs_covered_raw > 250))[,-c(which(names(wdi_df_res_90) == "most_affected_crys"))]

    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, max)
    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, mean)

    as.data.frame(filter(pctl_cpltns_df, variable == "sptinc992j"))
}



## same with share data: only sufficient global coverage for ptinc992j, diinc992j second 

## dbDisconnect(con)

## dbListTables(con)

## checking whether i don't accidentally skip some country-years, apparently not
## aggregate(year ~ country, multi_inner, max)
## aggregate(year ~ country, multi_inner, min)
## aggregate(year ~ country, multi_inner, len)




## ** old WID completeness checks

## *** WDI completeness checks

## **** gini variables
if (REDO_WDI_CPLTNS_CHK == TRUE){
    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")
    gini_variables <- dbGetQuery(con, "select distinct(variable) from wdi where first_letter=='g'")

    gini_data <- as_tibble(dbGetQuery(con, "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where first_letter='g'"))

    table(gini_data$percentile)
    check_wdi_cpltns("gptinc992j", "p0p100")

    gini_res <- lapply(as.list(gini_variables$variable), check_wdi_cpltns, "p0p100")
    gini_res_df <- do.call(rbind.data.frame,gini_res)
}


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
