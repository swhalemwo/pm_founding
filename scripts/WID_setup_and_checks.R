

## * WID
## ** setting up CH


## *** check country codes compatibility between WID and WB

df_gdp <- get_WB_data("NY.GDP.PCAP.CD")

countrycodes2c <- na.omit(countrycode(unique(df_gdp$iso3c), "iso3c", "iso2c"))
countrycodes3c <- na.omit(unique(df_gdp$iso3c))

## issues with CHI and XKX
## need to check what they actually refer in WB data
## then probably write some manual exceptions so that they get correctly translated from WB to WDI
## also need to check whether there are other mismatches between iso2c(wb) and WDI -> paste WDI (https://wid.world/codes-dictionary/#country-code)


compare_WID_WB_countrycodes <- function(df_wb, wid_dir) {
    #' manually necessary to check whether the iso2c countrycodes of the WID match the ones of the WB
    df_wb <- unique(df_wb[,c("country", "iso3c")])
    df_wb$iso2c <- countrycode(df_wb$iso3c, "wb", "iso2c")

    ## use "WID_countries.csv" instead of "countrycodes.csv", which wasn't present everywhere
    # use na.strings = "" to prevent "NA" (alpha2 for Namibia) to be read as NA
    wid_crycds <- as_tibble(read.csv(paste0(wid_dir, "WID_countries.csv"), sep = ";", header = T, na.strings = ""))
    
    df_crycd_mrg <- as_tibble(merge(df_wb, wid_crycds, by.x = "iso2c", by.y = "alpha2"))
    
    df_cry_mismatch <- as.data.frame(df_crycd_mrg[which(df_crycd_mrg$country != df_crycd_mrg$shortname),c("iso2c", "country", "titlename")])

    ## print(df_cry_mismatch)

    return(df_cry_mismatch)
}

compare_WID_WB_countrycodes(df_gdp, WID_DIR_v1)
compare_WID_WB_countrycodes(df_gdp, WID_DIR_v2)

## *** read into CH

read_WID_into_CH <- function(countrycodes3c, db_name, wid_dir) {
    #' read WID data into clickhouse for countrycodes3c with db name
    #' wid_dir is dir where all the individual WID country files are located

    wid_files <- list.files(wid_dir)

    ## dbGetQuery(con, "show tables")  ## this is how you can send arbitrary sql statements, hopefully
    existing_tables <- dbGetQuery(con, "show tables")
    
    if (db_name %in% existing_tables$name) {
        ## delete existing table if it exists
        dbGetQuery(con, paste0("drop table ", db_name))
    }

    create_tbl_cmd <- sprintf("CREATE TABLE %s (iso3c String, variable String, percentile String, year Int16,
value Float64, age Int16, pop String, varx String, first_letter String)
engine=MergeTree() 
PARTITION BY iso3c
ORDER BY tuple()", db_name) ## use proper MergeTree tables rather than default tinylogs

    dbGetQuery(con, create_tbl_cmd)
    
    for (code in countrycodes3c){
        ## manual exceptions for channel islands and kosovo
        if (code == "XKX") {
            cry_code2c <- "KV"
        } else if (code == "CHI"){
            cry_code2c <- "XI"
        } else {
            cry_code2c <- countrycode(code, "iso3c", "iso2c")
        }
        
        print(code)
        
        filename <- paste0("WID_data_", cry_code2c, ".csv")

        if (filename %in% wid_files){

            cry_data <- as_tibble(read.csv(paste(wid_dir, "WID_data_", cry_code2c, ".csv", sep=""), sep=";"))

            cry_data$country <- code
            cry_data$varx <- substring(cry_data$variable, 2, 6)
            cry_data$first_letter <- substring(cry_data$variable, 1,1)

            cry_data <- na.omit(cry_data) # lol why do you include NAs that I have to manually clean up???
            names(cry_data)[which(names(cry_data) == "country")] <- "iso3c" ## keep consistent variable names

            dbWriteTable(con, db_name, cry_data, append=TRUE)
        }
    }
    print("done")   
}

## read_WID_into_CH(countrycodes3c, "wid_v1", WID_DIR_v1)
## read_WID_into_CH(countrycodes3c, "wid_v2", WID_DIR_v2)



## ** completeness tests

## *** single and multi completeness check functions

check_wid_cpltns <- function(varx, percentile, base_df){
    #' check how well WID variables cover PM foundings ?
    #' 
    #' varx: complete WID variable name
    #'
    #' percentile: WID percentile specification, e.g. p90p100
    #'
    #' base df: the entire database in R memory
    
    
    varz <- varx ## need to assign to own objects to be able to filter on them 

    pctz <- percentile
    res <- filter(base_df, variable == varz & percentile == pctz)

    
    
    ## some exception to throw when too many variables
    if (length(table(res$variable)) > 1){
        print(varx)
        stop("too many variables")
    }

    ## print(nrow(res))
    if (nrow(res)!=0){
        
    ## make df base to merge WDI data to 
        dfb <- df_anls[,c("iso3c", "year", "nbr_opened")]

        dfc <- as_tibble(merge(dfb, res, by = c("year", "iso3c"), all.x = TRUE))
        cry_cvrg <- aggregate(year ~ iso3c, na.omit(dfc), length)
        crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$iso3c

        cry_pm_crvg_actual <- aggregate(nbr_opened ~ iso3c, na.omit(dfc), sum)
        cry_pm_crvg_ideal <- aggregate(nbr_opened ~ iso3c, dfc, sum)
        names(cry_pm_crvg_ideal) <- c("iso3c", "nbr_opened_ideal")
        
        cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
        cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
        cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

        ## get most affected countries, limit to 5 max
        most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
                                            function(x) (filter(cry_pm_cvrg_cprn, diff == x)$iso3c)))

        most_affected_crys2 <- unique(most_affected_crys)[1:min(len(most_affected_crys), 5)]


        ## country-year coverage of countries which have at least 3 WDI observations AND which have WDI data for that year
        opyrs <- sum(na.omit(dfc[which(dfc$iso3c %in% crys_geq3),])$nbr_opened) ## opening years covered

        ## coverage of countries which have at least three values, even if they don't have WDI data for years of museum founding
        cry_geq3 <- sum(filter(dfc, iso3c %in% crys_geq3)$nbr_opened)

        nbr_crys_geq3 <- len(crys_geq3)

        ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
        nbr_crys_geq1pm <- filter(aggregate(nbr_opened ~ iso3c, dfc, sum), iso3c %in% crys_geq3) %>%
            filter(nbr_opened >= 1) %>%
            nrow()

        return(list(
            variable = varx,
            percentile = percentile,
            opyrs = opyrs, 
            cry_geq3=cry_geq3,
            nbr_crys_geq3=nbr_crys_geq3,
            nbr_crys_geq1pm=nbr_crys_geq1pm,
            most_affected_crys = paste(most_affected_crys2, collapse = "--")))
    }
}
## check_wid_cpltns("sfiinc992i", "p90p100")


check_wid_cpltns_tuples <- function(df_tpls, base_df) {
    #' apply check_wid_cpltns to df of variables and percentiles
    ## apply(df_tpls, 1, function(x) print(paste0(x['variable'],"--", x['percentile'])))
    

    res <- apply(df_tpls, 1, function(x) check_wid_cpltns(x['variable'], x['percentile'],base_df))
    res_df <- as_tibble(rbindlist(res))

    return(res_df)

}

## *** exporting with proper names

wid_recode_list <- c("sptinc992j", "pretax income (equal-split adults = based on household)",
                 "sdiinc992j", "post-tax income (equal-split adults)",
                 "scainc992j", "post-tax disposable income (equal-split adults)",
                 "sfiinc992t", "fiscal income (threshold)",
                 "sfiinc992j", "fiscal income (equal-split adults)",
                 "sfiinc992i", "fiscal income (individuals)",
                 "shweal992j", "net wealth (equal-split adults)",
                 "sptinc992i", "pretax income (individuals)",
                 "sdiinc992i", "post-tax income (individuals)",
                 "sfiinc999t", "fiscal income (threshold)",
                 "apweal999i", "average individual wealth of combined sector (households, NPISH)",
                 "anweal999i", "average individual wealth of national economy",
                 "wwealn999i", "wealth-to-income ratio of national economy",
                 "mpweal999i", "total wealth of combined sector",
                 "apweal992i", "average individual wealth of combined sector (above 20 y/o)",
                 "mgweal999i", "total net wealth of general government",
                 "wwealg999i", "wealth-to-income ratio of national economy",
                 "wwealp999i", "net private wealth to net national income ratio",
                 "anweal992i", "average individual wealth of national economy (above 20 y/o)",
                 "agweal999i", "average individual net wealth of general government",
                 "mnweal999i", "total individual wealth of national economy",
                 "agweal992i", "average individual net wealth of general government")


wid_recode_codes <- wid_recode_list[seq(1, length(wid_recode_list)-1, 2)]
wid_recode_labels <- wid_recode_list[seq(2, length(wid_recode_list), 2)]

wid_recode_df <- as.data.frame(cbind(wid_recode_codes, wid_recode_labels))

wid_res_col_lbls <-c("variable", "meaning", "percentile", "opyrs", "cry_geq3", "nbr_crys_geq3", "nbr_crys_geq1pm", "most_affected_crys") ## column labels
wid_res_col_nms <- c("variable", "meaning", "percentile", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding", "countries most affected by missing data") ## full column names


## recode(res_df$variable,

## flexible recoding with vector
## https://stackoverflow.com/questions/49388313/recoding-values-based-on-two-vectors-levels-and-labels-with-identical-labels-a 

## seems to work also with variables that are not included: recodes them to NA



export_wid_cpltns_check <- function(res_df, tbl_label, tbl_caption) {
    #' export WID completeness check to latex with label/caption, variable meaning
    res_df <- add_column(res_df, meaning = wid_recode_labels[match(res_df$variable, wid_recode_codes)], .after = "variable")
    
    ##  print(res_df$meaning)

    names(res_df) <- wid_res_col_nms[match(names(res_df), wid_res_col_lbls)]

    xtbl <- xtable(res_df,
                   label = tbl_label,
                   caption = tbl_caption,
                   align= c("p{2cm}", "l", "p{5.5cm}","p{2cm}", rep("p{2.25cm}", 5)),
                   digits=0)
    ## return(xtbl)

    filename <- paste0(TABLE_DIR, tbl_label, ".tex")
    print(xtbl,
          include.rownames = F,
          file = filename,
          tabular.environment = 'longtable'
          )
}

## ** tests 

## compare v1 and v2 in terms of wealth variable coverage, v2 much better

## base_cmd_v1 <- "select iso3c, variable, percentile, year, first_letter, varx, value from wid_v1 where year >= 1985"
## base_df_v1 <- as_tibble(dbGetQuery(con, base_cmd_v1))

## base_cmd_v2 <- "select iso3c, variable, percentile, year, first_letter, varx, value from wid_v2 where year >= 1985"
## base_df_v2 <- as_tibble(dbGetQuery(con, base_cmd_v2))


## df_tpls_wealth_v1 <- dbGetQuery(con, "select distinct(variable),percentile from wid_v1 where ilike(varx, '%weal%') and first_letter = 's' and percentile = 'p99p100'")

## df_tpls_wealth_v2 <- dbGetQuery(con, "select distinct(variable),percentile from wid_v2 where ilike(varx, '%weal%') and first_letter = 's' and percentile = 'p99p100'")



## res_df_v1 <- check_wid_cpltns_tuples(df_tpls_wealth_v1, base_df_v1)
## res_df_v2 <- check_wid_cpltns_tuples(df_tpls_wealth_v2, base_df_v2)

## export_wid_cpltns_check(res_df_v1, "wid_wealth_v1", "wealth coverage in version 1")
## export_wid_cpltns_check(res_df_v2, "wid_wealth_v2", "wealth coverage in version 2")

## ** HWNI calculation

dbGetQuery(con, "show tables")

wid_wealth_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wid_v2 WHERE ilike(varx, '%weal%')"
wealth_vars <- as_tibble(dbGetQuery(con, wid_wealth_vars_cmd))

wealth_tbl <- table(wealth_vars$variable)
wealth_tbl[order(wealth_tbl)]

wealth_cmd <- "SELECT variable, percentile, value from wid_v2 where iso3c='DEU' and variable='thweal992j' and year=2000"



wealth_data <- as_tibble(dbGetQuery(con, wealth_cmd))

wealth_data$pct_lo <- as.numeric(unlist(lapply(strsplit(wealth_data$percentile, split='p'), function(x) x[2])))
wealth_data$pct_hi <- as.numeric(unlist(lapply(strsplit(wealth_data$percentile, split='p'), function(x) x[3])))

wealth_data$pct_len <- wealth_data$pct_hi-wealth_data$pct_lo

ggplot(filter(wealth_data, pct_lo > 80, pct_len >=0.1), aes(xmin=pct_lo, xmax=pct_hi, ymin=0, ymax=value, alpha=0.01)) +
    geom_rect()

ggplot(filter(wealth_data), aes(x=pct_lo, y=log10(value))) +
    geom_point()

approx(wealth_data$pct_lo, wealth_data$value, xout = 98.12)

RootLinearInterpolant <- function (x, y, y0 = 0) {
    #' calculate intercept of threshold function 
    #' https://stackoverflow.com/questions/52650467/how-to-estimate-x-value-from-y-value-input-after-approxfun-in-r
    
  if (is.unsorted(x)) {
      ind <- order(x)
      x <- x[ind]; y <- y[ind]
     }
  z <- y - y0
  ## which piecewise linear segment crosses zero?
  k <- which(z[-1] * z[-length(z)] < 0)
  ## analytically root finding
  xk <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
  xk
}

RootLinearInterpolant(wealth_data$pct_lo, wealth_data$value, y0=5e+6)

test_df <- rbindlist(list(list(a=1, b=2), list(a=3, b=4)))
aggregate(a ~ b, test_df, function(x) x[1])




wealth_cmd_all <- "select iso3c, variable, percentile, year, value from wid_v2 where variable='thweal992j' and year >=1985 and iso3c in ('DEU', 'USA', 'MNG', 'ITA', 'FRA')"

wealth_df <- as_tibble(dbGetQuery(con, wealth_cmd_all))

currency_cmd <- "select iso3c, year, variable, value from wid_v2 where variable='xlcusp999i' and iso3c in ('DEU', 'USA', 'MNG', 'DNK', 'ITA', 'FRA') and year>=1985"
currency_df <- as_tibble(dbGetQuery(con, currency_cmd))

ggplot(filter(currency_df, iso3c %in% c("USA", "DEU", 'FRA')) , aes(x=year, y=value, color=iso3c)) +
    geom_line()



wealth_df$pct_lo <- as.numeric(unlist(lapply(strsplit(wealth_df$percentile, split='p'), function(x) x[2])))

ggplot(filter(wealth_df, iso3c=="MNG", year==2000), aes(x=pct_lo, y=log10(value))) +
    geom_point()


x <- wealth_df %>%
    group_by(iso3c, year) %>%
    summarise(pct_cutoff = RootLinearInterpolant(pct_lo, value, y0=1e+06))
    
## do(RootLinearInterpolant(x=.$pct_lo, y=.$value, y0=1e+06)

ggplot(x, aes(x=year, y=pct_cutoff, group=iso3c, color=iso3c)) +
    geom_line()


getFX("EUR/USD", from = "2005-01-01")

library(priceR)

historical_exchange_rates("EUR", to = "USD", start_date = "2010-01-01", end_date = "2020-06-30")
historical_exchange_rates("DEM", to = "USD", start_date = "1999-01-01", end_date = "2000-06-30")


