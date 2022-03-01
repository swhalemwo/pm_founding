

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
    return(xk)
}

## x <- wealth_cur_df %>%
##     group_by(iso3c, year) %>%
##     summarise(pct_cutoff = RootLinearInterpolant(pct_lo, wealth_cur, y0=1e+06))

sanitize_number <- function(nbr) {
    #' custom formatting for number abbreviations because f_denom doesn't work??

    if (nbr >= 1000 & nbr < 1e6) {
        lbl <- paste0(nbr/1000, "K")
    } else if (nbr >= 1e6 & nbr < 1e+9) {
        lbl <- paste0(nbr/1e6, "M")
    } else {
        lbl <- paste0(nbr)
    }
    return(lbl)
}
        



wealth_cutoff <- function(x,y,cutoff_amt) {
    #' actually wrapper function for RootLinearinterpolant, also some diagnostics: 
    #' distances (percentile wise) to upper/lower value, 
    
    ## could make own funtion for this, but kinda lazy for now 
    cutoff = RootLinearInterpolant(x,y,cutoff_amt)
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
    z <- y - cutoff_amt
    ## which piecewise linear segment crosses zero?
    k <- which(z[-1] * z[-length(z)] < 0)

    
    cutoff_amt_sanitized <- sanitize_number(cutoff_amt)

    
    dist_down <- cutoff-x[k]
    dist_up <- x[k+1]-cutoff
    lenx <- len(x)
    vlus_above <- lenx-k
    maxy <- max(y)

    res_df <- tibble(pct_cutoff=cutoff,
                  dist_down=dist_down,
                  dist_up =dist_up,
                  lenx=lenx,
                  vlus_above = vlus_above,
                  maxy = maxy)
    
    names(res_df) <- paste(names(res_df), cutoff_amt_sanitized, sep = '_')

    return(res_df)
}

j <- wealth_cutoff(wealth_data$pct_lo, wealth_data$value, cutoff=5e+6)

get_wealth_df <- function() {
    #' get the basic wealth df (in USD PPP)
    #' store as global so I don't have to reload everything from CH if I want to change cutoff
    #' use threshold for now (easiest)

    wealth_cmd_all <- paste0("select iso3c, variable, percentile, year, value from wid_v2 where variable='thweal992j' and year >=", STARTING_YEAR)

    wealth_df <- as_tibble(dbGetQuery(con, wealth_cmd_all))

    currency_cmd <- paste0("select iso3c, year, value from wid_v2 where variable='xlcusp999i' and year>=", STARTING_YEAR)
    currency_df <- as_tibble(dbGetQuery(con, currency_cmd))
    names(currency_df)[3] <- "xlcusp999i"

    ## ggplot(filter(currency_df, xlcusp999i < 10), aes(x=year, y=xlcusp999i, color=iso3c)) +
    ##     geom_line()


    wealth_cur_df <- as_tibble(merge(wealth_df, currency_df, all.x = T))
    wealth_cur_df$wealth_cur <- wealth_cur_df$value/wealth_cur_df$xlcusp999i

    ## there are still quite some NAs
    ## table(filter(wealth_cur_df, is.na(wealth_cur))$iso3c)
    ## North Korea, South Sudan
    ## nobody cares bro 

    wealth_cur_df$pct_lo <- as.numeric(unlist(lapply(strsplit(wealth_cur_df$percentile, split='p'), function(x) x[2])))

    return(wealth_cur_df)
}



## ggplot(filter(wealth_cur_df, iso3c=="DEU", year==2000), aes(x=pct_lo, y=log10(value))) +
##     geom_point()


get_wealth_cutoff_pct <- function(wealth_cur_df, cutoff) {
    #' calculating share of people over cutoff

    
    df_wealth <- wealth_cur_df %>%
        group_by(iso3c, year) %>%
        do(wealth_cutoff(.$pct_lo, .$wealth_cur, cutoff_amt =cutoff))

    return(df_wealth)
}


wealth_cur_df <- get_wealth_df()
df_wealth <- get_wealth_cutoff_pct(wealth_cur_df, 1e+06)

## min max wealth is something like 10m


    
## do(RootLinearInterpolant(x=.$pct_lo, y=.$value, y0=1e+06)

ggplot(df_wealth, aes(x=year, y=pct_cutoff, group=iso3c, color=iso3c)) +
    geom_line()


## ** debug weird countries

table(filter(df_wealth, pct_cutoff < 95)$iso3c)
## AGO BLR COD UZB VEN 
##   4   3   5   1  25 
## much in values for vuvuzela -> maybe I have to lag the cur_df? 

filter(currency_df, iso3c=="VEN")$xlcusp999i

ggplot(filter(df_wealth, iso3c=="VEN"), aes(x=year, y=pct_cutoff, group=iso3c, color=iso3c)) +
    geom_line()




