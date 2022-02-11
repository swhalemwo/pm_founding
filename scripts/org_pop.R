## ** startup
## *** libraries
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)
library(lme4)
library(texreg)
library(ggplot2)
library(countrycode)
library(stargazer)
library(gridExtra)
library(parallel)
library(RClickhouse)
library(docstring)
library(DBI)
library(TTR)
library(ggpubr)
library(xtable)
library(OECD)
library(rsdmx)
library(data.table)
library(wbstats)
ds <- docstring


options(show.error.messages = TRUE)
options(show.error.locations = TRUE)



## *** set static vars

PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"

PMDB_DIR <- paste0(PROJECT_DIR, "data/pmdb/") # DIR for private museum database (currently excel import)
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")
FIG_DIR <- paste0(PROJECT_DIR, "figures/")
TABLE_DIR <- paste0(PROJECT_DIR, "tables/")
WID_DIR_v1 = paste0(PROJECT_DIR, "data/wid/wid_world_db/version1_oct21/")
WID_DIR_v2 = paste0(PROJECT_DIR, "data/wid/wid_world_db/version2_feb22/")


PROC_DATA_DIR <- paste0(PROJECT_DIR, "data/processed/")

STARTING_YEAR <- 1985
ENDING_YEAR <- 2020



con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

## *** source other files 
source(paste0(SCRIPT_DIR, "custom_funcs.R")) # random utils
source(paste0(SCRIPT_DIR, "base_df_creation.R")) # function to read in excel data
source(paste0(SCRIPT_DIR, "wb_api.R"))



## *** WID

## **** setting up CH

df_gdp <- get_WB_data("NY.GDP.PCAP.CD")

countrycodes2c <- na.omit(countrycode(unique(df_gdp$iso3c), "iso3c", "iso2c"))
countrycodes3c <- na.omit(unique(df_gdp$iso3c))

## issues with CHI and XKX
## need to check what they actually refer in WB data
## then probably write some manual exceptions so that they get correctly translated from WB to WDI
## also need to check whether there are other mismatches between iso2c(wb) and WDI -> paste WDI (https://wid.world/codes-dictionary/#country-code)

## **** check country codes compatibility between WID and WB

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

## **** read into CH

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

            ## only write schema with first table, otherwise append
            ## if (code == "ABW"){
            if (code == countrycodes3c[1]) {
                dbWriteTable(con, db_name, cry_data)
            }
            else {
                dbWriteTable(con, db_name, cry_data, append=TRUE)
                
            }
        }
    }
    print("done")   
}

read_WID_into_CH(countrycodes3c, "wid_v1", WID_DIR_v1)
read_WID_into_CH(countrycodes3c, "wid_v2", WID_DIR_v2)

## **** completeness tests



## *** join predictor data together
## For Inner Join
multi_inner <- as_tibble(Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list(df_wb_gini_molt, df_wb_population_molt, df_gdp_pcap_molt)
))
## how the fuck does that work? how does it know what variables to use to join?
## need to check reduce


multi_inner$year <- as.numeric(as.character(multi_inner$year))
multi_inner <- multi_inner[which(multi_inner$year >= STARTING_YEAR),]

filter(multi_inner, is.na(population))
## population only missing for stuff where I don't have gdp/gini data anyways


## ** basic DV table preparation
## add year: 
## just clean everything by taking first 4, drop everything that doesn't work lmao

## have 59 NAs, oh well
## let's say for now 1985-2021
# plot(table(df$year_opened_int), type='h')


df$year_opened_int2 <- df$year_opened_int
df$year_opened_int2[which(df$year_opened_int2 < STARTING_YEAR | df$year_opened_int2 > 2021)] <- NA
## up to 100 dropped atm 


df_open <- na.omit(df[,c('name', 'country', 'countrycode', 'year_opened_int2')])
# plot(table(df_open$year_opened_int2), type='l')
df_open$ctr <- 1


df_country_year_agg_cnt <- as_tibble(aggregate(ctr ~ countrycode + year_opened_int2, df_open, FUN = sum))
names(df_country_year_agg_cnt) <- c('countrycode', 'year', 'nbr_opened')

## df_country_year_agg_names <- as_tibble(aggregate(name ~ countrycode + year_opened_int2, df_open, c))

## df_country_year_agg_names <- as_tibble(aggregate(name ~ countrycode + year_opened_int2, df_open, function(x){paste(x, collapse = "----")}))

df_country_year_agg_names <- df_open %>% group_by(countrycode, year_opened_int2) %>% summarise(name = list(name))


names(df_country_year_agg_names) <- c("countrycode", "year", "name")

df_country_year_agg <- as_tibble(merge(df_country_year_agg_cnt, df_country_year_agg_names))



## ** merge basic opening data with gdp data

df_anls <- as_tibble(merge(multi_inner, df_country_year_agg,
                           by=c('countrycode', 'year'),
                           all.x= TRUE))

## fill up NAs up with 0s
df_anls$nbr_opened[which(is.na(df_anls$nbr_opened))] <- 0

df_anls$wv <- 0

df_anls$gdp_pcapk <- df_anls$gdp_pcap/1000

## check if WB and my country codes are the same, they are 
## x <- merge(df_gdp_pcap2[,c("V1", "V2")],
##       unique(df_country_year_agg[,c("country", "countrycode")]),
##       by.x = c("V2"),
##       by.y = c("countrycode"))
## x[which(x$V1 != x$country),]

## cumulative number of opened
df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$countrycode, FUN = cumsum)
df_anls$nbr_opened_cum_sqrd <- (df_anls$nbr_opened_cum^2)/100
## have to divide by 100 otherwise R glmer.nb complains


## PMs opened per 1m people -> rate
df_anls$nbr_opened_prop <- df_anls$nbr_opened/(df_anls$population/1000000)

## iceland, monaco, cyprus LUL
filter(df_anls, nbr_opened_prop > 1)




## ** directions of trade
## *** exploring completeness
## **** across years
## compare degree of coverage across years

con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

pairs_1985 <- as_tibble(dbGetQuery(con, "SELECT DISTINCT(CONCAT(toString(country_code), '-', toString(counterpart_country_code))) AS pair from dots_prep WHERE time_period='1985'"))

pairs_2020 <- as_tibble(dbGetQuery(con, "SELECT DISTINCT(CONCAT(toString(country_code), '-', toString(counterpart_country_code))) AS pair from dots_prep WHERE time_period='2020'"))

unique_pairs_1985 <- pairs_1985[which(pairs_1985$pair %!in% pairs_2020$pair),]
sample(unique_pairs_1985$pair, 200)
## seems mostly countries that don't exist anymore in 2020: USSR, East Germany, Czechoslovakia

tbl_imf <- table(countrycode(unlist(strsplit(unique_pairs_1985$pair, '-')), 'imf', 'country.name'))
tbl_imf[rev(order(tbl_imf))][c(0:30)]

## **** country coded completeness check: done 

## hmm a bunch of country codes don't get clearly matched
## maybe i can group by country_code and counterpart_country_code rather than by my string stuff
## also easier to which countrycodes produce mistakes in conversion

unq_crys1 <- as_tibble(dbGetQuery(con, "SELECT tpl.1 AS country_name, tpl.2 AS country_code FROM (SELECT DISTINCT(country_name, country_code) AS tpl FROM dots_prep GROUP BY country_name, country_code)"))

unq_crys2 <- as_tibble(dbGetQuery(con, "SELECT tpl.1 AS country_name, tpl.2 AS country_code FROM (SELECT DISTINCT(counterpart_country_name, counterpart_country_code) AS tpl FROM dots_prep GROUP BY counterpart_country_name, counterpart_country_code)"))

unq_crys <- unique(rbind(unq_crys1, unq_crys2))
unq_crys$conversion <- countrycode(unq_crys$country_code, "imf", "country.name")

imf_list <- apply(unq_crys, 1, function(x) c(x))
imf_list <- unq_crys$country_name
names(imf_list) <- unq_crys$country_code

as.data.frame(unq_crys[which(is.na(unq_crys$conversion)),])
## conversion problems mostly about regional groupings/associations (USSR, world, Emerging countries, Asia not specified) etc

as.data.frame(unq_crys[which(!is.na(unq_crys$conversion)),c("country_name", "conversion")])
## conversion between imf countrycode and name seems to work well 

## **** type of time period: done

## also need to compare coverage across time intervals


dots_time_cprn <- function(year){
    pairs_yx_qs <- as.data.frame(matrix(ncol = 2, nrow = 0))
    names(pairs_yx_qs) <- c("ccd", "ctrccd")

    print(year)

    for (i in seq(4)){
        cmd <- "SELECT tpl.1 AS ccd, tpl.2 AS ctrccd FROM (SELECT  Distinct(country_code, counterpart_country_code) AS tpl from dots_prep where time_period = '{year}Q{i}' group by country_code, counterpart_country_code)"

        pairs_yx_qx <- as_tibble(dbGetQuery(con, glue(cmd)))
        pairs_yx_qs <- rbind(pairs_yx_qs, pairs_yx_qx)
    }
    
    
    pairs_yx_qs <- unique(pairs_yx_qs)

    year_cmd <- "SELECT tpl.1 AS ccd, tpl.2 AS ctrccd FROM (SELECT  Distinct(country_code, counterpart_country_code) AS tpl from dots_prep where time_period = '{year}' group by country_code, counterpart_country_code)"
    pairs_yx <- as_tibble(dbGetQuery(con, glue(year_cmd)))

    names(pairs_yx) <- names(pairs_yx_qs)
    
    return(list(
        year=year,
        yearly_pairs= nrow(pairs_yx),
        quarter_pairs= nrow(pairs_yx_qs),
        yearly_unique= nrow(setdiff(pairs_yx, pairs_yx_qs)),
        quarterly_unique= nrow(setdiff(pairs_yx_qs, pairs_yx)),
        common_pairs= nrow(union(pairs_yx_qs, pairs_yx))))
}

dots_cpltns_res <- lapply(seq(1985,2020), dots_time_cprn)

dots_cpltns_df <- do.call(rbind.data.frame, dots_cpltns_res)
sum(dots_cpltns_df$yearly_unique)
sum(dots_cpltns_df$quarterly_unique)
## using yearly data seems ok, quarterly has slightly more unique, but could easily be that coverage is not complete for all quartiles
## could check but don't think there's much need at this stage
## could also rewrite function to save df R objects, but really no need atm





## *** computation

## figuring out meaning

vlu_cmd <- "SELECT * from dots 
where ((country_code='134' and counterpart_country_code='138') 
or (country_code='138' and counterpart_country_code='134')) and year=2020"

dots_test <- as_tibble(dbGetQuery(con, vlu_cmd))

as.data.frame(dots_test)

dots_test[,c("country_name", "counterpart_country_name", "indicator_code", "value")]

(filter(dots_test, country_name == "Germany" & indicator_code == "TXG_FOB_USD")$value - filter(dots_test, country_name == "Germany" & indicator_code == "TMG_CIF_USD")$value)/filter(dots_test, country_name == "Germany" & indicator_code == "TBG_USD")$value


vlu_cmd2 <- "SELECT country_code, counterpart_country_code, concat(toString(country_code), '-', toString(counterpart_country_code)) AS link1, concat(toString(counterpart_country_code), '-', toString(country_code)) AS link2, year, indicator_code, value FROM dots WHERE year='2020'"

fob_cif_cprn <- as_tibble(dbGetQuery(con, vlu_cmd2))

fob_df <- filter(fob_cif_cprn, indicator_code=="TXG_FOB_USD")[,c("country_code", "counterpart_country_code", "year", "value")]
names(fob_df) <- c("country_code", "counterpart_country_code", "year", "fob_value")
cif_df <- filter(fob_cif_cprn, indicator_code=="TMG_CIF_USD")[,c("country_code", "counterpart_country_code", "year", "value")]
## somehow need to change direction,
## try first with renaming
## then with link columns
names(cif_df) <- c("counterpart_country_code", "country_code", "year", "cif_value")

fob_cif_cprn2 <- as_tibble(merge(fob_df, cif_df))

dots_test[,c("country_name", "counterpart_country_name", "indicator_code", "value")]

filter(fob_cif_cprn2, (country_code==134 & counterpart_country_code==138) |  (country_code==138 & counterpart_country_code==134))
## huh NL imports more to DE than DE to NL, pattern also elsewhere: 
## https://tradingeconomics.com/netherlands/exports/germany
## https://tradingeconomics.com/germany/exports/netherlands
## numbers are off tho




fob_cif_cprn2$diff <- fob_cif_cprn2$fob_value - fob_cif_cprn2$cif_value
fob_cif_cprn2$diff_log <- log(fob_cif_cprn2$fob_value) - log(fob_cif_cprn2$cif_value)
hist(fob_cif_cprn2$diff_log, breaks = 1000)
summary(fob_cif_cprn2$diff)
hist(fob_cif_cprn2$diff, breaks = 1000)
boxplot(fob_cif_cprn2$diff, breaks = 500)

iqr <- IQR(fob_cif_cprn2$diff)
Q <- quantile(fob_cif_cprn2$diff, probs = c(0.25, 0.75))
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated <- subset(fob_cif_cprn2, fob_cif_cprn2$diff > (Q[1] - 1.5*iqr) & fob_cif_cprn2$diff < (Q[2]+1.5*iqr))
hist(eliminated$diff, breaks = 50)

## wide range of FOB and CIF diffs, most in ones/tens of millions, but some also hundreds of billions



## largest ones:



## first exclude regions/entities that don't translate properly, it's mostly regional groupings that don't make much sense anyways
fob_cif_cprn2$country_name <- countrycode(fob_cif_cprn2$country_code, 'imf', 'country.name')
fob_cif_cprn2$counterpart_country_name <- countrycode(fob_cif_cprn2$counterpart_country_code, 'imf', 'country.name')

fob_cif_cprn2_naomit <- na.omit(fob_cif_cprn2)
hist(fob_cif_cprn2_naomit$diff, breaks = 500)

top_diffs <- c(order(fob_cif_cprn2_naomit$diff)[c(1:15)], rev(order(fob_cif_cprn2_naomit$diff))[c(1:15)])
as.data.frame(fob_cif_cprn2_naomit[top_diffs,c("country_name", "counterpart_country_name", "diff")])

## whole bunch of China in both plus and minus on, also on both sides
## but also large differences between Canada/US, US/Mexico, Malaysia/US, Russia/NL
## but really seems like ~25/30 are about China


countrycode(c("DEU", "NLD"), "iso3c", "imf")


fob_cif_cprn2_naomit$ratio <- fob_cif_cprn2_naomit$fob_value/fob_cif_cprn2_naomit$cif_value
fob_cif_cprn2_naomit$ratio2 <- fob_cif_cprn2_naomit$cif_value/fob_cif_cprn2_naomit$fob_value
hist(fob_cif_cprn2_naomit$ratio, breaks = 1000)
hist(fob_cif_cprn2_naomit$ratio2, breaks = 1000)

summary(fob_cif_cprn2_naomit$ratio)

hist(fob_cif_cprn2_naomit$ratio[which(fob_cif_cprn2_naomit$ratio < 4)], breaks = 400)
## largest peak actually not at 1, but slightly below, maybe ~0.95

nrow(filter(fob_cif_cprn2_naomit, ratio < 0.5 | ratio > 2))/nrow(fob_cif_cprn2_naomit)
## 33% of observations have ratio <0.5 or >2


ifs <- read.csv("/home/johannes/Downloads/ifs/IFS_11-08-2021 14-30-52-56.csv")
## oof 1.5k indicators -> won't have bilateral values



## ** checking NAs

aggregate(gdp_pcap ~ countrycode, data = df_anls, function(x){sum(is.na(x))}, na.action = NULL)
## around 10% missing :(
## might have to kick out some countries/years


unique(df_open$countrycode)[which(unique(df_open$countrycode) %!in% (unique(df_gdp_pcap_molt$countrycode)))]
## seems ok,
## taiwan not separate country in WB.. just 1 PM tho, so shouldn't be big impact




## *** WDI completeness checks
## **** income

x <- tbl(con, "wdi") %>%
    filter(varx == "hweal") %>%
    group_by(percentile) %>%
    summarise(length(percentile))



base_cmd <- "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where year >= 1985"

base_df <- as_tibble(dbGetQuery(con, base_cmd))


check_wid_cpltns <- function(varx, percentile){
    #' check how well WID variables cover PM foundings

    ## print(varx)
    varz <- varx ## need to assign to own objects to be able to filter on them 

    if(missing(percentile)){

        res <- filter(base_df, varx==varz)
    
    } else {
        
        ## print(percentile)
        
        pctz <- percentile
        res <- filter(base_df, variable == varz & percentile == pctz)
       
    }

    ## some exception to throw when too many variables
    if (length(table(res$variable)) > 1){
        ## print(varx)
        stop("too many variables")
    }

    ## print(nrow(res))
    if (nrow(res)!=0){
        
    ## make df base to merge WDI data to 
    dfb <- df_anls[,c("countrycode", "year", "nbr_opened")]

    dfc <- as_tibble(merge(dfb, res, by = c("year", "countrycode"), all.x = TRUE))
    cry_cvrg <- aggregate(year ~ countrycode, na.omit(dfc), length)
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$countrycode

    cry_pm_crvg_actual <- aggregate(nbr_opened ~ countrycode, na.omit(dfc), sum)
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ countrycode, dfc, sum)
    names(cry_pm_crvg_ideal) <- c("countrycode", "nbr_opened_ideal")
    
    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ## maybe need to collapse them instead of having them as vector 
    most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
                                        function(x) (filter(cry_pm_cvrg_cprn, diff == x)$countrycode)))

    ## country-year coverage of countries which have at least 3 WDI observations AND which have WDI data for that year
    PMs_covered_raw <- sum(na.omit(dfc[which(dfc$countrycode %in% crys_geq3),])$nbr_opened)
    ## huh 280 for labsh, not too bad


    ## coverage of countries which have at least three values, even if they don't have WDI data for years of museum founding
    cry_cvrg_geq3 <- sum(filter(dfc, countrycode %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- filter(aggregate(nbr_opened ~ countrycode, dfc, sum), countrycode %in% crys_geq3) %>%
        filter(nbr_opened >= 1) %>%
        nrow()

    return(list(
        variable = varx,
        percentile = percentile,
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
    }
}

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

    shr_variables_p90 <- dbGetQuery(con, "select distinct(variable) from wdi where percentile='p90p100' and first_letter='s'")[[1]]
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

    wealth_res_df2 <- wealth_res_df2[,c('variable', 'variable_label', 'PMs_covered_raw', 'cry_cvrg_geq3', 'nbr_of_crys_geq3', 'nbr_of_crys_geq1pm')]

    names(wealth_res_df2) <- c("variable", "meaning", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")


    xtbl <- xtable(
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
}

## **** middle classes
## ***** pre-checking the variables to do, not much decrease
wid_inc_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wdi WHERE ilike(varx, '%inc%' )"
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

list_of_ranges <- split_percs(wid_inc_vars$percentile[c(0:5)])
check_ranges(list_of_ranges)


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

## same with share data: only sufficient global coverage for ptinc992j, diinc992j second 


## dbDisconnect(con)


## dbListTables(con)


## checking whether i don't accidentally skip some country-years, apparently not
## aggregate(year ~ country, multi_inner, max)
## aggregate(year ~ country, multi_inner, min)
## aggregate(year ~ country, multi_inner, len)



## **** done: check how DEU has shweal992j, but only p0p100 percentiles: is because metadata is for all countries even if they don't have that data, actual data only there for handful of countries
## wealth_check <- as_tibble(DBI::dbGetQuery(con, "select distinct(varx) as varx from wdi where country='DEU' and varx like '%weal%'"))


## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where varx like '%weal%' and country = 'DEU' and year >= 1985"))

## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where first_letter='s' and country = 'DEU' and year >= 1985"))


## table(res$variable, res$percentile)



## **** average vs share check

## see if shares are always there when there are averages, turns out it's the other way around: there are shares, but not always the averages to compute them
## -> sticking to shares is fine: more complete and easier to handle

## con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

## cmd_cplt_check <- "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where (percentile='p90p100' or percentile='p0p90') and year>=1985"

## res_cplt_check <- as_tibble(dbGetQuery(con, cmd_cplt_check))

## res_s_p90p100 <- filter(res_cplt_check, first_letter=="s" & percentile=="p90p100")
## res_a_p0p90 <- filter(res_cplt_check, first_letter=="a" & percentile=="p0p90")
## res_a_p90p100 <- filter(res_cplt_check, first_letter=="a" & percentile=="p90p100")
## res_a_mrg <- as_tibble(merge(res_a_p0p90, res_a_p90p100, by=c("countrycode", "variable", "year")))
## res_a_mrg$ratio <- res_a_mrg$value.y/(9*res_a_mrg$value.x + res_a_mrg$value.y)

## res_a_mrg_cut <- res_a_mrg[,c("countrycode", "variable", "year", "ratio")]
## res_a_mrg_cut$varx <- res_a_mrg$varx.x


## ## there are only 16k with top 10, but 1.2m with entire population

## ## make ratio
## ## actually average is not good for making top 10% concentration ratios: average of p0p100 is lower than p90p100 -> that's why ratio was negative
## ## would either need total of p90p100 and p0p100
## ## or average of p0p90 -> can calculate top10 ratio as p90p100/(9*p0p90 + p90p100)
## ## maybe first casting is better? then column filtering is easier 

## table(res_s_p90p100$variable, res_s_p90p100$varx)
## table(res_a_mrg_cut$variable, res_a_mrg_cut$varx)
## ## varx only differ from variables in that variables sometimes are measured on different levels of analysis
## ## -> can do varx2: only remove the first letter
## res_s_p90p100$varx2 <- substring(res_s_p90p100$variable, 2, 100)
## res_a_mrg_cut$varx2 <- substring(res_a_mrg_cut$variable, 2, 100)


## cpr_shrs <- as_tibble(merge(res_s_p90p100, res_a_mrg_cut, by=c("countrycode", "varx2", "year")))
## cor(cpr_shrs$ratio, cpr_shrs$value, use = "complete.obs")
## ## nice now correlation is pretty much 1
## ## res_s_p90p100 has more observations (14.7k) than res_a_mrg_cut (11.9k), res_a_mrg_cut also has 90 NAs
## ## which observations (countries, variables, years) have shares but not complete averages?
## ## multiple aggregations

## agg_s_year <- as.data.frame(table(res_s_p90p100$year))
## names(agg_s_year) <- c("year", "share_cnt")
## agg_r_year <- as.data.frame(table(res_a_mrg_cut$year))
## names(agg_r_year) <- c("year", "ratio_cnt")

## year_comparison <- merge(agg_s_year, agg_r_year, using = c("year"))
## year_cprn_melt <- melt(year_comparison, id="year")

## ggplot(year_cprn_melt, aes(x=year, y=value, group=variable, color=variable)) +
##     geom_line()
## ## tbh differences are not that big, maybe 20-30 per year  -> 10% difference

## res_s_p90p100[,c("countrycode", "variable", "year")]

## ## could combine, remove all that occur twice

## s_r_cbin <- rbind(res_s_p90p100[,c("countrycode", "varx2", "year")], res_a_mrg_cut[,c("countrycode", "varx2", "year")])
## s_r_cbin$ctr <- 1

## sr_cbin_agg <- as_tibble(aggregate(ctr ~ countrycode + varx2 + year, s_r_cbin, sum))
## plot(table(filter(sr_cbin_agg, ctr==1)$year), type='l')
## ## year: seems kinda stable, but decreaes in recent years
## table(filter(sr_cbin_agg, ctr==1)$varx2)

## ## most differences in variables about "fiinc": fiscal income: there in share, but not in average
## ## fiinc992i
## ## fiinc992j
## ## fiinc992t

## ## also some in others
## ## diinc992i
## ## diinc992t
## ## fiinc999t
## ## hweal992i
## ## ptinc992j 
## table(filter(sr_cbin_agg, ctr==1)$countrycode)

## cry_cnt_a <- as.data.frame(table(res_a_mrg_cut$countrycode))
## names(cry_cnt_a) <- c("countrycode", "count")
## cry_cnt_a$countrycode <- as.character(cry_cnt_a$countrycode)
## cry_cnt_a$condition <- "ratio"

## cry_cnt_s <- as.data.frame(table(res_s_p90p100$countrycode))
## names(cry_cnt_s) <- c("countrycode", "count")
## cry_cnt_s$countrycode <- as.character(cry_cnt_s$countrycode)
## cry_cnt_s$condition <- "share"

## cry_cnt_cbn <- rbind(cry_cnt_a, cry_cnt_s)

## ggplot(cry_cnt_cbn, aes(x=countrycode, fill=condition, y=count)) +
##     geom_bar(position = "dodge", stat="identity")

## ## most countries have just 35 observations, differences is only for the countries that have more: more shares than ratios


## *** EGMUS data

EGMUS_DIR <- "/home/johannes/Dropbox/phd/data/EGMUS/"

df_1998 <- read.csv(paste0(EGMUS_DIR, "egmus_export_1998_seded.csv"), sep = ";", header = T, dec = ",") 
df_1999 <- read.csv(paste0(EGMUS_DIR, "egmus_export_1999_seded.csv"), sep = ";", header = T, dec = ",") 
df_2000 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2000_seded.csv"), sep = ";", header = T, dec = ",")
df_2001 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2001_seded.csv"), sep = ";", header = T, dec = ",")
df_2002 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2002_seded.csv"), sep = ";", header = T, dec = ",")
df_2003 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2003_seded.csv"), sep = ";", header = T, dec = ",")
df_2004 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2004_seded.csv"), sep = ";", header = T, dec = ",")
df_2005 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2005_seded.csv"), sep = ";", header = T, dec = ",")
df_2006 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2006_seded.csv"), sep = ";", header = T, dec = ",")
df_2007 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2007_seded.csv"), sep = ";", header = T, dec = ",")
df_2008 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2008_seded.csv"), sep = ";", header = T, dec = ",")
df_2009 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2009_seded.csv"), sep = ";", header = T, dec = ",")
df_2010 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2010_seded.csv"), sep = ";", header = T, dec = ",")
df_2011 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2011_seded.csv"), sep = ";", header = T, dec = ",")
df_2012 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2012_seded.csv"), sep = ";", header = T, dec = ",")
df_2013 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2013_seded.csv"), sep = ";", header = T, dec = ",")
df_2014 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2014_seded.csv"), sep = ";", header = T, dec = ",")
df_2015 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2015_seded.csv"), sep = ";", header = T, dec = ",")
df_2016 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2016_seded.csv"), sep = ";", header = T, dec = ",")
df_2017 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2017_seded.csv"), sep = ";", header = T, dec = ",")
df_2018 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2018_seded.csv"), sep = ";", header = T, dec = ",")
df_2019 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2019_seded.csv"), sep = ";", header = T, dec = ",")
df_2020 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2020_seded.csv"), sep = ";", header = T, dec = ",")

df_egmus <- as_tibble(rbind(df_1998,df_1999,df_2000,df_2001,df_2002,df_2003,df_2004,df_2005,df_2006,
                  df_2007,df_2008,df_2009,df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016,
                  df_2017,df_2018,df_2019, df_2020))

df_egmus$private <- as.character(df_egmus$X3d..Ownership...private.owned.museums...Total)
df_egmus$private_clean <- gsub("\\.", "", df_egmus$private)
df_egmus$private_num <- as.numeric(df_egmus$private_clean)


## na.omit needed to remove all the points to still have line
ggplot(data = na.omit(df_egmus[,c("Year", "private_num", "Country")]), aes(x=Year, y=private_num, group=Country, color=Country)) +
    geom_line() 


aggregate(private_num ~ Country, df_egmus, mean, use="complete.obs")
filter(df_egmus, Country == "Italy")$private_num






## *** diffusion
## **** geographical proximity: use country boundaries
library(rgdal)
spdf <- readOGR(dsn = "/home/johannes/Dropbox/phd/papers/org_pop/data/boundaries/World_EEZ_v11_20191118/")


spdf_fltrd <- as_tibble(unique(spdf@data[,c("SOVEREIGN1", "SOVEREIGN2")]))
names(spdf_fltrd) <- c("cry1", "cry2")
## duplicate boundaries, turn to have alphabetically higher country first 
spdf_fltrd$cry1_cd <- countrycode(spdf_fltrd$cry1, "country.name", "iso3c", custom_match = c("Comores" = "COM", "Micronesia" = "FSM"))
spdf_fltrd$cry2_cd <- countrycode(spdf_fltrd$cry2, "country.name", "iso3c", custom_match = c("Comores" = "COM", "Micronesia" = "FSM"))

sea_boundaries <- rbind(spdf_fltrd[,c("cry1_cd", "cry2_cd")],
                        setNames(rev(spdf_fltrd[,c("cry1_cd", "cry2_cd")]),
                                 names(spdf_fltrd[,c("cry1_cd", "cry2_cd")])))


sea_boundaries <- na.omit(sea_boundaries)
sea_boundaries <- filter(sea_boundaries, cry1_cd < cry2_cd)


## https://stackoverflow.com/questions/19297475/simplest-way-to-get-rbind-to-ignore-column-names

land_boundaries <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/boundaries/geodatasource_land_boundaries.csv"))

land_boundaries$cry1_cd <- countrycode(land_boundaries$country_name, "country.name", "iso3c")
land_boundaries$cry2_cd <- countrycode(land_boundaries$country_border_name, "country.name", "iso3c")
land_boundaries <- na.omit(land_boundaries)

land_boundaries <- rbind(land_boundaries[,c("cry1_cd", "cry2_cd")],
                         setNames(rev(land_boundaries[,c("cry1_cd", "cry2_cd")]),
                                  names(land_boundaries[,c("cry1_cd", "cry2_cd")])))

land_boundaries <- unique(filter(land_boundaries, cry1_cd < cry2_cd))

all_boundaries <- unique(rbind(sea_boundaries, land_boundaries))

all_boundaries <- rbind(all_boundaries[,c("cry1_cd", "cry2_cd")],
                        setNames(rev(all_boundaries[,c("cry1_cd", "cry2_cd")]),
                                 names(all_boundaries)))

## *** create variable of number in neighbors

## need efficient way: for loops are slow
## expand, then aggregate: nbr_opened ~ country + year, mean

## also need year somehow
## first i need much more entries: merge all neighbor countries for each year 
geo_mrg1 <- as_tibble(merge(df_anls[c("countrycode", "year")], all_boundaries, by.x = "countrycode", by.y = "cry1_cd", all.x = TRUE))

filter(geo_mrg1, countrycode == "POL")
filter(geo_mrg1, cry2_cd == "POL")

table(as.data.frame(filter(geo_mrg1, countrycode == "POL"))$year)


## merge actual count to geo_mrg1
df_merge_prep <- df_anls[c("countrycode", "year", "nbr_opened")]
names(df_merge_prep) <- c("cry2_cd", "year", "geo_neib_opened")

geo_mrg2 <- as_tibble(merge(geo_mrg1, df_merge_prep, by = c("year", "cry2_cd"), all.x = TRUE))

as.data.frame(filter(geo_mrg2, countrycode == "POL" cry2_cd == "DEU"))
filter(geo_mrg2, cry2_cd == "DEU" & year == 2000)

## seems to have worked: yup: now proper duplication
## maybe previously just wrong query to check 


## aggregate by country-year
geo_agg1 <- as_tibble(aggregate(geo_neib_opened ~ countrycode + year, geo_mrg2, mean))
as.data.frame(filter(geo_agg1, countrycode == "POL"))

df_anls2 <- as_tibble(merge(df_anls, geo_agg1, by=c("countrycode", "year"), all.x = TRUE))
## pretty sure it's fine to set NA's to 0: means there are no neighbors
df_anls2$geo_neib_opened[which(is.na(df_anls2$geo_neib_opened))] <- 0

summary(df_anls2$geo_neib_opened)

df_anls <- df_anls2

## *** world values survey
df_wvs <- as_tibble(readRDS("/home/johannes/ownCloud/WVS/WVS_TimeSeries_1981_2020_R_v2_0.rds"))

    
## ** aggregating systematically

## specify variable-specific aggregation function 
vrbl_agg_func_dict <- list(gini=mean,
                           gdp_pcap=mean,
                           gdp_pcapk = mean, 
                           nbr_opened = sum,
                           nbr_opened_cum = max,
                           nbr_opened_cum_sqrd = max
                           )


agger <- function(varx, dfx){

    ## aggregate varx in df_anls by wv + countrycode, use aggregation function specified in vrbl_agg_func_dict
    ## dfx$agg_var <- as.numeric(unlist(dfx[,c(varx)]))
    funx <- vrbl_agg_func_dict[varx][[1]]
    dfx$agg_var <- as.numeric(unlist(dfx[,c(varx)]))
    ## df_aggx <- aggregate(agg_var ~ wv + countrycode, dfx, mean) ## old version with only mean 
    df_aggx <- aggregate(agg_var ~ wv + countrycode, dfx, FUN = funx)
    df_aggx[,varx] <- df_aggx$agg_var
    return(as_tibble(df_aggx[,c("countrycode", "wv", varx)]))}


## agger("nbr_opened_cum", filter(dfx, countrycode == "USA"))


agg_sys <- function(wave_lengthx, vrbls){
    ## wave_lengthx: spell length
    ## vrbls: vrbls to consider

    print(wave_lengthx)
    print(vrbls)

    dfx <- df_anls

    nbr_pm_ttl <- sum(dfx$nbr_opened)

    wv_ctr <- 0
    wv_nbr <- 1

    dfx$wv <- 0

    ## label the waves based on wavelength, not super elegant but works

    for (yearx in unique(dfx$year)){
        wv_ctr <- wv_ctr + 1
        ## print(c(wv_ctr, wv_nbr))
        dfx[which(dfx$year == yearx),"wv"] <- wv_nbr
        if (wv_ctr == wave_lengthx){
            wv_ctr <- 0
            wv_nbr <- wv_nbr + 1}
    }

    ## add nbr_opened by default 
    vrbls_all <- c("nbr_opened", vrbls)

    ## aggregate the variables duh 
    dfx_agg_vrbls <- as_tibble(Reduce(
        function(x,y, ...) merge(x,y, all = TRUE),
        ## lapply(c("gini", "gdp_pcap", "gdp_pcapk"), agger, dfx=dfx)
        lapply(vrbls_all, agger, dfx=dfx)
    ))

    ## dfx_agg_cnts <- as_tibble(aggregate(nbr_opened ~ countrycode + wv, dfx, sum))

    dfx_agg_names <- dfx %>% group_by(countrycode, wv) %>% summarise(name = list(unlist(name)))

    dfx_agg <- as_tibble(Reduce(
        function(x,y, ...) merge(x,y,all.x=TRUE),
        ## list(dfx_agg_means, dfx_agg_cnts, dfx_agg_names)))
        list(dfx_agg_vrbls, dfx_agg_names)))
        
    ## dfx_agg <- as_tibble(merge(dfx_agg_cnts, dfx_agg_means, by=c("countrycode", "wv"), all.x = T))

    
    return(list(df=dfx_agg,
                vrbls=vrbls,
                nbr_pm_ttl = nbr_pm_ttl,
                wave_lengthx = wave_lengthx,
                dfx_agg_names = dfx_agg_names,
                dfx_agg_vrbls = dfx_agg_vrbls
                ))
    }




df_agg2 <- agg_sys(2, c("gdp_pcapk", "gini"))
filter(df_agg2$dfx_agg_vrbls, countrycode == "USA")
df_agg4 <- agg_sys(4, c("gini", "gdp_pcap", "gdp_pcapk"))
df_agg8 <- agg_sys(8, c("gini", "gdp_pcapk"))





## could check the museum names in detail, but atm no unexpected behavior, small dips along the may could be longitudinal gerrymandering 
## unlist(na.omit(df_agg2$df)$name)[!is.na(unlist(na.omit(df_agg2$df)$name))]
## unlist(na.omit(df_agg4$df)$name)[!is.na(unlist(na.omit(df_agg4$df)$name))]
## unlist(na.omit(df_agg8$df)$name)[!is.na(unlist(na.omit(df_agg8$df)$name))]


score_agg <- function(agg_obj){
    
    dfx_agg <- na.omit(agg_obj$df)
    ## percent of entities covered
    pct_ent_cvrd <- nrow(dfx_agg)/nrow(agg_obj$dfx_agg_vrbls)
    ## pct_ent_cvrd <- nrow(dfx_agg)/nrow(agg_obj$dfx_agg_cnts)
    pct_pms_cvrd <- sum(dfx_agg$nbr_opened)/agg_obj$nbr_pm_ttl

    return(list(wave_lengthx = agg_obj$wave_lengthx,
                pct_ent_cvrd = pct_ent_cvrd,
                pct_pms_cvrd = pct_pms_cvrd,
                vrbls = paste(agg_obj$vrbls, collapse = "-")
                ))
}

score_agg(df_agg2)
score_agg(df_agg4)
score_agg(df_agg8)


## *** evaluating coverage

cvrg_evaluation <- function(vrbls, wvlen_start, wvlen_end){
    ## vrbls <- c("gini", "gdp_pcapk", "nbr_opened_cum")
    cbns <- do.call("c", lapply(seq_along(vrbls), function(i) combn(vrbls, i, FUN = list)))

    ## cfgs <- expand.grid(cbns, seq(1,10))
    cfgs <- expand.grid(cbns, seq(wvlen_start,wvlen_end))
    names(cfgs) <- c("vrbls", "wavelength")
    cfg_list <- apply(cfgs, 1, c)
    cover_res <- mclapply(cfg_list, function(x) score_agg(agg_sys(x$wavelength, x$vrbls)), mc.cores = 4)

    ## cover_res <- apply(cfgs, 1, function(x) score_agg(agg_sys(x$wavelength, x$vrbls)))
    res_df <- do.call(rbind, cover_res)

    ## for some reason necessary to unlist the columns 
    res_df2 <- as.data.frame(apply(res_df, 2, unlist))

    names(res_df2) <- c("wave_length", "prop. spells covered", "prop. PM founding covered", "vrbls")


    res_melt <- melt(res_df2, id=c("wave_length", "vrbls"))
    res_melt$value <- as.numeric(res_melt$value)
    res_melt$wave_length <- as.numeric(res_melt$wave_length)

    return(res_melt)
}

vrbls <- c("gini", "gdp_pcapk", "nbr_opened_cum")
wvlen_start <- 1
wvlen_end <- 10
res_melt <- cvrg_evaluation(vrbls, wvlen_start, wvlen_end)

pdf(paste(FIG_DIR,"completeness.pdf", sep = ""), height = 2.5, width = 5)

ggplot(res_melt, aes(x=factor(wave_length), y=value, group=interaction(variable, vrbls))) +
    geom_line(size=2, alpha = 0.6, position=position_jitter(w=0.15, h=0.015),
              mapping = aes(linetype = variable, color = vrbls)) +
    labs(x = "wave length", y="coverage") 

dev.off()



## hmm for some reason coverage goes down on higher values?
## could make sense for some differences, but multiples of lower values should be at least as complete as the lower values themselves, e.g. 8 should be as least as complete as 4
    
## error might have been agger fucking up by using df_anls instead of wave-length specific dfx
## now small fluctuations (also going down), but i think those should be able to happen
    


## ** add lagged values

## overly messy way of lagging variables that creates intermediary vars because mutate/lag doesn't accept variablies as input

lagger <- function(dfx, vrbls_to_lag){
    for (varx in vrbls_to_lag){
        lag_name = paste(varx, "_lag1", sep = "")
        ## eval(parse("lag_name"))
        ## df_anls$var_to_lag <- df_anls[,c(varx)]
        ## df_anls[,"var_lagged"] <- mutate(group_by(df_anls, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
        ## df_anls[,lag_name] <- df_anls$var_lagged
        ## df_anls <- df_anls[,-which(names(df_anls) %in% c("var_to_lag", "var_lagged"))]

        dfx[,"var_to_lag"] <- dfx[,c(varx)]
        dfx[,"var_lagged"] <- mutate(group_by(dfx, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
        dfx[,lag_name] <- dfx[,"var_lagged"]

        dfx <- dfx[,-which(names(dfx) %in% c("var_to_lag", "var_lagged"))]
    }
    return(dfx)
}

vrbls_to_lag <- c("gdp_pcap", "gdp_pcapk", "gini", "nbr_opened")

df_anls <- lagger(df_anls, vrbls_to_lag)

filter(df_anls[,c("countrycode", "year", "nbr_opened", "nbr_opened_lag1", "gini", "gini_lag1")], countrycode == "USA")


## ** negative binomial

## *** example
## https://rdrr.io/cran/lme4/man/glmer.nb.html

## set.seed(101)
## dd <- expand.grid(f1 = factor(1:3),
##                   f2 = LETTERS[1:2], g=1:9, rep=1:15,
##           KEEP.OUT.ATTRS=FALSE)
## summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
## dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
## str(dd)
## require("MASS")## and use its glm.nb() - as indeed we have zero random effect:
## ## Not run: 
## m.glm <- glm.nb(y ~ f1*f2, data=dd, trace=TRUE)
## summary(m.glm)
## m.nb <- glmer.nb(y ~ f1*f2 + (1|g), data=dd, verbose=TRUE)
## m.nb
## ## The neg.binomial theta parameter:
## getME(m.nb, "glmer.nb.theta")
## LL <- logLik(m.nb)
## ## mixed model has 1 additional parameter (RE variance)
## stopifnot(attr(LL,"df")==attr(logLik(m.glm),"df")+1)
## plot(m.nb, resid(.) ~ g)# works, as long as data 'dd' is found


## *** pglm

## found.pglm.nb1 <- pglm(nbr_opened ~ nbr_opened_lag1, data = df_anls,
##                         family=negbin,
##                         model = "within",
##                         index = "countrycode")

## found.pglm.nb2 <- pglm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb3 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1), data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb4 <- pglm(nbr_opened ~ nbr_opened_lag1 + gini_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb5 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + gini_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## screenreg(list(found.pglm.nb1,found.pglm.nb2,found.pglm.nb3,found.pglm.nb4,found.pglm.nb5))



## *** glmer.nb


## **** full df

stop("before models")

regger.nb <- function(list_of_models, data){
    #' batch processing of mah negative binomial regression models
    reg_res <- mclapply(list_of_models, glmer.nb, data = data, mc.cores = 6)
    return(reg_res)
}

df_anls$gdp_pcapd <- df_anls$gdp_pcap/100000

reg_res <- regger.nb(list(
    nbr_opened ~ (1 | countrycode),
    nbr_opened ~ nbr_opened_cum + (1 | countrycode),
    nbr_opened ~ gdp_pcapd + (1 | countrycode),
    nbr_opened ~ nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),
    nbr_opened ~ nbr_opened_lag1 + gini_lag1 + (1 | countrycode),
    nbr_opened ~ nbr_opened_lag1 + nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode)),
    df_anls)

screenreg(reg_res)

x <- glmer.nb(nbr_opened ~ gdp_pcapk + (1 | countrycode), data = df_anls)
screenreg(x)


mod <- reg_res[[1]]

screenreg(mod)

add_beta_modelsummary <- function(mod){
    #' add standardized effect to model in normal SE spot, use t/z stat
    bs <- fixef(mod)
    if (len(bs) > 2){
        betas <- c(NA, lm.beta.lmer(mod))
        names(betas) <- names(bs)

    ## add some NA entries for "SD (Intercept)" and "SD (Observations)"
        betas_padded <- c(betas, NA, NA)
        names(betas_padded)[(len(betas_padded)-1):len(betas_padded)] <- c("SD (Intercept)", "SD (Observations)")
    }
    else {
        betas_padded <- c(rep(NA, len(bs)), NA, NA)
        names(betas_padded) <- c(names(bs), c("SD (Intercept)", "SD (Observations)"))
    }
    modsum_std <- modelsummary(mod,
                               vcov = list(betas_padded),
                               output = "modelsummary_list")

    return(modsum_std)
        
}


mods_stds <- mclapply(reg_res, add_beta_modelsummary)
coef_map <- c("(Intercept)", "nbr_opened_cum", "nbr_opened_cum_sqrd", "gini_lag1", "nbr_opened_lag1",
              "SD (Intercept)", "SD (Observations)")

modelsummary(mods_stds[2:5], 
             estimate = "{estimate}[{statistic}]{stars}",
             coef_map = coef_map,
             output = "markdown")


## **** visualization
library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

plot_summs(reg_res[c(2,4:6)], exp=F)
plot_coefs(reg_res[3:6], facet.rows = 5)


## **** manual regression 



stop("functionalized models done")
## clean up the non-functionalized regressions below when I actually do them 

print("nb df_anls 1")
found.nb_fe_all <- glmer.nb(nbr_opened ~ (1 | countrycode), data = df_anls)
print("nb df_anls 2")
found.nb_nbr_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_anls)
found.nb_cum_all <- glmer.nb(nbr_opened ~ nbr_opened_cum + (1 | countrycode),  data = df_anls)
found.nb_cum_all_sqrd <- glmer.nb(nbr_opened ~ nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),  data = df_anls)
found.nb_org_pop <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),  data = df_anls)

print("nb df_anls 3")
found.nb_gdp_all <- glmer.nb(nbr_opened ~  gdp_pcapk_lag1 + (1 | countrycode), data = df_anls)
print("nb df_anls 4")
found.nb_gini_all <- glmer.nb(nbr_opened ~ gini_lag1  + (1 | countrycode), data = df_anls)
print("nb df_anls 5")
found.nb_all_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcapk_lag1 + gini_lag1 + (1 | countrycode), data = df_anls)

screenreg(list(found.nb_nbr_all, found.nb_cum_all, found.nb_cum_all_sqrd, found.nb_org_pop))
stargazer(list(found.nb_nbr_all, found.nb_cum_all, found.nb_cum_all_sqrd, found.nb_org_pop), type = "text")

found.nb_all_all_std <- createTexreg(coef.names = names(fixef(found.nb_all_all))[2:length(names(fixef(found.nb_all_all)))],
                                     coef = lm.beta.lmer(found.nb_all_all))


model_list_all <- list(found.nb_fe_all, found.nb_nbr_all, found.nb_gdp_all, found.nb_gini_all, found.nb_all_all, found.nb_all_all_std)


screenreg(model_list_all,
          custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list_all[1:5], function(x) sum(x@frame$nbr_opened))), 273)))


texreg(model_list_all,
          custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list_all[1:5], function(x) sum(x@frame$nbr_opened))), 273)),
       file = paste0(TABLE_DIR,"nb_all.tex"),
       label = "nb_all",
       caption = "Negative Binomial with full DF"
       )

## **** aggregate 4

df_lag4 <- df_agg4$df
df_lag4 <- lagger(df_lag4, vrbls_to_lag)

filter(df_lag4, countrycode=="USA")[,c("nbr_opened", "nbr_opened_lag1", "gini", "gini_lag1", "gdp_pcapk", "gdp_pcapk_lag1")]





print("lag4 1")
found.nb_fe <- glmer.nb(nbr_opened ~ (1 | countrycode), data = df_lag4)
print("lag4 2")
found.nb_nbr <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_lag4)
print("lag4 3")
found.nb_gdp <- glmer.nb(nbr_opened ~  gdp_pcapk_lag1 + (1 | countrycode), data = df_lag4)
print("lag4 4")
found.nb_gini <- glmer.nb(nbr_opened ~ gini_lag1  + (1 | countrycode), data = df_lag4)
print("lag4 5")
found.nb_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcapk_lag1 + gini_lag1 + (1 | countrycode), data = df_lag4)

found.nb_all_std <- createTexreg(coef.names = names(fixef(found.nb_all))[2:length(names(fixef(found.nb_all)))], coef = lm.beta.lmer(found.nb_all))

model_list <- list(found.nb_fe, found.nb_nbr, found.nb_gdp, found.nb_gini, found.nb_all, found.nb_all_std)

screenreg(model_list, custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list[1:5], function(x) sum(x@frame$nbr_opened))), 347)))



texreg(model_list,
       custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list[1:5], function(x) sum(x@frame$nbr_opened))), 347)),
       file = paste0(TABLE_DIR,"nb_agg4.tex"),
       label = "nb_agg4",
       caption = "Negative Binomial, aggregated to 4 year intervals"
       )

stop("models done")




## fastest way of standardized coefs: just add another model 

## screenreg(list(found.nb0, found.nb1, found.nb2,found.nb3,found.nb4,found.nb5))
## screenreg(list(found.nb1,found.pglm.nb1,found.nb3,found.pglm.nb3,found.nb4,found.pglm.nb4,found.nb5,found.pglm.nb5))


## model 2 (not log-transfomed gdp_pcap) doesn't work
## think the differences are smaller than in poisson:
## at least the general direction, but still bunch of differences with significance in nbr_opened_lag1 (between 3 and 4), gini (5)

## but i still don't know what's happening, and why things are different
## also need to understand the techniques: negative binomial/poisson especially

## *** interpreation
## can exp(coefs) to get ratios with a unit change
## A country's rate of founding PMs increases by exp(0.78) = 2.18 for each log(GDP) point
## A country's founding rate of PMs increases by exp(0.07) = 1.07 (7%) for each gini point


## https://cran.r-project.org/web/packages/effectsize/vignettes/from_test_statistics.html
library(effectsize)
anova(found.nb_all)

## anova result looks different 

## https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
## library(MuMIn)
## r.squaredGLMM(found.nb5x)
## marginal and conditional
## marginal: variance explained by fixed effects
## conditional: variance explained by entire model, including both FE/RE

## methods:
## - delta: for all distributions/links
## - lognormal, trigamma: only for logarithmic link
##   no idea if negbin has logarithmic link
## - trigamma recommended when available

## not all R^2 algorithms make sense?: https://stats.stackexchange.com/questions/250984/pseudo-r2-values-for-negative-binomial-regression-model-in-r-yields-inconsistent
## stata: https://stats.idre.ucla.edu/stata/output/negative-binomial-regression/: McFadden's pseudo R-squared means something different in negbin than in OLS -> interpret with caution

## why don't we do k-fold validation: precision, recall, and whatever performance techniques ML/AI has come up with
## https://stackoverflow.com/questions/63208120/how-can-i-use-k-fold-cross-validation-for-negative-binomial-regression-in-sklear
## could implement 





## ** visualization/inspection



library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

## plot_summs(found.nb1, found.nb3, found.nb4, found.nb5, plot.distributions = T)
## plot_summs looks nice, but idk if I shouldn't rather write my own ggplot visualization
## also no support for the pglm models -> idk if plot_summs has generic class that can be filled wiht arbitrary values
## also need standardized stuff
## also not that good at comparing models with different variables
## plotting distributions gets very full when more than a handful

## *** curves


## *** plot country trajectories with PM foundings
gdp_pcap_agg <- as_tibble(aggregate(gdp_pcapk ~ countrycode, df_anls, mean))
gini_agg <- as_tibble(aggregate(gini ~ countrycode, df_anls, mean))

var_means <- as_tibble(merge(gdp_pcap_agg, gini_agg, all.x = TRUE))

names(var_means) <- c("countrycode", "gdp_pcapk_mean", "gini_mean")

df_anls_vis <- as_tibble(merge(df_anls[,c("countrycode", "year", "nbr_opened", "gini", "gdp_pcapk")] , var_means, by='countrycode'))
df_anls_vis$gini_demeaned <- df_anls_vis$gini - df_anls_vis$gini_mean
df_anls_vis$gdp_pcapk_demeaned <- df_anls_vis$gdp_pcapk - df_anls_vis$gdp_pcapk_mean

## sum(df_anls_vis[which(df_anls_vis$countrycode == "DEU"),]$gdp_pcapk_demeaned)



plt1 <- ggplot(df_anls_vis, aes(x=year, y=gdp_pcapk_demeaned, group=countrycode, color = countrycode)) + 
    geom_line(alpha=0.15) +
    geom_line(df_anls_vis[which(df_anls_vis$countrycode %in% c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")),], mapping = aes(x=year, y=gdp_pcapk_demeaned, group=countrycode, color = countrycode), alpha = 0.8) +
    scale_color_discrete(breaks = c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")) + 
    geom_point(df_anls_vis[which(df_anls_vis$nbr_opened > 0),], mapping = aes(x=year, y=gdp_pcapk_demeaned, size = nbr_opened, color = countrycode), alpha = 0.8) +
    labs(y = "gdp_pcapk_demeaned")
plt1


plt2 <- ggplot(df_anls_vis, aes(x=year, y=gini_demeaned, group=countrycode, color = countrycode)) + 
    geom_line(alpha=0.15) +
    geom_line(df_anls_vis[which(df_anls_vis$countrycode %in% c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")),], mapping = aes(x=year, y=gini_demeaned, group=countrycode, color = countrycode), alpha = 0.8) +
    scale_color_discrete(breaks = c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")) + 
    geom_point(df_anls_vis[which(df_anls_vis$nbr_opened > 0),], mapping = aes(x=year, y=gini_demeaned, size = nbr_opened, color = countrycode), alpha = 0.8) +
    labs(y = "gini_demeaned")
plt2

pdf(paste0(FIG_DIR, "fe_viz.pdf"), height = 9, width = 9)
grid.arrange(plt1,plt2)
dev.off()


stop("plots and tables done")






## ** robust SEs
## https://stackoverflow.com/questions/26412581/robust-standard-errors-for-mixed-effects-models-in-lme4-package-of-r

## interpretation: the more points are above 0, the stronger the effect of that variable? 

## wow Italian gdp per capita, wtf happened


## maybe can plot hist of those points directly? 
## hist(df_anls_vis[which(df_anls_vis$nbr_opened > 0),]$gdp_pcapk_demeaned, breaks = 20)
## hist(df_anls_vis[which(df_anls_vis$nbr_opened > 0),]$gini_demeaned, breaks = 20)
## makes it looks murky af
## but wide spread doesn't mean that it can't be distinguished from 0
## but probably still too visualization-driven: it's a measure I only came up with because of visualization


## hmm actually you don't need lines, or can put high alpha there 
## raises question of how robust model is: is it all driven by US/Germany?



## ** poisson test

## function is
x <- seq(1,10,1)

poi.func <- function(x, lambda){
    return(((lambda^x)/(factorial(x))) * exp(1)^(-lambda))
    }


plot(poi.func(x,1), type='l')

for (i in seq(1,10,1)){
    lines(poi.func(x,i), type='l')
    }


plot(x/20, col='white')
lines(poi.func(x,2), type='l')
plot(2^x, type='l')
lines(factorial(x))




## *** pglm
library(plm)
## seems like poisson/negative binomial is implemented in https://cran.r-project.org/web/packages/pglm/pglm.pdf
## yves croissant, wrote some b

library(pglm)

found.pglm.poi1 <- pglm(nbr_opened ~ nbr_opened_lag1,
                        data = na.omit(df_anls[,c("countrycode", "nbr_opened", "nbr_opened_lag1")]),
                        family=poisson,
                        model = "within",
                        index = "countrycode")

found.pglm.poi2 <- pglm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi3 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1), data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi4 <- pglm(nbr_opened ~ nbr_opened_lag1 + gini_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi5 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + gini_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")


## *** comparison

summary(found.pglm.poi1)
screenreg(list(found.pglm.poi1,found.poi1))

screenreg(list(found.pglm.poi1,found.poi1,
               found.pglm.poi2,found.poi2,
               found.pglm.poi3,found.poi3,
               found.pglm.poi4,found.poi4,
               found.pglm.poi5,found.poi5))

## hmm different results
## overall tendencies are kinda the same, but not always: gini significant in one, not the other
## also differences in significance of nbr_opened_lag1
## only aggreement in significance in gdp_pcap_lag1 and log(gdp_pcap_lag1)






## ** visualization


df_plt <- df_anls
df_plt$region <- countrycode(df_plt$countrycode, "iso3c", "region")


## *** n-year cuts

year_selector <- function(x)(
    # convert cuts back to years
    substring(x, 2,5))


df_plt$cut <- cut(df_plt$year, seq(min(df_plt$year), max(df_plt$year)+5, by = 3))
df_plt$cut2 <- as.numeric(sapply(as.character(df_plt$cut), year_selector))

df_viz <- as_tibble(aggregate(nbr_opened ~ region + cut2, df_plt, sum))


## founding rates
## first country mean per cut
## then region sum 
df_viz_pop1 <- as_tibble(aggregate(population ~ countrycode + cut2 , df_plt, mean))
filter(df_viz_pop1, countrycode == "DEU")
df_viz_pop1$region <- countrycode(df_viz_pop1$countrycode, "iso3c", "region")

countrycode(unique(filter(df_viz_pop1, region == "South Asia")$countrycode), "iso3c", "country.name")


df_viz_pop2 <- as_tibble(aggregate(population ~ region + cut2, df_viz_pop1, sum))

## ggplot(df_viz_pop2, aes(x=cut2, y=population, group=region, color=region)) +
##     geom_line()



df_viz_pop3 <- as_tibble(merge(df_viz, df_viz_pop2))
df_viz_pop3$rate <- df_viz_pop3$nbr_opened/(df_viz_pop3$population/1e+8)


p_abs <- ggplot(df_viz, aes(x=cut2, y=nbr_opened, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(x="year (3 year aggregate)", y= "number opened", title = 'absolute')

p_rel <- ggplot(df_viz_pop3, aes(x=cut2, y=rate, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="foundings per 100m", x="year (3 year aggregate)", title = 'relative')


pdf(paste0(FIG_DIR, "foundings_cut3.pdf"), height = 9, width = 9)
ggarrange(p_abs, p_rel, nrow = 2)
dev.off()

## *** rolling average


df_viz_rol1 <- as_tibble(aggregate(cbind(population, nbr_opened) ~ region + year, df_plt[,c("countrycode", "year", "region", "population", "nbr_opened")], sum))

ROLLING_AVG_LEN <- 4

df_viz_rol2 <- df_viz_rol1  %>% group_by(region) %>% mutate(nbr_opened_rollavg = runMean(nbr_opened, ROLLING_AVG_LEN))
df_viz_rol2 <- df_viz_rol2  %>% group_by(region) %>% mutate(population_rollavg = runMean(population, ROLLING_AVG_LEN))

df_viz_rol2$rate_rollavg <- df_viz_rol2$nbr_opened_rollavg/(df_viz_rol2$population_rollavg/1e+8)

p_ra_abs <- ggplot(df_viz_rol2, aes(x=year, y=nbr_opened_rollavg, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs (y = "number opened (4 year rolling average)", title = "absolute")

p_ra_rel <- ggplot(df_viz_rol2, aes(x=year, y=rate_rollavg, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="foundings per 100m (4 year rolling average)", title = "relative")

pdf(paste0(FIG_DIR, "foundings_ra4.pdf"), height = 9, width = 9)
ggarrange(p_ra_abs, p_ra_rel, nrow = 2)
dev.off()


filter(df_viz_rol2, year > 2017 & region == "Middle East & North Africa")


## atm it's per hundred million
## even the absolute peak of founding was less than 5 per 100 million
## could even do per billion
## but not good: US/EU don't have more than a billion -> hard to understand what 30 per billion means; 3 per 100 million is much easier to imagine 


## hehehe Europe super strong
## weird how active Europe europe is in 80s


## *** rolling average country-wise rate
df_viz_rol_cry <- as_tibble(aggregate(cbind(population, nbr_opened) ~ countrycode + year, df_plt[,c("countrycode", "year", "region", "population", "nbr_opened")], sum))

ROLLING_AVG_LEN <- 5

df_viz_rol_cry <- df_viz_rol_cry %>%
    group_by(countrycode) %>%
    mutate(population_rollavg = rollmean_custom(population, win_len=ROLLING_AVG_LEN, orientation = "left"),
           nbr_opened_rollavg = rollmean_custom(nbr_opened, win_len=ROLLING_AVG_LEN, orientation = "left")
           )


df_viz_rol_cry$rate_rollavg <- df_viz_rol_cry$nbr_opened_rollavg/(df_viz_rol_cry$population_rollavg/1e+8)

df_viz_rol_cry <- filter(df_viz_rol_cry, countrycode %in% country_max_codes)

df_viz_rol_cry$country <- countrycode(df_viz_rol_cry$countrycode, "iso3c", "country.name")

p_ra_rate_cry <-
    ggplot(df_viz_rol_cry, aes(x=year, y=rate_rollavg, group=country, color=country)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y=paste0("foundings per 100m (", ROLLING_AVG_LEN ," years rolling average)"))

## p_ra_rate_cry

## *** rolling average country-wise absolute count 
p_ra_cnt_cry <- ggplot(df_viz_rol_cry, aes(x=year, y=nbr_opened_rollavg, group=country, color=country)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y=paste0("foundings per country (", ROLLING_AVG_LEN ," years rolling average)"))

pdf(paste0(FIG_DIR, "foundings_country_cnt_and_rate.pdf"), height = 9, width = 9)
ggarrange(p_ra_cnt_cry, p_ra_rate_cry, nrow = 2)
dev.off()



## *** cumulative

df_viz_rol1$nbr_opened_cum <- ave(df_viz_rol1$nbr_opened, df_viz_rol1$region, FUN = cumsum)

df_viz_rol1 <- df_viz_rol1 %>%
    group_by(region) %>%
    mutate(nbr_opened_max = max(nbr_opened_cum))

df_viz_rol1 <- mutate(df_viz_rol1, prop_cum = nbr_opened_cum/nbr_opened_max)

df_viz_rol1$alpha <- 1
df_viz_rol1[which(df_viz_rol1$nbr_opened_max < 50),'alpha'] <- 0.9


p_cum_abs <- ggplot(df_viz_rol1, aes(x=year, y=nbr_opened_cum, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="number opened (cumulative)")

p_cum_rel <-
    ggplot(df_viz_rol1, aes(x=year, y=prop_cum, group=region, color=region, alpha=alpha)) + 
    geom_line(size=1) +
    scale_alpha_continuous(range = c(0.3, 1), guide=FALSE) + 
    scale_color_brewer(palette="Dark2") +
    labs(y="proportion opened", caption = "regions with less PMs de-emphasized")


pdf(paste0(FIG_DIR, "foundings_cum.pdf"), height = 9, width = 9)
ggarrange(p_cum_abs, p_cum_rel, nrow = 2)
dev.off()

## europe slightly in front of NA, but East Asia (Korea, China) at same stage/ahead of Europe
## maybe due to korea? 
aggregate(nbr_opened ~ country, filter(df_plt, region == "East Asia & Pacific"), sum)
## -> do for countries with most PMs

country_max <- aggregate(nbr_opened_cum ~ countrycode, df_plt, max)
country_max_codes <- country_max[rev(order(country_max$nbr_opened_cum))[c(1:12)],]$countrycode


## try selecting countries with highest PM concentration per population, end up with Monaco, and mostly other small countries that have like 2-4 PMs: "MCO" "ISL" "CYP" "BEL" "CHE" "KOR" "DEU" "EST" "LBN" "NLD" "AUT" "GRC"
## df_plt$rate_opened_cum <- df_plt$nbr_opened_cum/(df_plt$population/1e+08)
## country_max_prop <- aggregate(rate_opened_cum ~ countrycode, df_plt, max)
## country_max_prop_codes <- country_max[rev(order(country_max_prop$rate_opened_cum))[c(1:12)],]$countrycode
## country_max_codes <- country_max[rev(order(country_max_prop$rate_opened_cum))[c(1:12)],]$countrycode


df_plt_cry <- filter(df_plt, countrycode %in% country_max_codes)

df_plt_cry <- df_plt_cry %>%
    group_by(countrycode) %>%
    mutate(nbr_opened_max = max(nbr_opened_cum))

df_plt_cry$prop_cum <- df_plt_cry$nbr_opened_cum/df_plt_cry$nbr_opened_max

df_plt_cry$lt <- recode(df_plt_cry$region,
    "Europe & Central Asia" = "twodash",
    "East Asia & Pacific" = "solid",
    "North America" = "F1")

df_plt_cry$country <- countrycode(df_plt_cry$countrycode, "iso3c", "country.name")

p_cry_cum_abs <- ggplot(df_plt_cry, aes(x=year, y=nbr_opened_cum, group=country, color=country)) +
    geom_line(size=2) +
    scale_color_brewer(palette = "Paired") +
    labs(y="number opened (cumulative)")

p_cry_cum_rel <-
    ggplot(df_plt_cry, aes(x=year, y=prop_cum, group=country, color=country, linetype=region)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y="proportion opened", caption = "12 countries with most PMs (70% of PMs)")


## also get cumulative rate: end is how many per 100m are there atm 
df_plt_cry$rate_opened_cum <- df_plt_cry$nbr_opened_cum/(df_plt_cry$population/1e+07)

p_cry_cum_rate <-
    ggplot(df_plt_cry, aes(x=year, y=rate_opened_cum, group=country, color=country)) +
    geom_line(size=2) +
    scale_color_brewer(palette = "Paired") +
    labs(y="number opened per 10m population (cumulative)")



pdf(paste0(FIG_DIR, "foundings_country_cumulative.pdf"), height = 12, width = 9)
ggarrange(p_cry_cum_abs, p_cry_cum_rel, p_cry_cum_rate, nrow = 3)
dev.off()

## China really late, Korea really early -> there's much variation within region
## but no idea how to visualize that.. ribbons? it's not an uncertainty estimate, i know the numbers
## think it makes rather sense to plot countries individually, maybe line-type by region 
## I don't trust these korea numbers
## US/Germany: have the most, and are weirdly in the middle, would have expected them to be leaders
## could be data issue: only good for them, and for the others the curves are fluctuating randomly because coverage incomplete




## ** looking into how messy variables can be sanitized automatically, little success so far

## *** variable completeness

var_cpltns <- apply(df, 2, function(x) table(is.na(x))[1])
var_cpltns[order(var_cpltns)]
var_cpltns_df <- as.data.frame(var_cpltns[order(var_cpltns)])

## would be interesting, but only few results
## - governance structure : 117 



## *** genre focus 
df$"Collection genre focus"
summary(df$"Collection genre focus")
table(is.na(df$"Collection genre focus"))

df$collection_genre_focus <- df$"Collection genre focus"

## *** size 
table(is.na(df$"Floor size"))

df$"Floor size"[!is.na(df$"Floor size")]

df$floor_size <- df$"Floor size"

df[which(df$floor_size == "NA"),]$floor_size <- NA

table(df$floor_size)


## *** activities
df$activities <- df$"Educational / outreach / social / artistic programs"
strsplit(df$activities, split=c(",|:|;"))
strsplit(df$activities[553], ',')
lapply(df$activities, strsplit, split=c(","))

strsplit("asdf,jk;l:l", c(",|;|:"))

strsplit2 <- function(x, min_len){
    chars <- c()
    ## for (i in (nchar(x)-min_len -1):nchar(x)){
    for (end in min_len:nchar(x)){
        chars <- c(chars, substring(x, 1:(nchar(x)-min_len +1), end))
    }
    return(unique(chars[which(lapply(chars, nchar) >= min_len)]))
}
         
strsplit2("abcdefg", 3)
## strsplit2(df$activities[553], 3)[4728]
## x <- df$activities[553]

## *** mission/vision


df$mission <- df$"Mission / vision"
table(is.na(df$mission))
hist(unlist(lapply(df$mission, nchar)), breaks = 40)

library(topicmodels)
library(tm)

vector_source <- VectorSource(df$mission)
corpus <- Corpus(vector_source)

review_corpus = tm_map(corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
## review_corpus = tm_map(review_corpus, removeWords, c("art", "museum", "contemporary", "collection", "artists", "exhibitions", "works"))
review_corpus = tm_map(review_corpus, removeWords, c(stopwords("german")))
review_corpus =  tm_map(review_corpus, stripWhitespace)

dtm <- DocumentTermMatrix(review_corpus)
## dropping documents with 0 terms
dtm <- dtm[which(apply(dtm, 1, sum) !=0),]

lda_res <- LDA(dtm, k=2)

library(tidytext)
lda_topics <- tidy(lda_res, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

## top_terms %>%
##   mutate(term = reorder_within(term, beta, topic)) %>%
##   ggplot(aes(beta, term, fill = factor(topic))) +
##   geom_col(show.legend = FALSE) +
##   facet_wrap(~ topic, scales = "free") +
##   scale_y_reordered()


beta_wide <- lda_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide2 <- rbind(beta_wide[order(beta_wide$log_ratio)[c(1:10)],],
                    beta_wide[rev(order(beta_wide$log_ratio))[c(1:10)],])

beta_wide2 <- beta_wide2[order(beta_wide2$log_ratio),]
beta_wide2$term <- factor(beta_wide2$term, levels = beta_wide2$term[order(beta_wide2$log_ratio)])

## ggplot(beta_wide2, aes(x=term, y=log_ratio)) +
##     geom_bar(stat="identity") +
##     coord_flip()


## * scrap
## ** test different kinds of aggregation

## wave_length <- 4
## wv_ctr <- 0
## wv_nbr <- 1

## df_anls$wv <- 0

## ## label the waves based on wavelength, not super elegant but works

## for (yearx in unique(df_anls$year)){
##     wv_ctr <- wv_ctr + 1
##     print(c(wv_ctr, wv_nbr))
##     df_anls[which(df_anls$year == yearx),"wv"] <- wv_nbr
##     if (wv_ctr == wave_length){
##         wv_ctr <- 0
##         wv_nbr <- wv_nbr + 1}
##     }



## merge test start
## df_agg_test <- as_tibble(cbind(c(1,1,1,2,2,2,3,3,3), c(NA,NA,1,NA,2,3,NA,NA,NA)))
## names(df_agg_test) <- c("group", "value")
## df_agg_test$value[which(is.na(df_agg_test$value))] <- "NA"
## aggregate(value ~ group, df_agg_test, mean)

## ## can't get NAs into the aggregation if there's no non-NA value 
## ## probably have to aggregate separately with na.omit, and then merge back to overall df


## df_agg_gini <- as_tibble(aggregate(gini ~ countrycode + wv, df_anls, mean, na.action = na.omit))

## ## make some arbitrary aggregation to merge other aggregations back to
## df_agg_gini2 <- as_tibble(merge(df_agg_cnts, df_agg_gini, by=c('countrycode', 'wv'), all.x = TRUE))


## df_agg_pcap <- as_tibble(aggregate(gdp_pcap ~ countrycode + wv, df_anls, mean))


## merge test end 


## ahh, what happened was that since I aggregated the variables in the same function they all got down to gini level
## yeah even though stuff is not deleted, it still means that all entries are fully complete -> get reduced down to lowest value

## 



## ## use reduce to join the means together 
## df_agg_means <- as_tibble(Reduce(
##     function(x,y, ...) merge(x,y, all = TRUE),
##     lapply(c("gini", "gdp_pcap", "gdp_pcapk"), agger, )
## ))


## df_agg_cnts <- as_tibble(aggregate(nbr_opened ~ countrycode + wv, df_anls, sum))

## ## ggplot(df_agg_means, aes(x=wv, y=gini, group = countrycode, color = countrycode)) +
## ##     geom_line()

## df_agg <- as_tibble(merge(df_agg_cnts, df_agg_means, by=c("countrycode", "wv"), all.x = T))


## ## checking how many museums are covered 

## ## not removing anything: 449
## sum(df_anls$nbr_opened)

## ## whenever I do gini with 5 years, I only have 260 or 263?? opening events
## sum(na.omit(df_anls[,c("gini","nbr_opened")])$nbr_opened)

## ## filtering for gdp_pcap: still 388
## sum(na.omit(df_anls[,c("gdp_pcap","nbr_opened")])$nbr_opened)

## ## even aggregating gini to 5 years: only get to 345,
## sum(na.omit(df_agg[,c("gini","nbr_opened")])$nbr_opened)
## ## with 2: 298
## ## with 3: 327
## ## with 5: 345
## ## with 8 to 345
## ## with 10 to 364
## sum(na.omit(df_agg[,c("gdp_pcap","nbr_opened")])$nbr_opened)
## ## pcap with 10: 388

## ## where the fuck are the museums founded for which I don't have data

## ## ggplot(df_anls, aes(x=year, y=countrycode, size = nbr_opened)) +
## ##     geom_point()

## df_agg$pm_preds_na <- 0
## df_agg[which(is.na(df_agg$gini)),]$pm_preds_na <- 1



## ggplot(df_agg, aes(x=factor(wv), y=countrycode, size = nbr_opened, color = factor(pm_preds_na))) +
##     geom_point()

## ## seems especially Korea, Russia -> country specific, not time-period effect
## ## for wave_length 2: also germany, india, japan
## mis <- aggregate(pm_preds_na ~ countrycode, df_agg[which(df_agg$nbr_opened > 0),], sum)
## mis[order(mis$pm_preds_na),]

## ## gdp_pcap seem to be mostly korea

## ** gtsummary: written for rstudio 
## mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)
## t1 <- tbl_regression(mod1, exponentiate = TRUE)
## gtsave(t1, filename = paste0(TABLE_DIR, "gtsummarytest.tex"))


## library(gtsummary); library(gt); library(dplyr)

## trial %>%
##   select(trt, age, grade) %>%
##   tbl_summary(by = trt) %>%
##     add_p() %>%
##     gt::gtsave(filename = paste0(TABLE_DIR, "gtsummarytest.tex")

## ** parallelization test 

## https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
## reg_obj <- nbr_opened ~ nbr_opened_lag1 + (1 | countrycode)
## reg_objs <- rep(list(reg_obj), 4)

## library(parallel)
## res_objs <- mclapply(reg_objs, glmer.nb, data = df_lag4, mc.cores = 4)

## * literature table
## ** organizational population
con_obvz <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "obvz")

dbSendQuery(con_obvz, "set joined_subquery_requires_alias=0")

lit_df <- as_tibble(dbGetQuery(con_obvz, "SELECT child, parent FROM (
    SELECT DISTINCT(child) AS child FROM bc WHERE parent IN ['ecology', 'legitimation', 'competition', 'founding', 'contagion', 'diffusion', 'density']
  ) JOIN (SELECT parent, child FROM bc WHERE parent NOT IN ['cls_papers', 'cls_toread', 'sbcls_B', 'sbcls_A', 'sbcls_C', 'cls_orgform']) USING child"))


lit_df$ctr <- 1


lit_df_cast <- as_tibble(dcast(lit_df, child ~ parent, fill=0))


## only selecting top 100 terms 
colsums <- apply(lit_df_cast[,c(2:len(lit_df_cast))], 2, sum)
colnames <-names(colsums[rev(order(colsums))][c(1:50)])
colpos <- which(names(lit_df_cast) %in% colnames)

rowsums <- apply(lit_df_cast[,c(2:len(lit_df_cast))], 1, sum)

lit_df_cast2 <- lit_df_cast[rev(order(rowsums))[c(1:100)],c(1,colpos)]

lit_df_melt <- as_tibble(melt(lit_df_cast2, id=c('child')))


## idk if much value
## doesn't help that paper titles are hard af to read, fucking hurts my eyes
## probably should use Holst directly, not some shitty wrapper like cluster_matrix
## also these manual reordering is bloat

lit_df_cast3 <- as.data.frame(lit_df_cast2)

rownames(lit_df_cast3) <- lit_df_cast3$child
lit_df_cast3 <- lit_df_cast3[,-1]


d_papers <- dist(lit_df_cast3)
d_tags <- dist(t(lit_df_cast3))
clust_papers <- hclust(d_papers, method = "ward.D2")
clust_tags <- hclust(d_tags, method = "ward.D2")


clust_cut_papers <- cutree(clust_papers,8)
clust_cut_tags <- cutree(clust_tags,6)

table(clust_cut_papers)
table(clust_cut_tags)

## should be able to change the factor order to that they correspond to clusters
lit_df_melt$child_srt <- factor(lit_df_melt$child, levels=names(clust_cut_papers[order(clust_cut_papers)]))
lit_df_melt$variable_srt <- factor(lit_df_melt$variable, levels=names(clust_cut_tags[order(clust_cut_tags)]))


## can draw boundaries now, still need function to calculate them properly 

boundary_df <- data.frame(matrix(ncol=4, nrow=0))
names(boundary_df) <- c("xmin", "ymin", "xmax", "ymax")

for (i in seq(1, max(clust_cut_papers))){
    for (k in seq(1, max(clust_cut_tags))) {
        xmin <- len(which(clust_cut_tags < k)) + 0.5
        ymin <- len(which(clust_cut_papers < i)) + 0.5

        xmax <- len(which(clust_cut_tags < k+1)) + 0.5
        ymax <- len(which(clust_cut_papers < i+1)) + 0.5

        boundary_df[nrow(boundary_df)+1,] <- c(xmin, ymin, xmax, ymax)
    }
}

## geom_rect(mapping = aes(xmin=0.5,xmax=50, ymin=0.5,ymax=10), size=0.1, fill=alpha("grey", 0), color="green")


p1 <- ggplot(NULL)+
    geom_tile(data=lit_df_melt, mapping = aes(x = variable_srt, y=child_srt, fill=value)) +
    geom_rect(data=boundary_df, mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=alpha("grey", 0), color="grey") +
    theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), axis.title.x = element_blank())

pdf(paste0(FIG_DIR, "lit_table.pdf"), width=12,  height = 12)
print(p1)
dev.off()
   


## flipped version 
ggplot(NULL)+
    geom_tile(data=lit_df_melt, mapping = aes(x = child_srt, y=variable_srt, fill=value))+
    geom_rect(data=boundary_df, mapping = aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill=alpha("grey", 0), color="grey") +
    theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), axis.title.x = element_blank())


hist(aggregate(value ~ variable, lit_df_melt, sum)$value)


## some fit stats
## 


wss2 <- function(d) {
      sum(scale(d, scale = FALSE)^2)
  }


wrap <- function(i, hc, x) {
    cl <- cutree(hc, i)
    spl <- split(x, cl)
    wss <- sum(sapply(spl, wss2))
    wss
}

hclustAIC <- function(to_cut, n, dfx){
    ## n2 <- nrow(clust.df)
    n2 <- len(clust_papers[[2]])
    m <- 2
    D <- wrap(n, hc=to_cut, x=dfx)

    return(data.frame(
        n = n,
        AIC = D + 2*m*n,
        BIC = D + log(n2)*m*n))
}




clust_res_papers <- lapply(seq(1,12), function(x) hclustAIC(clust_papers, x, lit_df_cast3))
clust_res_papers2 <- do.call(rbind, clust_res_papers)
clust_res_papers2$topic <- "papers"

clust_res_tags <-  lapply(seq(1,12), function(x) hclustAIC(clust_tags, x, t(lit_df_cast3)))
clust_res_tags2 <- do.call(rbind, clust_res_tags)
clust_res_tags2$topic <- "tags"

clust_res_cbn <- rbind(clust_res_papers2, clust_res_tags2)
clust_res_melt <- as_tibble(melt(clust_res_cbn, id=c("n", "topic")))


ggplot(clust_res_melt, aes(x=n, y=value, group=interaction(topic, variable), color=interaction(topic, variable))) +
    geom_line()

## ** subsidies

                        

subsidies_lit_df <- as_tibble(dbGetQuery(con_obvz, "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['state-support', 'subsidies', 'finance', 'policy', 'donation', 'tax-deduction', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"))


subs_cast <- dcast(subsidies_lit_df, child ~ parent, length)
subs_cast$fully_read <- subs_cast$cls_papers
subs_cast <- subs_cast[,-c(which(names(subs_cast) %in% c("cls_papers", "cls_toread")))]

subs_cast_fltrd <- subs_cast[which(rowSums(subs_cast[,c(2:7)]) > 0),]
nrow(subs_cast_fltrd)

write.csv(subs_cast_fltrd, paste0(TABLE_DIR, "subsidies_lit.csv"))

## ** functionalization

lit_tbl_gnrtr <- function(query, labelx) {
    #' generates lit table from clickhouse query
    #' "SELECT child, parent FROM (
    #' SELECT child, parent FROM bc
    #' WHERE parent IN ['list', 'of', 'tags', 'cls_papers', 'cls_toread']) JOIN 
    #' (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"
    
    lit_df <- as_tibble(dbGetQuery(con_obvz, query))
    lit_df_cast <- dcast(lit_df, child ~ parent, length)
    lit_df_cast$fully_read <- lit_df_cast$cls_papers
    
    lit_df_cast <- lit_df_cast[,-c(which(names(lit_df_cast) %in% c("cls_papers", "cls_toread")))]
    print(nrow(lit_df_cast))
    ## lit_df_cast
    ## rowSums doesn't work with just one colunm

    ## have to filter because getting fully read indicator means I have to have cls_toread/cls_papers 
    ## -> then gets all the entries regardless of tag

    if (ncol(lit_df_cast)>3){
        lit_df_cast_fltrd <- lit_df_cast[which(rowSums(lit_df_cast[,c(2:(ncol(lit_df_cast)-1))]) > 0),]
    } else {

        lit_df_cast_fltrd <- lit_df_cast[which(lit_df_cast[,c(2:(ncol(lit_df_cast)-1))] > 0),]
    }
    ## print(nrow(lit_df_cast_fltrd))
    print(lit_df_cast_fltrd)
    write.csv(lit_df_cast_fltrd, paste0(TABLE_DIR, paste0(labelx, ".csv")))
}

## ** inequality 
ineq_query <- "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['inequality', 'elites', 'concentration', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"


lit_tbl_gnrtr(ineq_query, "inequality_lit")

## *** looking for general mechanisms of inequality motivating elites to do stuf
ineq_query2 <- "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['inequality', 'elites', 'concentration', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='legitimation' or parent='legitimacy') USING child"

lit_tbl_gnrtr(ineq_query2, "inequality_lit2")


## ** status 
status_query <- "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['status', 'reputation', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"

lit_tbl_gnrtr(status_query, "status_lit")

## ** isomorphism
isomorphism_query <- "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['isomorphism', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"

lit_tbl_gnrtr(isomorphism_query, "isomorphism_lit")

## * pdf stuff
library(pdftools)
READINGS_DIR <- "/home/johannes/Dropbox/readings/"
x <- pdf_text(paste0(READINGS_DIR, "Walker_2019_collector.pdf"))

some_page <- x[[1]]

## would be necessary to make all the aliases of museum names



## * oecd
## ** library access, doesn't have labels tho 
library(OECD)
df_oced <- as_tibble(get_datasets())
## 1495 datasets noice

df_stan <- as_tibble(
    get_dataset("STANI4_2020", start_time = 2010, end_time = 2011,
                filter = list(c("DEU"))
                ))
## there's probably some query limit on the number of rows




dfx <- as_tibble(get_dataset("EPL_OV", 
                   filter = list(c("DEU", "FRA"), 
                                 c("EPRC_V1", "EPRC_V2")), 
                   start_time = 2008, end_time = 2010))


wdix <- read_dta("/home/johannes/ownCloud/wid/wid_2018_report/Computer Codes/Global Wealth Inequality/gpinterized.dta")
## only about   CN   FR   GB   US   WO

wdix <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/wid-data-25102021.dta"))
wdix_hist <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/WO_hist.dta"))

dfx2 <- as_tibble(get_dataset("SNA_TABLE11",
                              start_time = 2010, end_time = 2011,
                              filter = list(c("DEU"))
                ))

## ** sdmx parsing tests

SDMX_DIR <- "/home/johannes/ownCloud/oecd/SDMX/"

for (idx in df_oced$id){
    sdmx_url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", idx)
    dest_file <- paste0(SDMX_DIR, idx, ".xml")
    download.file(sdmx_url, dest_file)
}


library(rsdmx)

idx <- "STANI4"

sdmx_test <- readSDMX(dest_file, isURL = FALSE)


slotNames(sdmx_test)
## [1] "organisationSchemes" "concepts"            "codelists"          
## [4] "datastructures"      "xmlObj"              "schema"             
## [7] "header"              "footer"             
## these 8 overall slots seem to be general thing? 


cls <- slot(sdmx_test, "codelists")
slotNames(cls)
typeof(slot(cls, "codelists")) ## a list
len(slot(cls, "codelists"))


codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id")) #get list of codelists
codelist <- as.data.frame(slot(sdmx_test, "codelists"), codelistId = "CL_STANI4_IND") #get a codelist

## sdmx basically seems like annoyingly formatted  dict

codelist$idx <- idx

SDMX_TBL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_based_tables/"

write.csv(codelist, paste0(SDMX_TBL_DIR, idx, ".csv"))

## ** sdmx parsing proc

files_there <- list.files(SDMX_DIR)

SDMX_FAIL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_parsing_fail"
SDMX_FAIL_FILE <- paste0(SDMX_FAIL_DIR, "/fails.csv")

proc_codelist <- function(dsd, codelistx, sdmx_id) {
    #' convert sdmx codelist into df, outsourced to own function for better trycatching
    codelist_df <- as.data.frame(slot(dsd, "codelists"), codelistId = codelistx)
    names_codelist_df <- names(codelist_df)
    codelist_df$codelist <- codelistx
    codelist_df$sdmx_id <- sdmx_id
    ## reorder the columns so that I can easier grep/awk the databases/codelists where culture terms occur
    codelist_df <- codelist_df[,c("sdmx_id", "codelist", names_codelist_df)]

    filename <- paste0(SDMX_TBL_DIR, sdmx_id, "---", codelistx, ".csv")
    write.csv(codelist_df, filename)
}


proc_sdmx_file <- function(sdmx_file){
    sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
    dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
    ## print(slotNames(dsd)) overall slotnames are the same
    cls <- slot(dsd, "codelists")
    codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
    ## print(codelists) ## not every SDMX has the same codelist, unsurprisingly

    for (codelistx in codelists){
        print(codelistx)
        res <- tryCatch(
            proc_codelist(dsd, codelistx, sdmx_id),
            error=function(e) {
                FAILED_LIST <- c(failed_list, c(sdmx_id, codelistx))
                ## print(c(failed_list, c(sdmx_id, codelistx)))

                ## save which files failed to parse 
                write.table(paste(sdmx_id, codelistx, sep=","), file=SDMX_FAIL_FILE, append=TRUE,
                            col.names = FALSE, row.names = FALSE, quote = FALSE)
                
                }
            
        )
        ## codelist_df <- as.data.frame(slot(dsd, "codelists"), codelistId = codelistx)

        ## names_codelist_df <- names(codelist_df)
        ## codelist_df$codelist <- codelistx
        ## codelist_df$sdmx_id <- sdmx_id
        ## ## reorder the columns so that I can easier grep/awk the databases/codelists where culture terms occur
        ## codelist_df <- codelist_df[,c("sdmx_id", "codelist", names_codelist_df)]

        ## filename <- paste0(SDMX_TBL_DIR, sdmx_id, "---", codelistx, ".csv")
        ## write.csv(codelist_df, filename)
        
        ## print(codelist_df)
        ## print(paste(len(names(codelist_df)), paste(names(codelist_df), collapse = ", ")))
        ## not all codelist_dfs of the same sdmx file have the same variables,
        ## e.g. are about country, time, observation status,
        ## the "indicator" column seems to have all kinds of different names
        ## -> have to print everything and filter 
    }
}

mclapply(files_there, proc_sdmx_file, mc.cores = 6)

## ** working with parsing results
## get files with grepping in /home/johannes/ownCloud/oecd/sdmx_based_tables

sdmx_tables_musem <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'museum'"),
    intern = TRUE)
    
sdmx_tables_cultural_services <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'cultural services'"),
    intern = TRUE)

sdmx_tables_cultural <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv ' cultural'"),
    intern = TRUE)


relevant_sdmx_tables <- Reduce(union, list(sdmx_tables_musem, sdmx_tables_cultural_services, sdmx_tables_cultural))

relevant_sdmx_tables <- c(
    "AEA---CL_AEA_ACTIVITY.csv",
    "BIMTS_CPA---CL_BIMTS_CPA_CPA_VER_2_1.csv",
    "DIOC_OCCUPATION_DET---CL_DIOC_OCCUPATION_DET_DET_OCCUP.csv",
    "FATS_OUT3_SERV---CL_FATS_OUT3_SERV_SERV.csv",
    "FDI_CTRY_IND_SUMM---CL_FDI_CTRY_IND_SUMM_ECO_ACT.csv",
    "FDI_INC_IND---CL_FDI_INC_IND_ECO_ACT.csv",
    "FDI_POS_IND---CL_FDI_POS_IND_ECO_ACT.csv",
    "ERTR_ACC---CL_ERTR_ACC_ACT.csv",
    "FDI_FLOW_IND---CL_FDI_FLOW_IND_ECO_ACT.csv",
    "FATS_IN3_SERV---CL_FATS_IN3_SERV_SERV.csv",
    "FDI_CTRY_ECO_HIST---CL_FDI_CTRY_ECO_HIST_ECO_ACT.csv",
    "NCM_LIVE---CL_NCM_LIVE_INDICATOR.csv",
    "NCM_STAGING---CL_NCM_STAGING_INDICATOR.csv",
    "SNA_TABLE42---CL_SNA_TABLE42_ACTIVITY.csv",
    "SNA_TABLE8A_ARCHIVE---CL_SNA_TABLE8A_ARCHIVE_ACTIVITY.csv",
    "SNA_TABLE31---CL_SNA_TABLE31_ACTIVITY.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_TRANSACT.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_PRODUCT.csv",
    "SNA_TABLE6A_ARCHIVE---CL_SNA_TABLE6A_ARCHIVE_ACTIVITY.csv",
    "SDBS_BDI---CL_SDBS_BDI_SEC.csv",
    "SNA_TABLE7A_SNA93---CL_SNA_TABLE7A_SNA93_ACTIVITY.csv",
    "TEC1_REV4_COPY---CL_TEC1_REV4_COPY_SECTOR.csv",
    "SNA_TABLE8A---CL_SNA_TABLE8A_ACTIVITY.csv",
    "TEC5_REV4---CL_TEC5_REV4_SECTOR.csv",
    "SSIS_BSC---CL_SSIS_BSC_ISIC3.csv",
    "STANI4_2020---CL_STANI4_2020_IND.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_TRANSACT.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_PRODUCT.csv",
    "SNA_TABLE6A---CL_SNA_TABLE6A_ACTIVITY.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_TRANSACT.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_PRODUCT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_TRANSACT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_PRODUCT.csv",
    "SNA_TABLE7A_ARCHIVE---CL_SNA_TABLE7A_ARCHIVE_ACTIVITY.csv",
    "TEC9_REV4---CL_TEC9_REV4_SECTOR.csv",
    "SNA_TABLE9A---CL_SNA_TABLE9A_ACTIVITY.csv",
    "SSIS_BSC_ISIC4---CL_SSIS_BSC_ISIC4_ISIC4.csv",
    "STANI4_2016---CL_STANI4_2016_IND.csv",
    "SNA_TABLE41---CL_SNA_TABLE41_ACTIVITY.csv",
    "SDBS_BDI_ISIC4---CL_SDBS_BDI_ISIC4_SEC.csv",
    "TEC1_REV4---CL_TEC1_REV4_SECTOR.csv",
    "TEC6_REV4---CL_TEC6_REV4_SECTOR.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_ACTIVITY.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_PRODUCT.csv",
    "SNA_TABLE7A---CL_SNA_TABLE7A_ACTIVITY.csv",
    "STANI4---CL_STANI4_IND.csv",
    "STANINDICATORSI4---CL_STANINDICATORSI4_IND.csv")


sdmx_res_tbls <- lapply(relevant_sdmx_tables, function(x) as_tibble(read.csv(paste0(SDMX_TBL_DIR, x))))

sdmx_res_tbl_names <- unlist(lapply(sdmx_res_tbls, names))
table(sdmx_res_tbl_names)

col_names <- c("sdmx_id", "codelist", "id", "label.en", "description.en")

sdmx_res_cbn <- as_tibble(Reduce(function(x,y, ...) rbind(x[,col_names],y[,col_names]),sdmx_res_tbls))

## grepping multiple columns works best with apply, lapply on names(df) doesn't work properly for some reason 
sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl("museum", x)))>0),]


## seems using pipe I can use multiple terms with pipe 
sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl(" cultural|museum|cultural services", x)))>0),]

## sdmx_res_fltrd$id

## sdmx_res_fltrd[28,]
## hello "D90T92" my old friend
## let's see how to query you

## *** figuring out download 

x <- as_tibble(get_dataset("STANI4_2020", filter = list("D90T92")))

## maybe possible to first search which column is actually the indicator?

unlist(lapply(names(df_stan), function(x) grepl("D90T92", df_stan[,x])))
## for STAN, D90T92 is in the first column 

## trying to pass all for countries, just get specific indicator
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("AUTx"),c("PROD"), c("D90T92"))))
## this works for some reason

## passing all doesn't work tho :(
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("all"),c("PROD"), c("D90T92"))))

x <- as_tibble(get_dataset("STANI4_2020", filter="PROD.AUT", pre_formatted = TRUE))

## working url: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/AUT.PROD.D90T92/all
## not working: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/all.PROD.D90T92/all

## https://data.oecd.org/api/sdmx-json-documentation/: CAN USE EMPTY STRING TO SELECT ALL
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c("PROD"), c("D90T92"))))
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c(""), c("D90T92"))))


## AEA actually has the same structure huh 
x2 <- as_tibble(get_dataset("AEA", filter = list(c(""),c(""), c("R90-R92"))))

## FDI_POS_IND: doesn't have the same structure
## adding/removing empty filter strings doesn't seem to work

x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""), c("R91"))))
## actually does when just adding enough LUL 
x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""),c(""), c(""), c(""), c(""),  c(""), c(""), c("R91"))))

## *** functionalized download 


OECD_DATA_DIR <- "/home/johannes/ownCloud/oecd/api_data/"

download_oecd_df <- function(datasetx, filter_list){
    #' actual downloading 
    dfx <- get_dataset(datasetx, filter=filter_list)
    write.csv(dfx, paste0(OECD_DATA_DIR, datasetx))
    print("success")
    done <- TRUE
    }


datasets_already_there <- list.files(OECD_DATA_DIR)


options(timeout = 10)

download_oecd_dataset <- function(datasetx, idx) {
    print(paste0("dataset: ", datasetx, " id: ", idx))
    if (datasetx %!in% datasets_already_there){

        done <- FALSE
        filter_list <- list(c(idx))

        while (done==FALSE) {
            res <- tryCatch({
                print(paste0(filter_list, collapse = "-"))
                download_oecd_df(datasetx, filter_list)
                done <- TRUE
            },
            error=function(e) {
                print(paste0("error: ", length(filter_list)))
                filter_list <- rev(append(rev(filter_list), ""))
            })
            filter_list <- res
            Sys.sleep(2)
            
            if (length(filter_list) == 20) {
                done <- TRUE
                print("quit after too many tries")
            }
                
        }
    }
}      

## write to file to read from separate download_oecd.R
## write.csv(sdmx_res_fltrd, "/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv")


apply(sdmx_res_fltrd, 1, function(x) download_oecd_dataset(x["sdmx_id"], x["id"]))

## *** testing how to call data downloading best 

test_printer <- function(datasetx, idx){
    print(paste0("datasetx: ", datasetx))
    print(paste0("idx: ", idx))
    print("-----------")
    }

apply(sdmx_res_fltrd, 1, test_printer, datasetx=sdmx_id, idx=id)

apply(sdmx_res_fltrd, 1, function(x) test_printer(x$sdmx_id, x$id))
apply(sdmx_res_fltrd, 1, function(x) test_printer(x["sdmx_id"], x["id"]))

test_printer("asdf", "jjj")

apply(sdmx_res_fltrd, 1, function(x) print(names(x)))
apply(sdmx_res_fltrd, 1, function(x) print(x["sdmx_id"]))

## ** processing actual data
## maybe first get those that sufficient country coverage
## hope the label for the country code is the same, but probably isn't 


## find data set column sets 
namesx <- unlist(lapply(datasets_already_there, function(x) names(read.csv(paste0(OECD_DATA_DIR, x)))))
namesx_tbl <- table(namesx)
namesx_tbl[order(namesx_tbl)]

## country col is either: LOCATION (23), COU (12), COUNTRY(3)
## exceptions (so far): "BIMTS_CPA","TEC5_REV4"  ,"TEC6_REV4" ,"TEC9_REV4"

## find exceptions automatically where there's no column named "LOCATION", "COU", "COUNTRY"

namesx_exceptions <- c()

for (i in datasets_already_there){
    dfx <- read.csv(paste0(OECD_DATA_DIR, i))
    if (length(intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))) == 0) {
        print(i)
        namesx_exceptions <- c(namesx_exceptions, i)
    }
}

checked_exceptions <- c("BIMTS_CPA","TEC5_REV4","TEC6_REV4","TEC9_REV4","CRS1","CRS1_GREQ","DV_DCD_GENDER","DV_DCD_PPFD","MULTISYSTEM","RIOMARKERS","SOCX_DET","TSEC1","TSEC1_COPY")
lapply(setdiff(namesx_exceptions, checked_exceptions), print)

## check manually (should check whenever I change the datasets to be included)

## check snippet 
## dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR,


## first batch 
## "BIMTS_CPA"
## "TEC5_REV4"
## "TEC6_REV4"
## "TEC9_REV4"

## second batch 
## "CRS1" ## about agriculture anyways
## "CRS1_GREQ" ## about agriculture anyways
## "DV_DCD_GENDER" ## about agriculture anyways
## "DV_DCD_PPFD" ## about agriculture anyways 
## "MULTISYSTEM" ## about agriculture anyways 
## "RIOMARKERS"  ## about agriculture anyways
## "SOCX_DET" ## only 38 rows, seems all about spain 
## "TSEC1" ## not clear what it is, but only 134 rows
## "TSEC1_COPY" ## also unclear, but also only 134 rows



## names(dfx)

## sloppy coverage evaluations, better in functionalized form below (vague_cvrg)
## namesx_exceptions <- list("BIMTS_CPA"=1,"TEC5_REV4"  ,"TEC6_REV4"  ,"TEC9_REV4")
## for (i in datasets_already_there){
##     dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR, i)))
##     if (length(intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))) == 0) {
##         next
##     } else {
##         country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))
##         print(paste(i, nrow(unique(dfx[,country_col]))))
##     }
##     }

vague_cvrg <- function(namex){
    dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR, namex)))
    country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))
    country_nbr <- nrow(unique(dfx[,country_col]))
    year_min <- min(dfx$Time)
    year_max <- max(dfx$Time)
    time_cvrg <- year_max-year_min
    return(list(
        namex = namex,
        country_nbr = country_nbr,
        year_min = year_min,
        year_max = year_max,
        time_cvrg=time_cvrg))
}

vague_cvrg(datasets_already_there[1])

vague_cvrg_res <- lapply(setdiff(datasets_already_there, namesx_exceptions), vague_cvrg)

vague_res_df <- as_tibble(rbindlist(vague_cvrg_res))
as.data.frame(vague_res_df)


## focus on AEA
## STANI4_2016
## STANI4_2020


## make all the oecd dfs into named list for nicer access
oecd_dfs <- list()
for (i in datasets_already_there){
    dfx <-  as_tibble(read.csv(paste0(OECD_DATA_DIR, i)))
    oecd_dfs[[i]] <- dfx
}




## ** variable extraction 


cpltns_checker <- function(vx, varx) {
    #' assesses completeness of variable in terms of df_anls PM coverage 
    # there's still a bug with cry_cvrg_geq3, which can be higher than 217 sometimes 

    dfb <- df_anls[,c("countrycode", "year", "nbr_opened")]
    dfc <- as_tibble(merge(dfb, vx, by = c("year", "countrycode"), all.x = TRUE))

    cry_cvrg <- aggregate(year ~ countrycode, na.omit(dfc), length)
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$countrycode
    cry_pm_crvg_actual <- aggregate(nbr_opened ~ countrycode, na.omit(dfc), sum)
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ countrycode, dfc, sum)
    names(cry_pm_crvg_ideal) <- c("countrycode", "nbr_opened_ideal")

    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ## most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
    ##                                     function(x) (filter(cry_pm_cvrg_cprn, diff == x)$countrycode)))

    most_affected_crys <- cry_pm_cvrg_cprn$countrycode[order(cry_pm_cvrg_cprn$diff)[1:4]]
    

    PMs_covered_raw <- sum(na.omit(dfc[which(dfc$countrycode %in% crys_geq3),])$nbr_opened)

    cry_cvrg_geq3 <- sum(filter(dfc, countrycode %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- filter(aggregate(nbr_opened ~ countrycode, dfc, sum), countrycode %in% crys_geq3) %>%
        filter(nbr_opened >= 1) %>%
        nrow()

    return(list(
        varx=varx,
        nobs=nrow(vx),
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
 
}

## generate filter expression with eval(parse())
generate_sel_str <- function(combox){
    strs <- c()

    for (k in 1:ncol(combox)) {

        col_name <- names(combox)[k]
        col_vlu <- as.data.frame(combox)[1,k]
        
        print(paste(col_vlu, typeof(col_vlu)))
        if (is.na(col_vlu)) {
            strx <- paste0('is.na(dfx$', col_name, ")")

        } else if (typeof(col_vlu) == "character"){
            strx <- paste0('dfx["', col_name, '"]=="', col_vlu,'"')
        } else  {
            strx <- paste0('dfx["', col_name, '"]==', col_vlu)
        }
        strs <- c(strs, strx)
    }

    strx_cbn <- paste(strs,collapse =  " & ")
    return(strx_cbn)
}


filter(vague_res_df, country_nbr > 25 & time_cvrg > 25)

lapply(filter(vague_res_df, country_nbr > 25 & time_cvrg > 25)$namex, function(x) names(oecd_dfs[[x]]))


idx <- "AEA"
idx <- "TISP_EBOPS2010"
idx <- "STANI4_2020"


dfx <- oecd_dfs[[idx]]

namesx <- names(dfx)
country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))

dfx$ObsValue <- dfx$ObsValue * (10^dfx$POWERCODE)

combo_cols <- setdiff(namesx, c(country_col, "ObsValue", "X", "Time", "POWERCODE"))

combos <- unique(dfx[,combo_cols])

filter(dfx, is.na(OBS_STATUS) & )


combos <- unique(dfx[,c("MEASURE", "POLLUTANT")])


dfx[which(eval(parse(text=strx_cbn))),]

## which(dfx["IND"] == "D90T92" & is.na(dfx$OBS_STATUS))
    
res <- list()     

for (i in 1:nrow(combos)) {
    print(i)
    combox <- combos[i,]
    print(combox)

    strx_cbn <- generate_sel_str(combox)
    vx <- dfx[which(eval(parse(text=strx_cbn))),]


    ## vx <- dfx[which(dfx["MEASURE"]== as.data.frame(combox)[1,"MEASURE"] &
    ##                 dfx["POLLUTANT"]== as.data.frame(combox)[1,"POLLUTANT"]),]


    ## some flexible renaming, kinda like SQL
    vx2 <- vx %>%
        rename(countrycode=country_col, year=Time, value =ObsValue) %>%
        select(countrycode, year, value)
    
    varx <- paste(combox, collapse = "-")
    
    resx <- cpltns_checker(vx2, varx)
    res[[i]] <- resx
}

res_df <- as_tibble(rbindlist(res))

hist(res_df$cry_cvrg_geq3)

hist(res_df$nobs)

filter(res_df, PMs_covered_raw > 130 & cry_cvrg_geq3 > 200)$varx

i <- which(res_df$varx=="D90T92-NA-NA-P1Y-PER-SELF")
i <- which(res_df$cry_cvrg_geq3 > 250)



## ** debugging failed files, doesn't seem that many -> fine to ignore
proc_sdmx_file("REVPER.xml")
proc_sdmx_file("EO27_VINTAGE.xml")

lapply(list("EO27_VINTAGE.xml", "REVPER.xml"), proc_sdmx_file)



## each item is again a slot-carrier (?) with bunch of slots?

sdmx_file <- "EO27_VINTAGE.xml"
##  [1] "id"                  "agencyID"            "version"            
##  [4] "uri"                 "urn"                 "isExternalReference"
##  [7] "isFinal"             "validFrom"           "validTo"            
## [10] "Name"                "Description"         "Code"               

lapply(list_of_codelists, function(x) slotNames(x))
## need to compare with some item that I know works

sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
## print(slotNames(dsd)) overall slotnames are the same
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))

codelistx <- "CL_EO27_VINTAGE_LOCATION"

list_of_codelists <- slot(slot(dsd, "codelists"),"codelists")
location_codelist <- list_of_codelists[[1]]
slot(location_codelist)
## huh actually has no "Code" slot that could provide the rows
slotNames(location_codelist)

## maybe should see if all the sdmx files that don't work have the same structure
## first see where it works -> write exception





sdmx_file <- "REVPER.xml"
##  [1] "id"                  "agencyID"            "version"            
##  [4] "uri"                 "urn"                 "isExternalReference"
##  [7] "isFinal"             "validFrom"           "validTo"            
## [10] "Name"                "Description"         "Code"               
sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
## print(slotNames(dsd)) overall slotnames are the same
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))


## -> each of the names within the slots seem to be the same
list_of_codelists <- slot(slot(dsd, "codelists"),"codelists")
codelistx <- "CL_REVPER_TIME_FORMAT"
## index 8
## each item again codelist
time_format_codelist <- list_of_codelists[[8]]
## seems like slot "Code" is a list of things that gets converted to rows
slot(time_format_codelist, "Code")

row1 <- slot(time_format_codelist, "Code")[[1]]
## then uses slots id, label, description
## label and description are seem to be named lists -> should be able to loop over them


x <- slot(cls, "codelists")[[1]]
lapply(slotNames(x), function(i) {slot(x, i)})



## proc_sdmx_file(sdmx_file)




## need to compare one codelist that works properly with one that doesn't
## might need manual parsing, have to make sure that it produces the same result in a codelist that works than what is produced by overloaded as.data.frame



## ** concept stuff, not needed
slotNames(sdmx_test)
## [1] "organisationSchemes" "concepts"            "codelists"          
## [4] "datastructures"      "xmlObj"              "schema"             
## [7] "header"              "footer"             

slotNames(slot(sdmx_test, "concepts"))
## yo dawg i heard you like concepts in your concepts
slot(slot(sdmx_test, "concepts"), "concepts")
len(slot(slot(sdmx_test, "concepts"), "concepts"))


country_slot <- slot(slot(sdmx_test, "concepts"), "concepts")[[1]]
slotNames(country_slot)
slot(country_slot, "Name")
slot(country_slot, "Name")$en

year_slot <- slot(slot(sdmx_test, "concepts"), "concepts")[[2]]
slotNames(year_slot)
concept_slot <- slot(sdmx_test, "concepts")
typeof(concept_slot) ## -> S4
len(concept_slot) ## -> 1
slotNames(concept_slot)
[1] "concepts"       "conceptSchemes" "xmlObj"         "schema"        
[5] "header"         "footer"

concept_slots <- slot(concept_slot, "concepts")
lapply(concept_slots, function(x) {slotNames(x)})
unlist(lapply(concept_slots, function(x) {slot(x, "Name")$en}))
##  [1] "Country"            "Variable"           "Industry"          
##  [4] "Time"               "Observation Value"  "Time Format"       
##  [7] "Observation Status" "Unit"               "Unit multiplier"   
## [10] "Reference period"  
