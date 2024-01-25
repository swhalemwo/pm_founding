## * wb api

download_WB_data <- function(indx){
    #' downloads data with the WB API, can take single WB variable codes or vectors thereof
    print(paste0("now downloading: ", indx))
    indx_data <- wb_data(indicator = indx, start_date = STARTING_YEAR, end_date = ENDING_YEAR)
    
    date_pos <- which(names(indx_data)=="date")
    names(indx_data)[date_pos] <- "year"

    return(indx_data[,c("iso3c", "country", "year", indx)])
}

## dfx <- download_WB_data(c("NY.GDP.PCAP.CD"))

save_WB_data <- function(df) {
    #' saves WB data
    write.table(df, file = paste0(PROC_DATA_DIR, "WB_data_proc.csv"))
}

## save_WB_data(dfx)


read_WB_data <- function() {
    df_wb <- as_tibble(read.table(paste0(PROC_DATA_DIR, "WB_data_proc.csv")))
}


gd_wb_gdpgrowth <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' explore WB indicators for GDP growth
    

    ## see what's there in WB 
    l_wbcache <- wb_cache()

    l_wbcache$indicators %>% adt %>% .[grepl("growth", indicator, ignore.case = T)] %>%
        .[, .(indicator, indicator_id)] %>% print(n=300)

    dt_gdp_growth_indicators <- l_wbcache$indicators %>% adt %>%
        .[grepl("growth", indicator, ignore.case = T) & grepl("GDP", indicator_id)]
    ## get 6 potential indicators

    ## evaluate the 6 ->  gets down to 2
    dt_wb_gdpgrowth <- wb_data(dt_gdp_growth_indicators[, indicator_id],
                               start_date = STARTING_YEAR, end_date = ENDING_YEAR) %>% adt

    dt_wb_gdpgrowth %>% summary
    gdpgrowth_penl_indicators <- dt_wb_gdpgrowth %>% .[, map(.SD, ~sum(is.na(.x))),
                                  .SDcols = dt_gdp_growth_indicators[, indicator_id]] %>%
        keep(~.x < 2000) %>% names

    
    ## look into the two
    dt_gdp_growth_indicators[indicator_id %in% gdpgrowth_penl_indicators,
                             .(indicator_id, indicator, indicator_desc)] %>% adf

    ## 1 NY.GDP.MKTP.KD.ZG            GDP growth (annual %)
    ## 2 NY.GDP.PCAP.KD.ZG GDP per capita growth (annual %)

    ## compare them 
    dt_wb_gdpgrowth[, ..gdpgrowth_penl_indicators] %>% cor(use = "complete.obs")


    dt_wb_gdpgrowth[, c("iso3c", "date", gdpgrowth_penl_indicators), with = F] %>%
        ## .[iso3c %in% c("DEU", "USA", "ISR", "CHN")] %>% 
        melt(id.vars = c("iso3c", "date")) %>% 
        ggplot(aes(x=date, y=value, color = variable)) +
        facet_wrap(~iso3c) + geom_line()
    

    ## dt_gdp_growth_indicators[
    ## only viable indicators are  NY.GDP.PCAP.KD.ZG and NY.GDP.MKTP.KD.ZG, all others have no coverage
    ## for now use NY.GDP.PCAP.KD.ZG, which fits more into PCAP framework that I'm using
        

    ## calculate myself
    dt_wb_mnl <- df_wb %>% adt %>% .[, gdpgrwoth_mnl := fgrowth(NY.GDP.PCAP.CD), iso3c]

    ## compare 

    dt_gpdgrwth_cprn <- join(dt_wb_mnl[, .(iso3c, year, gdpgrwoth_mnl)], 
         dt_wb_gdpgrowth[, .(iso3c, year = date, NY.GDP.PCAP.KD.ZG)], on = .c(iso3c, year), how = "full")

    cor(dt_gpdgrwth_cprn[, .(gdpgrwoth_mnl, NY.GDP.PCAP.KD.ZG)], use = "complete.obs") ## 95 correlation..
    
    dt_gpdgrwth_cprn %>% melt(id.vars = .c(iso3c, year)) %>% 
        .[iso3c %in% sample(funique(iso3c), 6)] %>% 
        ggplot(aes(x=year, y=value, color = variable)) +
        facet_wrap(~iso3c) + geom_line()
    


}

## gd_wb_gdpgrowth()

## df_wb <- read_WB_data()

cpr_gdp_sources <- function(gdp_cvrtd, wid_gdp_cvrtd) {
    #' compare the GDP sources: WID and WB

    ## calculate metrics to measure differences
    gdp_cpr <- inner_join(
        select(gdp_cvrtd, iso3c, year, USD_constant_wb = USD_constant),
        select(wid_gdp_cvrtd, iso3c, year, USD_constant_wid = USD_constant)) %>%
        mutate(diff_addgn = USD_constant_wid - USD_constant_wb, # absolute diff
               diff_mult1 = USD_constant_wid/USD_constant_wb, # relative diff one direction 
               diff_mult2 = USD_constant_wid/USD_constant_wb) # relative diff other direction
    ## differences don't seem so big        

    ## aggregate the diffs by country 
    diff_crys_prep <- gdp_cpr %>%
        group_by(iso3c) %>%
        summarize(sum_diff_addgns = sum(diff_addgn)/len(diff_addgn),
                  sum_diff_mult1 = sum(diff_mult1)/len(diff_mult1),
                  sum_diff_mult2 = sum(diff_mult2)/len(diff_mult2))

    ## select all the countries that are in top 30 on any of metrics (atm 34)
    diff_crys_vec <- lapply(c("sum_diff_addgns", "sum_diff_mult1", "sum_diff_mult2"), \(x)
                            diff_crys_prep %>% arrange(-get(x)) %>% head(n=30) %>% pull(iso3c)) %>%
        unlist() %>% unique()
    
    ## plot gdp lines, color by source
    bind_rows(
        select(gdp_cvrtd, iso3c, year, USD_constant) %>% mutate(source = "WB"),
        select(wid_gdp_cvrtd, iso3c, year, USD_constant) %>% mutate(source = "WID")) %>%
        filter(iso3c %in% diff_crys_vec) %>% 
        ggplot(aes(x=year, y=USD_constant, group = source, color = source)) +
        geom_line() +
        facet_wrap(~iso3c, scales = "free")
    ## invisible(NULL)

    
}
    
get_wid_gdp <- function(wid_vx) {
    #' get the gdp per capita values from WID 

    ## use agdpro999i: constant LCU per capita
    wid_gdp_cmd <-sprintf(
        "select iso3c, year, variable, value from %s where variable = 'agdpro999i' AND year >= %s",
        wid_vx, STARTING_YEAR)

    wid_gdp_tb <- atb(dbGetQuery(con, wid_gdp_cmd))
    names(wid_gdp_tb) <- c("iso3c", "year", "gdp_var", "gdp_vlu")
    
    wid_gdp_prep <- inner_join(wid_gdp_tb,
               select(cur_df, iso3c, year, xlcusx999i_2021))
    
    ## convert: WID is in constant LCU -> can just convert with xlcusx999i_2021 into constant USD
    wid_gdp_cvrtd <- wid_gdp_prep %>%
        mutate(USD_constant = gdp_vlu/xlcusx999i_2021) %>%
        mutate(region = countrycode(iso3c, "iso3c", "un.regionsub.name"))

    return(wid_gdp_cvrtd)
}

CPR_GDP_SOURCES <- F

cvrt_ny.gdp.pcap.cd <- function(df_wb_new) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' wrapper for conversion of GDP into constant USD

    ## join with cur_df to convert to 2021-constant USD
    gdp_prep <- inner_join(select(df_wb_new, iso3c, year, NY.GDP.MKTP.CN, SP.POP.TOTL),
                           select(cur_df, iso3c, year, inyixx999i, xlcusx999i_2021)) 

    ## use World bank data 
    gdp_cvrtd <- gdp_prep %>%
        mutate(LCU_current_pcap = NY.GDP.MKTP.CN/SP.POP.TOTL) %>% # get gdp pcap current lcu
        mutate(LCU_constant_pcap = LCU_current_pcap/inyixx999i) %>% # convert everything to constant LCU
        mutate(USD_constant = LCU_constant_pcap/xlcusx999i_2021) %>% # get constant USD
        mutate(region = countrycode(iso3c, "iso3c", "un.region.name"))

    ## filter(gdp_cvrtd, iso3c == "DEU")

    ## viz_lines(gdp_cvrtd, y="USD_constant", facets = "region")
    ## hmm Venezuela is bs, rest could be plausible? idk

    ## use WID data
    wid_gdp_cvrtd <- get_wid_gdp(WID_VX)
    
    ## convert gdp of WB and WID if necessary; atm doesn't seem like big difference
    if (CPR_GDP_SOURCES) {
        cpr_gdp_sources(gdp_cvrtd, wid_gdp_cvrtd)
    }

    ## add WID GDP to the data
    df_wb_new2 <- inner_join(df_wb_new, select(wid_gdp_cvrtd, iso3c, year, USD_constant))

    ## yeet WB GDP, replace by WID GDP
    df_wb_new3 <- df_wb_new2 %>%
        select(-NY.GDP.PCAP.CD) %>%
        select(everything(), NY.GDP.PCAP.CD = USD_constant) %>%
        filter(iso3c != "VEN") # yeet vuvuzela, probably for being garbage

    ## df_wb_new3 %>% adt %>% .[iso3c == "HTI"] %>% viz_lines(y="NY.GDP.PCAP.CD", duration = 1)

    return(df_wb_new3)
}

        

    





get_WB_data <- function(indx, refresh_all=FALSE) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' get WB variables, download all new if asked to do so, else read from file

    ## download everything if no file is there
    if ("WB_data_proc.csv" %in% list.files(PROC_DATA_DIR)) {
        df_wb_local <- read_WB_data()
        wb_vars_there <- names(df_wb_local)[4:len(names(df_wb_local))]
        print(paste0("wb_vars_there: ", paste0(wb_vars_there, collapse = ", ")))
        nothing_there <- FALSE

    } else {
        nothing_there <- TRUE
        refresh_all <- TRUE
        wb_vars_there <- c()
    }
        

    if (refresh_all) {
        print(paste0("download all indicators anew: ", indx))
        ## indx <- c(indx,  "NY.GDP.PCAP.KD.ZG")
        df_wb_api <- download_WB_data(indx)

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% indx)]

        if (nothing_there) {
            ## assign result of API call to have something to be able to not change merge syntax below
            df_wb_local <- df_wb_api[,c("iso3c", "country", "year")]
        }
        
    } else {
        wb_vars_to_download <- indx[which(indx %!in% wb_vars_there)]
        print(paste0("number of vars to download: ", length(wb_vars_to_download)))
        print(paste0("download selected indicator(s) anew: ", wb_vars_to_download))
        
        if (length(wb_vars_to_download) > 0) {
            df_wb_api <- download_WB_data(wb_vars_to_download)
        } else {
            df_wb_api <- df_wb_local[,c("iso3c", "country", "year")]
        }

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% wb_vars_to_download)]
        print(paste0("unchanged_vars: ", paste0(unchanged_vars, collapse = ", ")))
        ## need to this out: how to get the variables which shouldn't be changed; think i got it now
    }

    df_wb_new <- merge(df_wb_local[,c("iso3c", "country", "year", unchanged_vars)], df_wb_api) %>% adt %>% 
        .[!is.na(countrycode(iso3c, "iso3c", "country.name"))] %>%
        atb() # filter out countries that don't conform to iso3c standard (for now CHI)

    

    ## inspect coverage 
    ## colSums(is.na(select(df_wb_new, NY.GDP.PCAP.CD, NY.GDP.MKTP.CN)))

    ## convert to constant USD, using WID data
    ## actually also needs NY.GDP.MKTP.CN to be present for comparison.. could be yeeted tho
    if ("NY.GDP.PCAP.CD" %in% indx) {
        df_wb_new <- cvrt_ny.gdp.pcap.cd(df_wb_new)
    }

    save_WB_data(df_wb_new)

    return(df_wb_new[,c("iso3c", "country", "year", indx)])
    
}



## indx <- c("NY.GDP.PCAP.CD")
## x <- get_WB_data(indx)


## complete data from 1995 onwards
## can see what kind of different ways I can use to remove NAs:
## remove stuff before 1995
## remove countries with NAs
## maybe I can also have different starting dates per country? idk, will see when i check the method
