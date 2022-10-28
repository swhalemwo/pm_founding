## * cultural spending stuff


## ** ILO


proc_ilo_df <- function(ilo_df_id, dfx) {
    #' process the ILO dfs (different resolution)

    ## dfx <- ilo_dfs[[ilo_df_id]]

    dfx$iso3c <- countrycode(dfx$ref_area.label, "country.name", "iso3c")
    dfx$year <- dfx$time

    dfx$Item <- ilo_df_id
    dfx$Value <- dfx$obs_value

    return(select(dfx, iso3c, year, Item, Value))


}

gen_ilo_df <- function() {
    ilo_df_files <- list.files(paste0(PROJECT_DIR, "data/ILO"))
    names(ilo_df_files) <- lapply(ilo_df_files, function(x) substring(x, 1, nchar(x)-4))

    ilo_dfs <- lapply(ilo_df_files, function(x) as_tibble(read.csv(paste0(PROJECT_DIR, "data/ILO/", x))))


    ilo_df <- Reduce(function(x,y,...) rbind(x,y), lapply(names(ilo_dfs), \(x) proc_ilo_df(x, dfx = ilo_dfs[[x]])))
    return(ilo_df)
}





## ** UN

## *** WID currency converter

gen_cur_df <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate currency converter df for all kinds of purposes

    currency_cmd <- paste0("select iso3c, year, variable, value from wid_v2 where (variable='xlcusx999i' or variable='inyixx999i' or variable='xlcusp999i') and year>=", STARTING_YEAR)

    cur_df_raw <- as_tibble(dbGetQuery(con, currency_cmd))
    ## add fx rate for cuba for 2021, 1 according to fxtop
    cur_df_raw[nrow(cur_df_raw)+1,] <- list("CUB", 2021, "xlcusx999i", 1)

    oecd_cur_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/OECD/fx_rates.csv"))) %>% 
        select(iso3c=LOCATION, year = TIME, value = Value) %>%
        mutate(variable = "xlcusx999i")

    cur_df_cbn <- as_tibble(rbind(cur_df_raw, oecd_cur_df)) %>%
        group_by(iso3c, year, variable) %>%
        summarise(value=mean(na.omit(value))) %>%
        ungroup() # ungroup needed to not automatically include year when creating 2021 series 


    cur_df <- pivot_wider(cur_df_cbn, names_from = variable, values_from = value)

    ## construct fx and ppp rates for 2021 for constant price series
    cur_df <- cur_df %>% merge(
                             filter(cur_df, year == 2021) %>%
                             mutate(xlcusx999i_2021 = xlcusx999i,
                                    xlcusp999i_2021 = xlcusp999i) %>%
                             select(iso3c, xlcusx999i_2021, xlcusp999i_2021)) %>%
        as_tibble()

    ## add meme countries
    ## Faroe Islands (uses same as DNK)

    fro_cpi <- read_excel(paste0(PROJECT_DIR, "data/fro_cpi/IP02050_PRIS_ARSMID_20220602-091544.xlsx"), skip = 2)[seq(82),]
    names(fro_cpi) <- c("year", "value")
    
    fro_cpi$inyixx999i_fro <- fro_cpi$value/filter(fro_cpi, year==2021)$value
    fro_cpi$iso3c <- "FRO"

    cur_df <- merge(cur_df, fro_cpi, all.x = T) %>% atb() %>% 
        mutate(inyixx999i = ifelse(iso3c == "FRO", inyixx999i_fro, inyixx999i)) %>%
        select(all_of(names(cur_df)))


    return(cur_df)
}





## *** df2

construct_un_df2 <- function() {
    #' read in un_df2
    un_df2 <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/", "UNdata_output_gross_value_added_fixed_assests_industry_cur_prices.csv")))

    un_df2$iso3c <- countrycode(un_df2$Country.or.Area, "country.name", "iso3c")
    un_df2$year <- un_df2$Year
    un_df2$region <- countrycode(un_df2$iso3c, "iso3c", "un.region.name")

    ## pivoting UN df to wide to see how good coverage is if all variables are combined

    ## un_df2_cur %>%
    ## ungroup %>%
    ## filter(nbr_curs > 1) %>%
    ## select(iso3c, Currency)%>%
    ## unique()

    
    ## fuck some countries have multiple currencies
    ## at least LTU and LVA have euros, and then only BGR left -> can take care of that manually

    ## actually kinda helped that I did it with un_df3 before:
    ## now can just yeet lats, litas, and lev (re-denom. 1:1000)

    curs_to_yeet <- c("lev (re-denom. 1:1000)", "lats", "litas")
    curs_to_rename <- c("euro" = "Euro")

    un_df2_fltrd <- filter(un_df2, Currency %!in% curs_to_yeet, year >= STARTING_YEAR)
    un_df2_fltrd$Currency <- recode(un_df2_fltrd$Currency, !!!curs_to_rename)

    return(select(un_df2_fltrd, iso3c, year, Series, Item, Value, Currency))
}

## un_df2_fltrd %>%
##     group_by(iso3c, year) %>%
##     mutate(nbr_curs = len(unique(Currency))) %>%
##     filter(nbr_curs > 1) %>%
##     ungroup() %>%
##     select(iso3c, Currency) %>%
##     unique()
## duplicate currencies gone 



## *** un_df3

check_mult_cur <- function(df) {
    #' check if there countries where there are multiple currencies
    #' requires iso3c, year and Currency

    cur_df <- df %>%
        group_by(iso3c, year) %>%
        mutate(nbr_curs = n_distinct(Currency_tws), nbr_series = n_distinct(Currency_tws))

    cur_anls_df <- cur_df %>%
        ungroup() %>%
        filter(nbr_curs > 1) %>%
        select(iso3c, Currency_tws) %>%
        unique() %>%
        as.data.frame()

    return(cur_anls_df)

}


## check_mult_cur(un_df3)
## check_mult_cur(un_df3_fltrd)


    ## merging item and currency data to see if/how currency changes are reflected in exchange rates
    ## and whether that impacts coverage

    ## rbind or merge + pivot_longer? rbind lol

check_cry_cur <- function(cry) {
    #' plot currencies and series for a country to determine for each country with multiple currencies whether to drop or rename currencies

    filter(un_df3, iso3c == cry) %>%
        select(iso3c, year, Value, Series, Currency) %>%
        rbind(., select(., iso3c, year, Value, Series = Currency) %>%
                 ## add also a facet with all the currencies combined
                 ## joins stuff of different series together tho 
                 mutate(Currency = "curs")) %>%
        rbind(filter(cur_df, iso3c == cry) %>%
              mutate(Currency = "wid", Series = "xlcusx999i") %>%
              select(iso3c, year, Value = xlcusx999i,  Currency, Series)) %>%
        ggplot(aes(x=year, y=Value, color = factor(Series))) +
        facet_wrap(~Currency, scales = "free_y", nrow=3) +
        geom_line(size=1.5)

}

## un_df3 <- construct_gvt_consumption_expenditure()
## check_cry_cur("MEX")


construct_gvt_consumption_expenditure <- function() {
    #' final construction function of un_df3

    un_df3 <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/UNdata_Gvt_consumption_expenditure.csv")))

    un_df3$iso3c <- countrycode(un_df3$Country.or.Area, "country.name", "iso3c")
    un_df3$year <- un_df3$Year
    un_df3$region <- countrycode(un_df3$iso3c, "iso3c", "un.region.name")

    ## un_df3 %>%
    ##     group_by(iso3c, year) %>%
    ##     summarize(some_val=1) %>%
    ##     cpltns_checker("some_val")

    ## viz_lines(un_df3, x="year", y="Value", grp="iso3c", time_level = "ra", duration = 4, max_lines = 8, facets = "region")


    un_df3$Currency_tws <- trimws(un_df3$Currency) 
    ## here: investigate which currencies have to be renamed or dropped (countries from check_mult_cur), investigation with check_cry_cur


    curs_to_yeet <- c(
        "1999 ATS euro / euro","Austrian schilling",
        "Azerbaijan manat",
        "1999 BEF euro / euro", "Belgian franc",
        "lev (re-denom. 1:1000)",
        "Cyprus pound",
        "Estonian kroon", "Estonian Kroon",
        "1999 FIM euro / euro", "Finish markka",
        "1999 FRF euro / euro", "French franc",
        "1999 DEM euro / euro", "deutsche mark",
        "2001 GRD euro / euro",
        "1999 IEP euro / euro",
        "1999 ITL euro / euro", "Italian lira",
        "lats",
        "Litas", "litas",
        "1999 LUF Euro / Euro", "1999 LUF euro / euro", "Luxembourg franc",
        "Maltese liri",
        "1999 NLG euro / euro", "1999 NLG Euro / Euro",
        "1999 PTE euro / euro", "Portuguese escudo",
        "Romanian Leu",
        "Russian ruble",
        "Slovak koruna", "Slovak Koruna",
        "Slovenian tolar", "tolar",
        "1999 ESP euro / euro", "peseta",
        "bolivar")

    ## filter(wid_cur_df, iso3c == "VEN") %>%
    ##     ggplot(aes(x=year, y=log10(value))) +
    ##     facet_wrap(~variable) +
    ##     geom_line()

    
    curs_to_rename <- list(
        list(from = "pataca", to = "Pataca"),
        list(from = "Danish Krone (DDK.)", to = "Danish Krone"),
        list(from = "forint", to = "Forint"),
        list(from = "euro", to = "Euro"),
        list(from = "pakistan rupee", to = "Pakistan Rupee"),
        list(from = "Pakistan rupee", to = "Pakistan Rupee"),
        list(from = "zloty", to = "Zloty"),
        list(from = "Russian ruble (re-denom. 1:1000)", to = "Russian Ruble"),
        list(from = "Swedish krona", to = "Swedish Krona"),
        list(from = "Tanzania shilling", to = "Tanzanian Shilling")
        
    )

    countries_to_yeet <- c("VEN")
    ## VEN data is whack

    ## https://stackoverflow.com/questions/19265172/converting-two-columns-of-a-data-frame-to-a-named-vector
    curs_to_rename_cfg <- rbindlist(curs_to_rename) %>%
        pull(to, from)

    ## https://github.com/tidyverse/dplyr/issues/3899

    

    un_df3_fltrd <- filter(un_df3, Currency_tws %!in% curs_to_yeet, iso3c %!in% countries_to_yeet, year >= STARTING_YEAR)
    un_df3_fltrd$Currency_tws <- recode(un_df3_fltrd$Currency_tws, !!!curs_to_rename_cfg)

    ## manual adjustments
    un_df3_fltrd <- un_df3_fltrd %>% 
        mutate(Value = ifelse(iso3c == "BLR" & Series == 1000, Value/10000, Value),
               Value = ifelse(iso3c == "BLR" & Series == 1100, Value/1,Value))
    ## %>% 
    ##     filter(iso3c == "BLR") %>% select(iso3c, year, Series, Value)

        
    ## filter(un_df3_fltrd, iso3c == "BLR") %>% select(iso3c, year, Series, Value)
        
    


    ## un_df3_fltrd %>%
    ##     ## filter(Series == 1000) %>%
    ##     group_by(iso3c, year) %>%
    ##     summarize(some_val = 1) %>%
    ##     cpltns_checker(varx = "some_val")
    ## ## using all Series or not substantially affects sample size..

    return(select(un_df3_fltrd, iso3c, year, Item, Series, Value, Currency = Currency_tws))
}
    
    

## *** series combining

combine_un_series <- function() {
    #' construct the UN series by combining un_df2 (output_gross_value_added_fixed_assests_industry_cur_prices) and un_df3 (govt consumption expenditure)
    #' also construct cautious (only series 1000) and risky/SMOrc (mean of all series) for each indicator

    

    un_df2 <- construct_un_df2()
    un_df3 <- construct_gvt_consumption_expenditure()

    un_df <- as_tibble(rbind(un_df2, un_df3))

    ## un_df$region <- countrycode(un_df$iso3c, "iso3c", "un.region.name")

    
    ## **** smorc: just calc mean
    un_df_smorc <- un_df %>%
        mutate(Item = paste0("UN_SMOrc ", Item)) %>%
        group_by(iso3c, year, Item) %>%
        summarize(Value = mean(Value)) %>%
        select(iso3c, year, Item, Value)



    ## **** conservative
    un_df_caution <- un_df %>%
        filter(Series == 1000) %>%
        mutate(Item = paste0("UN_caution ", Item)) %>%
        select(iso3c, year, Item,  Value)


    df_un_all <- rbind(un_df_smorc, un_df_caution)
    return(df_un_all)
}



## ** IMF


## *** just bulk download

get_imf_data_old <- function() {
    #' generate the IMF data: percentage of GDP spent on all kinds of culture/religion/recreation related activities

    imf_file <- "GFSCOFOG_04-16-2022 19-03-49-90_timeSeries.csv" ## all 
    imf_file <- "GFSCOFOG_04-21-2022 10-14-59-63_timeSeries.csv" ## just culture + recreation

    imf_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/IMF/", imf_file)))

    ## matching all the columns that don't have a year in them 
    imf_non_year_cols <- names(imf_df)[!unlist(lapply(names(imf_df), function(x) scramblematch("^X\\d{4}$", x)))]

    imf_df_melt <- as_tibble(reshape2::melt(imf_df, id=imf_non_year_cols))
    imf_df_melt <- rename(imf_df_melt, year=variable)
    imf_df_melt$year <- as.numeric(substring(imf_df_melt$year, 2, 5))

    ## names(imf_df_melt)

    ## table(imf_df_melt$year)
    ## table(imf_df_melt$COFOG.Function.Name)
    ## table(imf_df_melt$Sector.Name)
    ## table(imf_df_melt$Unit.Name)
    ## table(imf_df_melt$Attribute)

    ## unique(imf_df_melt[,c("COFOG.Function.Name", "COFOG.Function.Code")])

    ## think it makes sense to be as broad as possible?
    ## include as many items as possible, and filter later automatically?
    ## but then there's no reason not to include other functions?
    ## maybe there's a difference between
    ## - proxy: assuming that a variable measures the same concept?
    ## - imputation: seeing some empirical relationship without theoretical justification?
    ## idk if that distinction can really be made
    
    imf_culture <- filter(imf_df_melt,
                          ## COFOG.Function.Code == "GF08", 
                          Sector.Name == "General government",
                          Unit.Name == "Percent of GDP",
                          ## Country.Name == "Germany",
                          year >= 1985,
                          Attribute == "Value",
                          value != ""
                          ) %>%
        select(iso3c=Country.Name, year, COFOG.Function.Code, value)

    imf_culture$value <- as.numeric(imf_culture$value)
    imf_culture$iso3c <- countrycode(imf_culture$iso3c, "country.name", "wb")

    ## imf_culture_wide <- pivot_wider(imf_culture, names_from = COFOG.Function.Code, values_from = value)

    return(
        select(imf_culture, iso3c, year, Item = COFOG.Function.Code, Value = value) %>%
        mutate(Item = paste0("IMF_", Item))
        )


}


get_imf_data <- function() {
    #' generate the IMF data: percentage of GDP spent on all kinds of culture/religion/recreation related activities

    imf_file <- "GFSCOFOG_04-16-2022 19-03-49-90_timeSeries.csv" ## all 
    imf_file <- "GFSCOFOG_04-21-2022 10-14-59-63_timeSeries.csv" ## just culture + recreation

    imf_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/IMF/", imf_file)))

    ## matching all the columns that don't have a year in them 
    imf_non_year_cols <- names(imf_df)[!unlist(lapply(names(imf_df), function(x) scramblematch("^X\\d{4}$", x)))]

    imf_df_melt <- as_tibble(reshape2::melt(imf_df, id=imf_non_year_cols))
    imf_df_melt <- rename(imf_df_melt, year=variable)
    imf_df_melt$year <- as.numeric(substring(imf_df_melt$year, 2, 5))

    ## names(imf_df_melt)

    ## table(imf_df_melt$year)
    ## table(imf_df_melt$COFOG.Function.Name)
    ## table(imf_df_melt$Sector.Name)
    ## table(imf_df_melt$Unit.Name)
    ## table(imf_df_melt$Attribute)

    ## unique(imf_df_melt[,c("COFOG.Function.Name", "COFOG.Function.Code")])

    ## think it makes sense to be as broad as possible?
    ## include as many items as possible, and filter later automatically?
    ## but then there's no reason not to include other functions?
    ## maybe there's a difference between
    ## - proxy: assuming that a variable measures the same concept?
    ## - imputation: seeing some empirical relationship without theoretical justification?
    ## idk if that distinction can really be made
    
    imf_culture <-  filter(imf_df_melt,
           COFOG.Function.Code == "GF08", 
           Sector.Name == "General government",
           ## Unit.Name == "Percent of GDP",
           Unit.Name == "Domestic currency" | Unit.Name == "Percent of GDP", # later pivot_wider both 
           ## Country.Name == "Germany",
           year >= STARTING_YEAR,
           Attribute == "Value",
           value != "") %>% 
    mutate(iso3c = countrycode(Country.Name, "country.name", "wb"),
           value = as.numeric(value)) %>%
    select(iso3c, year, Unit.Name, value) %>%
    pivot_wider(names_from = Unit.Name, values_from = value)

    imf_cult_cbn <- merge(imf_culture, cur_df, all.x = T) %>% atb() %>%
        mutate(constant_usd = (`Domestic currency`/inyixx999i)/xlcusx999i_2021, # Blanchet_2017_conversions
               source = "imf",
               format = "tlycg") %>%
        select(iso3c, year, constant_usd, pct_value = `Percent of GDP`, source, format) %>%
        pivot_longer(cols = c(constant_usd, pct_value), names_to = "measure")

    print(filter(imf_cult_cbn, is.na(value)) %>% pull(iso3c) %>% table())

    ## imf_culture_wide <- pivot_wider(imf_culture, names_from = COFOG.Function.Code, values_from = value)

    ## return(
    ##     select(imf_culture, iso3c, year, Item = COFOG.Function.Code, Value = value) %>%
    ##     mutate(Item = paste0("IMF_", Item))
    ##     )
    return(na.omit(imf_cult_cbn))

}

## get_imf_data()
    
## imf_culture$region <- countrycode(imf_culture$iso3c, "iso3c", "un.region.name")

## viz_lines(imf_culture, x="year", y="value", time_level = "ra", duration = 3, grp="iso3c", facets = "region", max_lines = 6)

## cpltns_checker(filter(imf_culture, COFOG.Function.Code=="GF0802"), "value")
## cpltns_checker(filter(imf_culture, COFOG.Function.Code=="GF08"), "value")
       

## GF0802: so far basically Europe plus handful of Asian countries, no US (LUL/Latin America, Africa), around 800-900 nobs
## having all (GF08): 1400 nobs
## have to do that properly lol





## ** merging all

gen_cult_spending <- function() {
    #' combine everything into nice function
    

    df_cult <- as_tibble(Reduce(function(x,y) rbind(x,y),
                                list(
                                    gen_ilo_df(),
                                    combine_un_series(),
                                    get_imf_data_old())))

    
    ## *** completeness checks
    ## unique(df_cult$Item)

    ## cult_cpltns_res <- rbindlist(lapply(unique(df_cult$Item), \(x)
    ##                                     cpltns_checker(vx = filter(df_cult, Item == x)[,c("iso3c", "year")] %>%
    ##                                                        mutate(!!x := 1), varx = x))) %>%
    ##     as_tibble()

    ## cult_cpltns_res[order(cult_cpltns_res$nobs, decreasing = T),] %>%
    ##     select(varx, nobs, ratio_opngs_cvrd, nbr_of_crys_geq3) %>% adf()

    ## un_df <- combine_un_series()

    ## un_t1 <- filter(un_df, Item == "UN_SMOrc Recreation, culture and religion") %>% select(iso3c, year) %>%
    ##     mutate(x1 = 1) %>% unique()
    ## un_t2 <- filter(un_df, Item != "UN_SMOrc Recreation, culture and religion") %>% select(iso3c, year) %>%
    ##     mutate(x2 = 1) %>% unique()
    ## un_tc <- merge(un_t1, un_t2, all = T) %>% atb()
    ## filter(un_tc, is.na(x1), !is.na(x2))


    ## *** use UN_SMOrc Recreation, culture and religion (best coverage), add currencies
    df_wb$GDP.TTL <- df_wb$NY.GDP.PCAP.CD * df_wb$SP.POP.TOTL
    

    
    un_df_smorc <- filter(df_cult, Item == "UN_SMOrc Recreation, culture and religion") %>% na.omit()
    ## un_df_smorc <- filter(df_cult, Item == "UN_caution Recreation, culture and religion") %>% na.omit()
    
    un_df_smorc_cur <- as_tibble(merge(un_df_smorc, cur_df))

    ## at least not too many NAs for the currency conversions...
    un_df_smorc_cur_gdp <- as_tibble(merge(un_df_smorc_cur, select(df_wb, iso3c, year, GDP.TTL)))

    un_df_smorc_cur_gdp <- un_df_smorc_cur_gdp %>%
        mutate(value_constant = Value/inyixx999i) %>% 
        mutate(smorc_dollar_fx = value_constant/xlcusx999i_2021,
               smorc_dollar_ppp = value_constant/xlcusp999i_2021) %>% #
        mutate(pct_fx = 100*(smorc_dollar_fx/GDP.TTL),
               region = countrycode(iso3c, "iso3c", "un.region.name"),
               ## smorc_dollar_fxm = smorc_dollar_fx/1e6)
               smorc_dollar_fxm = smorc_dollar_fx/1e8)
    


    ## validity checks: yeet some countries/country years that are outstandingly weird

    ## un_df3 <- construct_gvt_consumption_expenditure()
    ## un_df3$Currency_tws <- trimws(un_df3$Currency)
    ## check_cry_cur("QAT")



    un_df_smorc_clean <- filter(un_df_smorc_cur_gdp, iso3c != "UKR" | year > 1996,
                                iso3c != "SMR",
                                iso3c != "ECU",
                                iso3c != "QAT",
                                iso3c != "GEO") ## weirdly high now after changing to constant prices too 
    
    ## viz_lines(un_df_smorc_clean, y="pct_fx", facets = "region")

    return(select(un_df_smorc_clean, iso3c, year, smorc_dollar_fxm, pct_fx))

}

## un_df_smorc_clean = gen_cult_spending()

## pdf(paste0(FIG_DIR, "UN_SMOrc_gvt_expenditure.pdf"), width = 17, height = 10)
## un_df_smorc_clean$region <- countrycode(un_df_smorc_clean$iso3c, "iso3c", "un.region.name")
## viz_lines(un_df_smorc_clean, y="pct_fx", facets = "region")
## ## viz_lines(un_df_smorc_cur_gdp, y="pct_fx", facets = "region")
## dev.off()




## hmm the basically all variables that combine have decent coverage -> could try imputing 

## UN_SMOrc Recreation, culture and religion is doing a lot of the heavy lifting 


## ** second version with imputing tlycgs

## *** oecd 

get_oecd_table11 <- function() {
    #' get the oecd 
    
    oecd_table11 <- atb(read.csv(paste0(OECD_DATA_DIR, "SNA_TABLE11")))
    ## table(oecd_table11$SECTOR)
    ## table(oecd_table11$TRANSACT)
    ## table(oecd_table11$UNIT)
    ## table(oecd_table11$POWERCODE)


    df_oecd_fltrd <- filter(oecd_table11,
                            TRANSACT %in% c("TLYCG", "P3CG"), # total gvt expenditure 
                            SECTOR == "GS13") %>% # general government 
        mutate(value = ObsValue * 10^POWERCODE,
               format = tolower(TRANSACT)) %>% 
        select(iso3c = LOCATION, year = Time,  currency = UNIT, value, format)

    df_oecd_mrgd <- merge(df_oecd_fltrd, cur_df, all.x = T) %>% atb() %>%
        merge(df_wb, all.x = T) %>% atb() %>%
        mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
               constant_usd = (value/inyixx999i)/xlcusx999i_2021, # hope it makes sense (Blanchet_2017_conversions)
               source = "oecd_table11") %>%
        select(iso3c, year, constant_usd, pct_value, source, format) %>% 
        pivot_longer(cols = c(constant_usd, pct_value), names_to = "measure")
    
    ## df_oecd_mrgd <- merge(df_oecd_fltrd,
    ##                       select(df_wb, iso3c, year, NY.GDP.MKTP.CN), all.x = T) %>% atb() %>%
    ##     mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
    ##            source = "oecd_table11")

    ## return(select(df_oecd_mrgd, iso3c, year, pct_value, source))
    return(select(df_oecd_mrgd, iso3c, year, value, measure, source, format) %>% na.omit())
}

get_oecd_table11_archive <- function() {
    #' generate the oecd table 11 archive data

    
    oced_table11_arc <- atb(read.csv(paste0(OECD_DATA_DIR, "SNA_TABLE11_ARCHIVE")))

    oecd_table11_arc_fltrd <- filter(oced_table11_arc, SECTOR == "GS13",
                                     TRANSACT %in% c("TLYCG", "P3CG")) %>%
        mutate(value = ObsValue*10^POWERCODE,
               format = tolower(TRANSACT)) %>%
        select(iso3c = LOCATION, year = Time,  currency = UNIT, value, format)

    ## filter out colombia in not COP: WID seems to assume COP 
    oecd_table11_arc_fltrd <- filter(oecd_table11_arc_fltrd, iso3c != "COL" | (iso3c == "COL" & currency == "COP"))

    oecd_table11_arc_mrgd <- merge(oecd_table11_arc_fltrd, cur_df, all.x = T) %>% atb() %>% 
        merge(df_wb, all.x = T) %>% atb() %>%
        mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
               constant_usd = (value/inyixx999i)/xlcusx999i_2021, # hope it makes sense (Blanchet_2017_conversions)
               source = "oecd_table11_arc") %>%
        select(iso3c, year, constant_usd, pct_value, source, format)  %>%
        pivot_longer(cols = c(constant_usd, pct_value), names_to = "measure")
    

    ## oecd_table11_arc_mrgd <- merge(oecd_table11_arc_fltrd,
    ##                                select(df_wb, iso3c, year, NY.GDP.MKTP.CN), all.x = T) %>% atb() %>%
    ##     mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
    ##            region = countrycode(iso3c, "iso3c", "un.region.name"),
    ##            source = "oecd_table11_arc")
    
    ## viz_lines(oecd_table11_arc_mrgd, y="pct_value", facets = "region")
    ## looks somewhat plausible

    return(select(oecd_table11_arc_mrgd, iso3c, year, value, measure, source, format) %>% na.omit())

}

## *** un

get_un_data <- function() {
    #' construct the un cultural spending data
    #' separate function just for spending 
    
    
    
    un_df <- construct_gvt_consumption_expenditure()
    
    ## old version: just mean 
    ## un_df_clpsd <- un_df %>% group_by(iso3c, year) %>%
    ##     summarize(value = mean(Value))


    ## exclude a bunch of stuff that makes no sense on visual inspection
    un_df_fltrd <- un_df %>%
        filter(iso3c != "JPN" | (iso3c == "JPN" & Series == 1000),
               iso3c != "ARM" | (iso3c == "ARM" & Series == 1000)) %>%
        group_by(iso3c, year) %>%
        mutate(series1k_there = ifelse(1000 %in% Series, T, F))
                  
    

    ## where Series 1k is available, use 1k, else mean 
    un_df_clpsd <- rbind(    
        filter(un_df_fltrd, series1k_there, Series==1000) %>%
        group_by(iso3c, year) %>%
        summarize(value = Value),
        filter(un_df_fltrd, !series1k_there) %>%
        group_by(iso3c, year) %>%
        summarize(value = mean(Value))
        )
    
    ## ## check for number of currencies
    ## un_df_fltrd %>%
    ##     group_by(iso3c) %>%
    ##     mutate(nbr_curs = n_distinct(Currency)) %>%
    ##     filter(nbr_curs > 1) %>% 
    ##     select(iso3c, year, Series, Value, Currency) %>% adf()

    ## un_df_fltrd %>% filter(iso3c == "TZA") %>%
    ##     ggplot(aes(x=year, y=Value, group = Series, shape = Currency, color = factor(Series))) +
    ##     geom_line() +
    ##     geom_point()
        

    ## un_df_fltrd %>% filter(iso3c == "TZA") %>% pull(Currency) %>% unique()


    ## ## countries that at some point have multiple series, and not 1k everywhere
    ## pdf(paste0(FIG_DIR, "un_cult_spending_series_visual_inspection.pdf"), width = 16, height = 10)
    ## un_df2 %>%
    ##     group_by(iso3c) %>%
    ##     mutate(nbr_series = len(unique(Series)),
    ##            series1k_perfect = all(series1k_there)) %>%
    ##     filter(nbr_series > 1, !series1k_perfect) %>%
    ##     ggplot(aes(x=year, y=Value, grp = Series, color = factor(Series))) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")
    ## dev.off()

    ## un_df_clpsd %>%
    ##     ## filter(iso3c %in% all_of(mult_series_crys)) %>% 
    ## ggplot(aes(x=year, y=value)) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")
           
    ## data - Japan 
    ## filter(un_df, iso3c == "JPN") %>% select(year, Series, value = Value) %>%
    ##     rbind(mutate(japan_mof, Series = 111, value = value*1e9)) %>%
    ##     ggplot(aes(x=year, y=value, group = Series, color = factor(Series))) +
    ##     geom_line()


    ## countries with multiple mismatched series for the years without 1k series
    ## filter(un_df2, !series1k_there, iso3c %in% mult_series_crys) %>%
    ##     ggplot(aes(x=year, y=Value, grp = Series, color = factor(Series))) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")

    ## ## countries with multiple mismatched series generally 
    ## filter(un_df2, iso3c %in% mult_series_crys) %>%
    ##     ggplot(aes(x=year, y=Value, grp = Series, color = factor(Series))) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")


    ## filter(un_df2, !series1k_there) %>%
    ##     group_by(iso3c) %>%
    ##     mutate(nbr_series = len(unique(Series))) %>%
    ##     filter(nbr_series > 1) %>%     
    ##     ggplot(aes(x=year, y=Value, grp = Series, color = factor(Series))) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")


    ## ## all country-years without 1k series
    ## filter(un_df2, !series1k_there) %>% 
    ##     ggplot(aes(x=year, y=Value, grp = Series, color = factor(Series))) +
    ##     geom_line() +
    ##     facet_wrap(~iso3c, scales = "free")

    
    
    ## filter(un_df_clpsd, iso3c == "HRV") %>% pull(value) %>% plot(type = "l")

    ## filter(un_df_clpsd, iso3c == "UKR", year < 2000)
    ## filter(cur_df, iso3c == "UKR", year < 2000)

    ## filter out whack data for Ukraine pre-1996, and other crappy cases 
    un_df_clpsd <- un_df_clpsd %>%
        filter(iso3c != "UKR" | (iso3c == "UKR" & year > 1996),
               iso3c != "ECU" | (iso3c == "ECU" & year < 1990),
               iso3c != "GEO")
               
    ## filter(x, iso3c == "UKR")
    ## filter(un_df_clpsd, iso3c == "UKR")

    ## convert SMR lira to euro 1999 fx rate: https://tassidicambio.bancaditalia.it/terzevalute-wf-ui-web/timeSeries
    un_df_clpsd <- un_df_clpsd %>%
        mutate(value = ifelse(iso3c == "SMR" & year %in% seq(1997, 1999), value/1936.27, value))

    
    un_df_cbn <- merge(un_df_clpsd, cur_df, all.x = T) %>% atb() %>%
        merge(., df_wb) %>% atb() %>%
        mutate(constant_usd = (value/inyixx999i)/xlcusx999i_2021,
               pct_value = (value/NY.GDP.MKTP.CN)*100,
               source = "un",
               format = "p3cg") %>%
        select(iso3c, year, constant_usd, pct_value, source, format) %>% 
        pivot_longer(cols = c(constant_usd, pct_value), names_to = "measure")
    ## filter_at(un_df_cbn, vars(-constant_usd, pct_value), any_vars(is.na(.)))
    

    print("un data NAs")
    print(filter(un_df_cbn, is.na(value)) %>% pull(iso3c) %>% table())
    
    
    return(na.omit(un_df_cbn)) 

}


## *** eurostat    

## *** eurostat
get_eurostat <- function() {
    #' generate the eurostat data
    
    ## df_eurostat <- atb(read.csv(paste0(PROJECT_DIR, "data/eurostat/gov_10a_exp__custom_2811962_page_linear.csv")))

    df_eurostat <- atb(read.csv(paste0(PROJECT_DIR, "data/eurostat/new/gov_10a_exp__custom_2827619_linear.csv")))

    df_eurostat$iso3c <- countrycode(df_eurostat$geo, "iso2c", "iso3c", custom_match = c("EL" = "GRC"))

    df_euro_fltrd <- df_eurostat %>%
        mutate(iso3c = countrycode(geo, "iso2c", "iso3c", custom_match = c("EL" = "GRC")),
               year = TIME_PERIOD) %>%
        select(iso3c, year, unit, cofog99, value = OBS_VALUE) %>%
        filter(cofog99 == "GF08", unit == "MIO_NAC" | unit == "PC_GDP") %>%
        pivot_wider(names_from = unit, values_from = value)
     
    
   
    ## viz_lines(df_euro_fltrd, y="OBS_VALUE", facets = "iso3c")
    df_euro_fltrd2 <- merge(df_euro_fltrd, cur_df, all.x = T) %>% atb() %>%
        mutate(constant_usd = (MIO_NAC*1e6/inyixx999i)/xlcusx999i_2021,
               source = "eurostat",
               format = "tlycg") %>%
        select(iso3c, year, constant_usd, pct_value = PC_GDP, source, format) %>%
        pivot_longer(cols = c(constant_usd, pct_value), names_to = "measure")
               
    ## viz_lines(df_euro_fltrd2, y="pct_value", duration = 1, facets = "iso3c")

    ## check that eurostat geo columns adheres to iso2c (does so everywhere except greece -> manual exception)
    ## unique(select(df_eurostat, iso3c, geo)) %>% na.omit() %>%
    ##     mutate(geo2 = countrycode(iso3c, "iso3c", "iso2c")) %>%
    ##     filter(geo != geo2)
        
    ## df_euro_fltrd <-df_eurostat %>%
    ##     mutate(iso3c = countrycode(geo, "iso2c", "iso3c", custom_match = c("EL" = "GRC")),
    ##            source = "eurostat") %>% na.omit() %>% 
    ##     select(iso3c, year = TIME_PERIOD, pct_value = OBS_VALUE, source)
    
    return(df_euro_fltrd2)
}

sort_by_priority <- function(dfx, priority_vec, pos) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' pick all obs that adhere to priority_vec[pos], but only if not also matched by higher priority
    #' to be used with lapply(seq_along(priority_vec))
    #' needs source column that corresponds to values in priority_vec

    prty_vlu <- priority_vec[pos]
    vlus_to_disregard <- priority_vec[0:(pos-1)]
    
    grouping_vars <- intersect(c("iso3c", "year"), names(dfx))

    ## print(grouping_vars)
    ## dfx %>% group_by(iso3c, year) %>%
    dfx %>% group_by_at(c(grouping_vars)) %>%
    ## dfx %>% group_by(iso3c) %>%
        ## first exclude all the higher priorities
        mutate(matched_by_higher_prorities = ifelse(len(intersect(source, vlus_to_disregard))==0, F, T)) %>%
        filter(!matched_by_higher_prorities, source == prty_vlu) %>%
        select(all_of(names(dfx)))
}

## df_cprn %>% filter(format == "tlycg") %>% 
##     sort_by_priority(source_priority, 1)


gen_cult_spending_source_df <- function() {

    df_cprn <- atb(Reduce(\(x,y) rbind(x,y),
                          list(
                              get_un_data(),
                              get_oecd_table11(),
                              get_oecd_table11_archive(),
                              get_imf_data(),
                              get_eurostat())))

    
    return(df_cprn)
}


## df_cprn <- gen_cult_spending_source_df()

## look at countries with multiple sources -> see if there are disruptions
## only CHL: drop un series
## df_cprn %>% filter(format == "p3cg", measure == "constant_usd") %>%
##     group_by(iso3c) %>%
##     mutate(nbr_sources = len(unique(source))) %>%
##     filter(nbr_sources > 1) %>%
##     ggplot(aes(x=year, y=value, group=source, color = source)) +
##     geom_line() +
##     geom_jitter() + 
##     facet_wrap(~iso3c, scales = "free")


## *** choose p3cg series 
## drop CHL un series
## also pick p3cg series in order of oecd_table11, un, oecd_t11_archive

choose_p3cg_series <- function(df_cprn) {
    #' select the p3cg data for each CY based on source_priority_vec_p3cg

    source_priority_vec_p3cg <- c("oecd_table11", "un", "oecd_table11_arc")

    df_p3cg <- df_cprn %>% filter(format == "p3cg", iso3c != "CHL" | (iso3c=="CHL" & source != "un"))

    df_p3cg_fltrd <- lapply(seq_along(source_priority_vec_p3cg), \(pos)
                            sort_by_priority(
                                dfx = df_p3cg,
                                priority_vec = source_priority_vec_p3cg,
                                pos = pos)) %>%
        Reduce(\(x,y) rbind(x,y), .) %>%
        select(iso3c, year, source, format, measure, value)

    return(df_p3cg_fltrd)
}


## visual check: doesn't seem to work: still multiple values there; later: fixed
## df_p3cg_fltrd %>% filter(measure == "pct_value") %>% 
##     ggplot(aes(x=year, y=value, group=source, color = source)) +
##     geom_line() +
##     geom_jitter() + 
##     facet_wrap(~iso3c, scales = "free")

## *** choose tlycg series 
## df_cprn %>% filter(format == "tlycg", measure == "pct_value", source != "oecd_table11_arc") %>%
##     group_by(iso3c) %>%
##     mutate(nbr_sources = len(unique(source))) %>%
##     filter(nbr_sources > 1) %>% 
##     ggplot(aes(x=year, y=value, group=source, color = source)) +
##     geom_line() +
##     geom_jitter() + 
##     facet_wrap(~iso3c, scales = "free")

choose_tlycg_series <- function(df_cprn) {
    #' select the tlycg data for each CY based on source_priority_vec_tlycg

    source_priority_vec_tlycg <- c("oecd_table11", "imf", "eurostat", "oecd_table11_arc")


    df_tlycg_fltrd <- lapply(seq_along(source_priority_vec_tlycg), \(pos) sort_by_priority(
                                                                    dfx = filter(df_cprn, format == "tlycg"),
                                                                    priority_vec = source_priority_vec_tlycg,
                                                                    pos = pos)) %>%
        Reduce(\(x,y) rbind(x,y), .)

    return(df_tlycg_fltrd)
    }

## ggplot(filter(df_tlycg_cbn, measure == "pct_value"), aes(x=year, y=value, color = source)) +
##     geom_line() +
##     geom_point() +
##     facet_wrap(~iso3c, scales = "free")

    

## df_cprn has 15,076
## filtered has 15,040 -> lose 36 huh: also lose 12 CYs with iso3c == NA

    
## *** combine filtered tlycg and p3cg for imputation

inf.omit <- function(vec) {
    vec[!is.infinite(vec)]
    }


select_proper_tlycg_series <- function(df_cult_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' impute tlycg where necessary, pick the highest-quality series for each CY
    
    ## get the countries with overlapping series -> mean for all 
    df_cult_sclr <- df_cult_cbn %>% group_by(iso3c, year) %>%
        mutate(nbr_formats_cy = len(unique(format))) %>%
        group_by(iso3c) %>%
        mutate(nbr_formats_c = len(unique(format)), ## just one series -> use general scaler
               nbr_formats_cy_max = max(nbr_formats_cy)) %>% ## to filter countries that never have overlap
        select(iso3c, year, format, measure, value, nbr_formats_cy, nbr_formats_c, nbr_formats_cy_max) %>% 
        pivot_wider(names_from = format, values_from = value) %>%
        mutate(scaler = tlycg/p3cg)


    ## generate median scalar for everybody 
    med_scaler_vlu <- df_cult_sclr %>% pull(scaler) %>% na.omit() %>% inf.omit() %>% median()
    ## print(med_scaler_vlu)


    ## get countries where series overlap at least at some points to calculate country-specific scalars
    crys_ovlp <- filter(df_cult_sclr, nbr_formats_c == 2, nbr_formats_cy_max == 2)
    crys_ovlp_scaler <- crys_ovlp %>%
        group_by(iso3c) %>%
        summarize(scaler_cry = median(inf.omit(scaler), na.rm = T)) %>% na.omit()

    
    ## add the country-level scalar of those countries that have them 
    df_cult_sclr_cbn <- merge(df_cult_sclr, crys_ovlp_scaler, all.x = T) %>% atb()



    ## filter(df_cult_sclr_cbn, p3cg < 0) %>% pull(p3cg) %>% hist()
    ## filter(df_cult_sclr_cbn, iso3c == "YEM") %>% adf()
    ## yeet CYs with negative cultural spending
    df_cult_sclr_cbn1 <- df_cult_sclr_cbn %>% mutate(p3cg = ifelse(p3cg < 0, NA, p3cg))

    

    ## compute the different tlycg values 
    df_cult_sclr_cbn2 <- df_cult_sclr_cbn1 %>%
        filter(measure == "constant_usd") %>% 
        select(iso3c, year, p3cg, tlycg, scaler_cry) %>%
        mutate(scaler_med = med_scaler_vlu,
               tlycg_ovlp = p3cg * scaler_cry,
               tlycg_avg = p3cg * scaler_med)

    ## pivot into longer format for sort by_priority, also yeet NAs for that
    df_cult_sclr_cbn3 <- df_cult_sclr_cbn2 %>%
        select(iso3c, year, tlycg, tlycg_ovlp, tlycg_avg) %>% 
        pivot_longer(cols = c(tlycg, tlycg_ovlp, tlycg_avg), names_to = "source") %>%
        na.omit()

    ## actually sort by priority
    tlycg_priority_vec <- c("tlycg", "tlycg_ovlp", "tlycg_avg")
    df_tlycg_fnl <- lapply(seq_along(tlycg_priority_vec), \(pos)
                           sort_by_priority(dfx = df_cult_sclr_cbn3,
                                            priority_vec = tlycg_priority_vec,
                                            pos = pos)) %>%
        Reduce(\(x,y) rbind(x,y), .)

    return(df_tlycg_fnl)
}

gen_cult_spending_imptd <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' wrapper function for combining and imputing different tlycg/p3cg series

    df_cprn <- gen_cult_spending_source_df()
    df_p3cg_fltrd <- choose_p3cg_series(df_cprn)
    df_tlycg_fltrd <- choose_tlycg_series(df_cprn)
    

    ## i think it's not feasible to maintain source: p3cg and tlycg often come from different sources 
    df_cult_cbn <- rbind(df_p3cg_fltrd, df_tlycg_fltrd) %>% atb()


    df_tlycg_fnl <- select_proper_tlycg_series(df_cult_cbn)

    df_tlycg_fnl %>%
        mutate(smorc_dollar_fxm = value/1e6,
               smorc_dollar_fxm_sqrd = smorc_dollar_fxm^2) %>%
        select(iso3c, year, smorc_dollar_fxm, smorc_dollar_fxm_sqrd)
    
}
