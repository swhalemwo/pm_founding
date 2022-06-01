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


    ## imf_culture_wide <- pivot_wider(imf_culture, names_from = COFOG.Function.Code, values_from = value)

    ## return(
    ##     select(imf_culture, iso3c, year, Item = COFOG.Function.Code, Value = value) %>%
    ##     mutate(Item = paste0("IMF_", Item))
    ##     )
    return(imf_cult_cbn)

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
    ##     select(varx, nobs, ratio_opngs_cvrd, nbr_of_crys_geq3)


    ## *** use UN_SMOrc Recreation, culture and religion (best coverage), add currencies
    df_wb$GDP.TTL <- df_wb$NY.GDP.PCAP.CD * df_wb$SP.POP.TOTL
    

    
    un_df_smorc <- filter(df_cult, Item == "UN_SMOrc Recreation, culture and religion") %>% na.omit()
    
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





    



