## * cultural spending stuff


culture_df <- filter_sdmx_results("cultural services")
filter(culture_df, !grepl("agricult", description.en, ignore.case = T))
filter(culture_df, !grepl("agricult", description.en, ignore.case = T))$description.en
filter(culture_df, !grepl("agricult", description.en, ignore.case = T))$id

grepl("agricult", culture_df$description.en, ignore.case = T)



filter_sdmx_results("museum") %>% as.data.frame()
    

## * check my original findings
## no idea how I got those anymore

culture_df2 <- filter_sdmx_results("cultural")

## ** STAN generally 

filter(culture_df2, grepl("STAN", sdmx_id, ignore.case = T))$description.en

## ** stan08bis

filter(culture_df2, sdmx_id == "STAN08BIS")

df_stan08bis <- as_tibble(read.csv(paste0(OECD_DATA_DIR, "STAN08BIS")))
as.data.frame(head(df_stan08bis))

table(df_stan08bis$VAR) %>% sort() %>% as.data.frame()

## ** STANI4_2016

filter(culture_df2, sdmx_id == "STANI4_2016")
df_stani4_2016 <- as_tibble(read.csv(paste0(OECD_DATA_DIR, "STANI4_2016")))

table(df_stani4_2016$VAR) %>% sort()



## ** ILO


proc_ilo_df <- function(ilo_df_id) {
    #' process the ILO dfs (different resolution)

    dfx <- ilo_dfs[[ilo_df_id]]

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


    ilo_df <- Reduce(function(x,y,...) rbind(x,y), lapply(names(ilo_dfs), proc_ilo_df))
    return(ilo_df)
}





## ** UN

## *** WID currency converter

get_wid_cur_converter <- function() {

    currency_cmd <- paste0("select iso3c, year, variable, value from wid_v2 where variable='xlcusp999i' or variable = 'xlcusx999i' and year>=", STARTING_YEAR)
    wid_cur_df <- as_tibble(dbGetQuery(con, currency_cmd))
    return(wid_cur_df)
}

wid_cur_df <- get_wid_cur_converter()



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
        select(iso3c, year, Value, Series, Currency_tws) %>%
        rbind(., select(., iso3c, year, Value, Series = Currency_tws) %>%
                 ## add also a facet with all the currencies combined
                 ## joins stuff of different series together tho 
                 mutate(Currency_tws = "curs")) %>%
        rbind(filter(wid_cur_df, iso3c == cry, year > STARTING_YEAR) %>%
              mutate(Currency_tws = "wid", Series = "wid") %>%
              select(iso3c, year, Value = value, Currency_tws = variable, Series)) %>%
        ggplot(aes(x=year, y=Value, color = factor(Series))) +
        facet_wrap(~Currency_tws, scales = "free_y", nrow=3) +
        geom_line(size=1.5)

}

## check_cry_cur("DEU")


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
        list(from = "Swedish krona", to = "Swedish Krona")
    )

    countries_to_yeet <- c("VEN")
    ## VEN data is whack

    ## https://stackoverflow.com/questions/19265172/converting-two-columns-of-a-data-frame-to-a-named-vector
    curs_to_rename_cfg <- rbindlist(curs_to_rename) %>%
        pull(to, from)

    ## https://github.com/tidyverse/dplyr/issues/3899

    un_df3_fltrd <- filter(un_df3, Currency_tws %!in% curs_to_yeet, iso3c %!in% countries_to_yeet, year >= STARTING_YEAR)
    un_df3_fltrd$Currency_tws <- recode(un_df3_fltrd$Currency_tws, !!!curs_to_rename_cfg)

    ## un_df3_fltrd %>%
    ##     ## filter(Series == 1000) %>%
    ##     group_by(iso3c, year) %>%
    ##     summarize(some_val = 1) %>%
    ##     cpltns_checker(varx = "some_val")
    ## ## using all Series or not substantially affects sample size..

    return(select(un_df3_fltrd, iso3c, year, Item, Series, Value, Currency))
}
    
    

## *** series combining

combine_un_series <- function() {
    #' construct the UN series by combining un_df2 (output_gross_value_added_fixed_assests_industry_cur_prices) and un_df3 (govt consumption expenditure)
    #' also construct cautious (only series 1000) and risky/SMOrc (mean of all series) for each indicator

    un_df2 <- construct_un_df2()
    un_df3 <- construct_gvt_consumption_expenditure()

    un_df <- as_tibble(rbind(un_df2, un_df3))

    un_df$region <- countrycode(un_df$iso3c, "iso3c", "un.region.name")

    ## **** smorc: just calc mean
    un_df_smorc <- un_df %>%
        group_by(iso3c, year, Item, Series) %>%
        summarize(Value = mean(Value), Item = paste0("UN_SMorc ", Item)) %>%
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

get_imf_data()
    
## imf_culture$region <- countrycode(imf_culture$iso3c, "iso3c", "un.region.name")

## viz_lines(imf_culture, x="year", y="value", time_level = "ra", duration = 3, grp="iso3c", facets = "region", max_lines = 6)

cpltns_checker(filter(imf_culture, COFOG.Function.Code=="GF0802"), "value")
cpltns_checker(filter(imf_culture, COFOG.Function.Code=="GF08"), "value")
       

## GF0802: so far basically Europe plus handful of Asian countries, no US (LUL/Latin America, Africa), around 800-900 nobs
## having all (GF08): 1400 nobs
## have to do that properly lol


## *** correlation check
## compare cultural services (GF0802) and Expenditure on recreation, culture, & religion (GF08, overarching category)
## have to do that systematically too 



imf_cpr <- filter(imf_df_melt,
       COFOG.Function.Code %in% c("GF08", "GF0802"),
       Sector.Name == "General government",
       Unit.Name == "Percent of GDP",
       ## Country.Name == "Germany",
       year >= 1985,
       Attribute == "Value",
       value != ""
       ) %>%
    mutate(value=as.numeric(value)) %>%
    select(iso3c = Country.Name, year, cofog_code = COFOG.Function.Code, value)

imf_cpr_wide <- pivot_wider(imf_cpr, names_from = cofog_code)

imf_cpr_wide <- imf_cpr %>%
    group_by(iso3c, cofog_code) %>%
    mutate(vlu_mean = mean(value)) %>%
    mutate(diff = value - vlu_mean)
    
imf_cpr_wide2 <-
    imf_cpr_wide %>%
    select(iso3c, year, cofog_code, diff) %>%
    pivot_wider(names_from = cofog_code, values_from = diff)


imf_cpr_wide %>%
    group_by(iso3c) %>%
    mutate(GF08_mean = mean(GF08, na.rm = T), GF0802_mean = mean(GF0802, na.rm = T))





cor(imf_cpr_wide$GF08, imf_cpr_wide$GF0802, use = "complete.obs")
cor(imf_cpr_wide2$GF08, imf_cpr_wide2$GF0802, use = "complete.obs")

na.omit(imf_cpr_wide)

plot(imf_cpr_wide2$GF08, imf_cpr_wide2$GF0802)
## hmm 0.85 correlation, that seems pretty good tbh
## but what if values are not missing at random, which they probably aren't?
## SOL?
## there could be countries that don't fund cultural services at all?

## also need to consider longitudinal nature, can't just throw them all together
## demeaning both vars: correlation down to 0.63

ggplot(imf_cpr_wide2, aes(x=GF08, y=GF0802, color=iso3c)) +
    geom_point(show.legend=FALSE) +
    xlim(-0.5, 0.5) +
    ylim(-0.25, 0.25)

## ** add currencies
wid_cur_df <- rename(wid_cur_df, conversion = value)

wid_cur_df_wide <- pivot_wider(wid_cur_df, names_from = variable, values_from = conversion) %>%
    filter(year >= STARTING_YEAR)
    
df_wb$GDP.TTL <- df_wb$NY.GDP.PCAP.CD * df_wb$SP.POP.TOTL

un_df_cur_caution <- as_tibble(merge(un_df_caution, wid_cur_df_wide))

un_df_cur_caution_gdp <- as_tibble(merge(un_df_cur_caution, select(df_wb, iso3c, year, GDP.TTL)))

## should probably divide
un_df_cur_caution_gdp$caution_dollar <- un_df_cur_caution_gdp$caution / un_df_cur_caution_gdp$xlcusx999i
un_df_cur_caution_gdp$pct <- 100*(un_df_cur_caution_gdp$caution_dollar/un_df_cur_caution_gdp$GDP.TTL)



filter(un_df_cur_caution_gdp, Item == "MIXED INCOME, NET")

rbindlist(lapply(unique(un_df_cur_caution_gdp$Item), \(x)
       cpltns_checker(vx = filter(un_df_cur_caution_gdp, Item == x)[,c("iso3c", "year")] %>%
                          mutate(!!x := 1), varx = x))) %>%
    filter(PMs_covered_raw > 200)

pdf(paste0(FIG_DIR, "un_value_added.pdf"), width = 17, height = 10)
filter(un_df_cur_caution_gdp, Item == "Recreation, culture and religion") %>%
    viz_lines(x="year", y="pct", grp = "iso3c", time_level = "ra", duration = 4, facets = "region", max_lines = 8)
dev.off()

## hmmm some countries have absurdly high percentages still -> check them individually: un data or conversion factor
## fortunately not that many 

un_df_cur_caution_gdp %>%
    group_by(iso3c, Item) %>%
    summarize(max_pct = max(pct)) %>%
    filter(max_pct > 5) %>%
    count(iso3c) %>%
    ;

filter(un_df_cur_caution, 


    



