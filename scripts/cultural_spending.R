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
ilo_df_files <- list.files(paste0(PROJECT_DIR, "data/ILO"))
names(ilo_df_files) <- lapply(ilo_df_files, function(x) substring(x, 1, nchar(x)-4))

ilo_dfs <- lapply(ilo_df_files, function(x) as_tibble(read.csv(paste0(PROJECT_DIR, "data/ILO/", x))))

name_ilo_df <- function(x) {
    col_name = enquo(x)

    select(ilo_dfs[[x]], ref_area.label, time, get(col_name) = obs_value)
}
    

lapply(names(ilo_dfs), function(x)
    
    )



ilo_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/ILO/EMP_TEMP_SEX_ECO_NB_A-filtered-2022-03-31.csv")))
ilo_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/ILO/EMP_TEMP_SEX_EC2_NB_A-filtered-2022-03-31.csv")))
as.data.frame(head(ilo_df))


ilo_df$iso3c <- countrycode(ilo_df$ref_area.label, "country.name", "iso3c")
ilo_df$year <- ilo_df$time
ilo_df$region <- countrycode(ilo_df$iso3c, "iso3c", "un.region.name")

viz_lines(ilo_df, x="year", y="obs_value", time_level = "ra", grp= "iso3c", duration = 4, facets = "region", max_lines = 8)

ilo_df %>%
    group_by(iso3c) %>%
    summarize(min_year = min(year)) %>%
    pull(min_year) %>%
    hist()





cpltns_checker(filter(ilo_df, sex.label == "Sex: Total") , "obs_value")

## ** UN

## un_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/UNdata_Export_20220331_131339247.csv")))
## table(un_df$SNA93.Item.Code)

## un_df$iso3c <- countrycode(un_df$Country.or.Area, "country.name", "iso3c")
## un_df$region <- countrycode(un_df$iso3c, "iso3c", "un.region.name")
## un_df$year <- un_df$Year

## filter(un_df, SNA93.Item.Code=="R") %>% na.omit() %>%
##     cpltns_checker(varx="Value")
    
## filter(un_df, SNA93.Item.Code=="R") %>%
##     pull(iso3c) %>%
##     table()
              

## filter(un_df, SNA93.Item.Code=="R") %>%
##     viz_lines(x="year", y="Value", time_level = "ra", grp= "iso3c", duration = 4, facets = "region", max_lines = 8)

un_dfs <- list(
list(filename="UNdata_output_gross_value_added_fixed_assests_industry_cur_prices.csv", yearcol="Year"),
list(filename="UNdata_value_added_cur_prices_ISIC.csv", yearcol="Year"),
list(filename="UNdata_value_added_industry_constant_prices.csv", yearcol="Fiscal.Year"),
list(filename="UNdata_value_added_by_econ_activity_cur_prices_nat_cur.csv", yearcol="Year")
)

names(un_dfs) <- unlist(lapply(un_dfs, function(x) substring(x['filename'], first=1, last=nchar(x['filename'])-4)))

un_df <- lapply(un_dfs, function(x) as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/", x['filename']))))



check_un_cpltns <- function(filename, yearcol){
    
    un_dfx <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/", filename)))

    un_dfx$iso3c <- countrycode(un_dfx$Country.or.Area, "country.name", "iso3c")
    un_dfx$year <- un_dfx[[yearcol]]

    cpltns_res <- un_dfx %>%
        group_by(iso3c, year) %>%
        summarize(some_val=1) %>%
        cpltns_checker(varx="some_val")

    return(cpltns_res)
    
}

rbindlist(lapply(un_dfs, function(x) check_un_cpltns(x[['filename']], x[['yearcol']])))
## only UNdata_output_gross_value_added_fixed_assests_industry_cur_prices.csv has any decent coverage

un_df2 <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/", "UNdata_output_gross_value_added_fixed_assests_industry_cur_prices.csv")))

## but also so many different things

table(un_df2$Country.or.Area)
table(un_df2$Item) %>% sort()

un_df2$iso3c <- countrycode(un_df2$Country.or.Area, "country.name", "iso3c")
un_df2$year <- un_df2$Year
un_df2$region <- countrycode(un_df2$iso3c, "iso3c", "un.region.name")

## pivoting UN df to wide to see how good coverage is if all variables are combined

un_df2_wide <- select(un_df2, iso3c, year, Item, Value, Series) %>%
    filter(Item != "") %>%
    pivot_wider(names_from = Item, values_from = Value)

filter(un_df2_wide, Series==1000) %>%
    apply(2, function(x) cpltns_checker(varx = x))


## compare coverage of series 1000 with no series restriction


un_cpltns_check_1k <- lapply(head(unique(un_df2$Item),-1),
       function(x) cpltns_checker(vx = filter(un_df2_wide, Series ==1000)[,c("iso3c", "year", x)], varx = x)) %>%
    rbindlist() %>%
    filter(PMs_covered_raw > 200)

un_cpltns_check_all <- lapply(head(unique(un_df2$Item),-1),
       function(x) cpltns_checker(vx = filter(un_df2_wide)[,c("iso3c", "year", x)], varx = x)) %>%
    rbindlist() %>%
    filter(PMs_covered_raw > 200)

## calculating diffs of nobs and percentages of PM foundings covered depending on series choice
merge(
    select(un_cpltns_check_1k, varx, ratio_opngs_cvrd_1k = ratio_opngs_cvrd, nobs_1k = nobs),
    select(un_cpltns_check_all, varx, ratio_opngs_cvrd_all = ratio_opngs_cvrd, nobs_all = nobs)) %>%
    mutate(diff_nobs = nobs_all - nobs_1k, diff_ratio = ratio_opngs_cvrd_all - ratio_opngs_cvrd_1k)

series_cprn <- as_tibble(merge(filter(un_df2, Series==1000) %>%
    select(iso3c, year) %>%
    unique() %>%
    mutate(series1k=1), 
filter(un_df2) %>%
    select(iso3c, year) %>%
    unique() %>%
    mutate(series_all=1), all=T))

series_cry_cprn <- filter(series_cprn, is.na(series1k)) %>%
    count(iso3c) %>%
    arrange(desc(n)) %>%
    as.data.frame()

## check if countries with non-1k Series change 
non1k_crys <- na.omit(series_cry_cprn$iso3c)[1:9]

filter(un_df2, iso3c %in% non1k_crys, Item =="Equals: VALUE ADDED, GROSS, at basic prices") %>%
    count(iso3c,Series)

## hmm now only testing for one Item, and for some countries
## I think I should generalize this, but requires abstraction

## I hope product does some good job of maximizing country-variables that differ in Series

select(un_df2, iso3c, year, Item, Series) %>%
    mutate(n=1) %>%
    group_by(iso3c, Item, Series) %>%
    summarize(cnt=sum(n), one=1) %>%
    group_by(iso3c, Item) %>%
    summarize(n2=sum(cnt), n1=sum(one), prod_cnt = prod(cnt), div_cnt = prod(1/cnt)) %>%
    filter(n1==2) %>%
    arrange(desc(prod_cnt))


filter(un_df2, Item == "Gross fixed capital formation", iso3c== "ISL")$year %>% sort()

## fuck with ISL it's that variables are reported in multiple formats
## tbh this is also a good comparison: if country-year-variables are reported in same series, then I can run correlation

## fucking abstractions
## alternative is disruption: seeing if change in series causes change in values
## fuck i'm getting tired

                                                                        



## want to see which countries have which variables on multiple series
## 

## hmm using only 1000 series decreases number by quite something

un_df2_wide$ttl <- rowsum
    


## figuring out non-unique values are due to different Series 
## un_df2_wide <- select(un_df2, iso3c, year, Item, Value, Series) %>%
##     filter(Item != "") %>%
##     unique() %>%
##     pivot_wider(names_from = Item, values_from = Value, values_fn = length) %>%
##     pivot_longer(cols = head(unique(un_df2$Item),-1))

## filter(un_df2_wide, value > 1) %>%
##     count(name) %>%
##     arrange(n)

## filter(un_df2, Item == "Equals: VALUE ADDED, GROSS, at basic prices") %>%
##     select(iso3c, year, Item, Value) %>%
##     count(iso3c, year, Item, Value) %>%
##     filter(n>1) %>%
##     count(iso3c)

## filter(un_df2, Item == "Equals: VALUE ADDED, GROSS, at basic prices", iso3c=="BIH") %>%
##     select(iso3c, Year, Series, Fiscal.year.type, Value) %>%
##     group_by(Year) %>%
##     as.data.frame()


    
    




filter(un_df2, Item == "Equals: VALUE ADDED, GROSS, at basic prices") %>%
    select(iso3c, year, Value, region) %>%
    viz_lines(x="year", y="Value", grp="iso3c", time_level = "ra", duration = 4, facets = "region")
    

filter(un_df2, Item == "Equals: VALUE ADDED, GROSS, at basic prices") %>%
    select(iso3c, year, Value, region) %>%
    cpltns_checker(varx = "Value")

filter(un_df2, Item == "Equals: VALUE ADDED, GROSS, at basic prices") %>%
    select(iso3c, year, Value, region) %>%
    head()

## ** government expenditure by function

un_df3 <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/UN/UNdata_Gvt_consumption_expenditure.csv")))

head(as.data.frame(un_df3))

un_df3$iso3c <- countrycode(un_df3$Country.or.Area, "country.name", "iso3c")
un_df3$year <- un_df3$Year
un_df3$region <- countrycode(un_df3$iso3c, "iso3c", "un.region.name")

un_df3 %>%
    group_by(iso3c, year) %>%
    summarize(some_val=1) %>%
    cpltns_checker("some_val")

viz_lines(un_df3, x="year", y="Value", grp="iso3c", time_level = "ra", duration = 4, max_lines = 8, facets = "region")

## ** IMF

## *** API 

library(imfr)

df_imf_ids <- as_tibble(imf_ids())
filter(df_imf_ids, scramblematch("COFOG", description))
filter(df_imf_ids, scramblematch("cofog", description))
filter(df_imf_ids, scramblematch("finance", description))



cofog_codelist <- imf_codelist("GFSCOFOG")

cofog_codelist$codelist


## codes seem to be columns?
## but wouldn't codes be column names in long format?
## example has 'FILR_PA', 'EREER_IX', which are not in imf_codelist("IFS")
## example IFS has code "CL_INDICATOR_IFS", also "CL_INDICATOR_BOP" in r manual,
## also CL_INDICATOR_DOT at https://meshry.com/blog/downloading-data-from-the-imf-api-using-r/
## COFOG doesn't have INDICATOR code, guess that means API is fucking useless
cofog_area <- as_tibble(imf_codes("CL_AREA_GFSCOFOG"))
cofog_sector <- as_tibble(imf_codes("CL_SECTOR_GFSCOFOG"))
cofog_unit <- as_tibble(imf_codes("CL_UNIT_GFSCOFOG"))
cofog_cofog <- as_tibble(imf_codes("CL_COFOG_GFSCOFOG"))

## also using return_raw doesn't make it work reeeeeeeee
cofog_data <- imf_data(
    database_id = "GFSCOFOG",
    indicator = c("GF0802"),
    return_raw = T)

cofog_df <- cofog_data$CompactData$DataSet

cofog_data <- imf_data(
    database_id = "GFSCOFOG",
    indicator = c("GF0602"))
## cofog 



## **** example

imf_codelist("IFS")

imf_codes("CL_INDICATOR_IFS")

ex_interest <- imf_data(database_id = 'IFS',
                        indicator = c('FILR_PA', 'EREER_IX'),
                        freq = 'M')

## maybe should still try to build indexer of all IMF data like for oecd
## could be useful for searching for tax data









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

    imf_culture_wide <- pivot_wider(imf_culture, names_from = COFOG.Function.Code, values_from = value)

    return(imf_culture_wide)
}

## get_imf_data()
    
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
