## * cultural_spending_playground

## ** exploring UN dataframes
## *** exploration of all UN dfs related to cultural spending

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

## *** exploration of output_gross_value_added_fixed_assests_industry_cur_prices


## but also so many different things

## table(un_df2$Country.or.Area)
## table(un_df2$Item) %>% sort()




## ** compare OECD and WID exchange rates, when using market exchange rates (xlcusx999i) for WID they basically completely match

## use PPP rates tho LOL

## OECD
## oecd_fx_df <- filter_sdmx_results("exchange rates")
## unique(oecd_fx_df$description.en)

## oecd_str <- "Real effective exchange rates"
## oecd_str <- "Foreign effective weighted exchange rates"
## oecd_str <- "EXCH"
## oecd_str <- "ppp"

## filter(oecd_fx_df, description.en == oecd_str)

## download_oecd_dataset("CSPCUBE", "INTLCOMP_T1")
## download_oecd_dataset("CSP2012", "INTLCOMP_T1")
## download_oecd_dataset("CSP2012", "INTLCOMP")
## ## downloads not working properly 
## ## EO97_OUTLOOK97: economic outlook 2015
## ## tbh would be interesting to see the impact of predictions

## maybe just use the website?
## https://data.oecd.org/conversion/exchange-rates.htm

oecd_cur_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/OECD/fx_rates.csv")))
oecd_cur_df <- select(oecd_cur_df, iso3c=LOCATION, year = TIME, oecd_fx = Value)

filter(oecd_cur_df, iso3c == "DEU") %>%
    ggplot(aes(x=year, y=oecd_fx)) +
    geom_line()
 
## comparison

cur_df_cpr <- as_tibble(merge(
    wid_currency_df,
    oecd_cur_df))
   
cor(cur_df_cpr$xlcusx999i, cur_df_cpr$oecd_fx)

cur_df_sum <- group_by(cur_df_cpr, iso3c) %>%
    summarize(corx= cor(xlcusx999i, oecd_fx))

hist(cur_df_sum$corx, breaks = 20)

filter(cur_df_sum, corx > 0.9) 
filter(cur_df_sum, corx < 0.7)
## maybe can use same UN data source to get GDP? and

cur_df_cpr$region <- countrycode(cur_df_cpr$iso3c, "iso3c", "un.region.name")
cur_df_cpr <- create_facets(cur_df_cpr, "region", "iso3c", 5)

pdf(paste0(FIG_DIR, "fx_comparison_WID_OECD.pdf"), width = 17, height = 10)
cur_df_cpr %>%
    group_by(iso3c) %>%
    mutate(corx = cor(xlcusx999i, oecd_fx)) %>%
    ## filter(corx < 0.8) %>%
    pivot_longer(cols=c("xlcusx999i", "oecd_fx")) %>%
    ggplot(aes(x=year, y=value, color = iso3c, linetype = name)) +
    facet_wrap(~iso3c, scales = "free") +
    geom_line()
dev.off()


## ** more sophisticated UN series resolution attempts, abandoned in favor of brainlet strats (Smorc/conservative)

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

## *** abstraction of series comparison 

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

## use "s" prefix for the series values to have them as strings to have them nicer to edit

un_df2$Series_m <- paste0("s", un_df2$Series)

series_labels <- paste0("s", na.omit(unique(un_df2$Series)))
names(series_labels) <- series_labels

un_df2$one <- 1

## construct series for each df
## uses namesseries_one as identifier for merging

series_data <- lapply(series_labels, function(x)
    filter(un_df2, Series_m == x) %>%
    select(iso3c, year, Series, Item, Value, !!paste0("one", x) := one))

## series_data$`100`

as_tibble(merge(series_data$s100, series_data$s200))

series_combns <- as.data.frame(t(combn(names(series_data), m=2)))

## combn_dfs <- rbindlist(apply(series_combns, 1, function(x)
##     list(x1 = x['V1'],
##          x2 = x['V2'],
##          ovlp = nrow(merge(series_data[[x['V1']]][,c("iso3c", "year", "Item")],
##                            series_data[[x['V2']]][,c("iso3c", "year", "Item")])))))

## says no overlap, but i had overlap before in ISL -> i'm tired of your fucking lying R
## probably due to inclusion of value column?
## hmm doesn't seem so
##  was actually due to inclusion of Series column -> yeeted


## *** correlation calculations

## wonder if I have to focus on the relative size of the overlap between two series?

series_cprr <- function(s1, s2) {
    #' compares two series

    df1 <- series_data[[s1]]
    df2 <- series_data[[s2]]

    n1 <- nrow(df1)
    n2 <- nrow(df2)

    df_joint <- as_tibble(merge(
        mutate(df1, Value1=Value) %>%
        select(iso3c, year, Item, Value1),
        mutate(df2, Value2=Value) %>%
        select(iso3c, year, Item, Value2)))

    n_joint = nrow(df_joint)

    nj_crys <- len(unique(df_joint$iso3c))
    nj_vars <- len(unique(df_joint$Item))
    nj_time <- len(unique(df_joint$year))

    corx = cor(df_joint$Value1, df_joint$Value2)

    ## maybe add some more nuanced correlation: per item, or per item*country
    ## var_cors <- df_joint %>%
    ##     group_by(Item) %>%
    ##     summarize(corx = cor(Value1, Value2), lenx = len(Value1)) %>%
    ##     filter(lenx > 10)

    return(list(
        s1 = s1,
        s2 = s2,
        n1 = n1,
        n2 = n2,
        n_joint = n_joint,
        corx = corx,
        nj_crys =nj_crys,
        nj_vars = nj_vars,
        nj_time = nj_time))
}

## series_cprr("s1000", "s1100")

combn_dfs <- rbindlist(apply(series_combns, 1, function(x) series_cprr(x['V1'], x['V2'])))
    
## *** plotting series overlaps
library(igraph)

g <- graph_from_data_frame(combn_dfs, directed = F)

plot(g, edge.label = E(g)$n_joint, edge.width = E(g)$n_joint/100, title = "asdf")

g.copy <- delete.edges(g, which(E(g)$n_joint == 0))

pdf(paste0(FIG_DIR, "UN_series_plot.pdf"), width = 8, height=6)
plot(g.copy, edge.label = E(g.copy)$n_joint, edge.width = E(g.copy)$n_joint/100, vertex.size=30,
     main = "UN series overlap in country-year-variables")
dev.off()

## still seems impossible to construct variables in tidyverse calls
## construct one-variable and pivot wider before filtering each series?
    
## actually works now (https://stackoverflow.com/questions/56162309/new-column-from-string-in-dplyr):
un_df2 %>% mutate(!!paste0("dd", "jj") := 100)


## *** comparison visualization
## pick s1000 and s1100: have most overlap

df_join <- as_tibble(merge(
    select(series_data[['s1000']], iso3c, year, Item, Value1 = Value),
    select(series_data[['s1100']], iso3c, year, Item, Value2 = Value))) %>%
    pivot_longer(cols = c("Value1", "Value2"))


pdf(paste0(FIG_DIR, "UN_series_1000_1100_comparison.pdf"), width = 18, height = 10)
ggplot(df_join, aes(x=year, y=value, color = iso3c, linetype = name)) +
    facet_wrap(~interaction(iso3c, substring(Item, 1, 30)), scales = "free") + 
    geom_line() +
    labs(title = "comparison of series 1000 and 1100 for countries and variables where both have data (UNdata_output_gross_value_added_fixed_assests_industry_cur_prices)")
dev.off()


## color by country
## facet by variable?


## *** series transition

## see how it looks like when series change

sample_lines_id <- un_df2 %>%
    group_by(iso3c, Item) %>%
    summarize(nbr_series = len(unique(Series))) %>%
    filter(nbr_series > 1) %>%
    ungroup() %>%
    select(iso3c, Item) %>%
    sample_n(30)
## for some reason it merges a whole lot of extra rows, need to use unique
sample_lines_data <- as_tibble(unique(merge(select(un_df2, iso3c, year, Series),
                                            sample_lines_id)))
pdf(paste0(FIG_DIR, "UN_series_ovlp.pdf"), width = 18, height=10)
## ggplot(sample_lines_data, aes(x = year, y=interaction(Series, Item, iso3c), fill = factor(Series))) +
ggplot(sample_lines_data, aes(x = year, y=factor(Series), fill = factor(Series))) +    
    facet_wrap(~interaction(Item, iso3c), scales = "free") + 
    geom_tile() +
    labs(title = "comparison of series coverage for sample of 30 country-variables which have more than one series (UNdata_output_gross_value_added_fixed_assests_industry_cur_prices)")
dev.off()

    ## ungroup() %>%
    ## count(nbr_series)

## ** diagnostics from reading in un_df2
un_df2_wide <- select(un_df2, iso3c, year, Item, Value, Series) %>%
        filter(Item != "") %>%
        pivot_wider(names_from = Item, values_from = Value)


    un_df2_cur <- un_df2 %>%
        group_by(iso3c, year) %>%
        mutate(nbr_curs = len(unique(Currency)))

    table(filter(un_df2_cur, nbr_curs > 1)$iso3c)

un_df2_cur %>%
    ungroup %>%
    filter(nbr_curs > 1) %>%
    select(iso3c, Currency)%>%
    unique()

table(un_df2_cur$nbr_curs)

filter(un_df2_cur, nbr_curs > 1) %>%
    select(iso3c, year, Item, Currency) %>%
    as.data.frame()

## ** misc 


## want to see which countries have which variables on multiple series
## 

## hmm using only 1000 series decreases number by quite something

## un_df2_wide$ttl <- rowsum
    


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
## ** IMF API fails, just bulk download

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


