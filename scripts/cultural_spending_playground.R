## * cultural_spending_playground

## ** older OECD data sources

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




## ** substitution


df_cult_wide <- df_cult %>%
    ## filter(!scramblematch("UN_SMOrc", Item)) %>% ## uncomment to exclude SMOrc
    na.omit() %>%
    pivot_wider(names_from = Item, values_from = Value)
    
df_cult_wide$some_val <- 1
cpltns_checker(df_cult_wide, "some_val")


## *** getting vars: ask on SO

## **** ask on SO
## https://stackoverflow.com/questions/72055576/select-set-of-columns-so-that-each-row-has-at-least-one-non-na-entry#72055576

df_cult_wide_optim <- df_cult_wide[,3:ncol(df_cult_wide)]


best <- function(df){
    best <- which.max(colSums(sapply(df, complete.cases)))
    while(any(rowSums(sapply(df[best], complete.cases)) == 0)){
        
        best <- c(best, which.max(sapply(df[apply(is.na(df[best]), 1, all), ],  \(x) sum(complete.cases(x)))))
    }
    best
}

best_vars <- best(df_cult_wide_optim)




## pull(`UN_SMOrc Recreation, culture and religion`)

## most of the work seems to be done by UN_SMOrc Recreation, culture and religion

## yup seems to work: 
## most numbers of NAs are 6, while best is 7 vars

## hmm need to check more for how many countries I have only one variable

df_cult_wide$nbr_nas <- apply(is.na(df_cult_wide[names(best_vars)]),1,sum)

filter(df_cult_wide, nbr_nas == 6) %>% select(names(best_vars)) %>%
    is.na() %>% apply(2, \(x) len(x) - sum(x))

## assess overall coverage of best_vars
select(df_cult_wide, names(best_vars)) %>%
    apply(2, is.na) %>% apply(2, \(x) len(x) - sum(x))

## find too other indicators that have largest overlap
filter(df_cult_wide_optim, !is.na(`UN_SMOrc Recreation, culture and religion`)) %>%
    apply(2, is.na) %>% apply(2, \(x) len(x) - sum(x)) %>% sort(decreasing = T) %>% enframe()
## fuck coverage not good



## *** imf correlation check
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

## ** add currencies test (with unrelated variable)
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

filter(un_df_cur_caution, )


## ** check integration of data sources


df_cult <- as_tibble(Reduce(function(x,y) rbind(x,y),
                            list(
                                gen_ilo_df(),
                                combine_un_series(),
                                get_imf_data())))

df_cult <- gen_cult_spending()

## *** imf table  11 

filter_sdmx_results("recreation") %>%
    filter(!scramblematch("agricult", label.en), scramblematch("SNA", sdmx_id)) %>% adf()

datasets_already_there <- list.files(OECD_DATA_DIR)

download_oecd_dataset("SNA_TABLE11", "080")
download_oecd_dataset("SNA_TABLE11_ARCHIVE", "080")

get_oecd_table11 <- function() {
    #' get the oecd 

    
    oecd_table11 <- atb(read.csv(paste0(OECD_DATA_DIR, "SNA_TABLE11")))
    ## table(oecd_table11$SECTOR)
    ## table(oecd_table11$TRANSACT)
    ## table(oecd_table11$UNIT)
    ## table(oecd_table11$POWERCODE)

    df_oecd_fltrd <- filter(oecd_table11,
                            TRANSACT == "TLYCG", # total gvt expenditure 
                            SECTOR == "GS13") %>% # general government 
        mutate(value = ObsValue * 10^POWERCODE) %>% 
        select(iso3c = LOCATION, year = Time,  currency = UNIT, value)

    df_oecd_mrgd <- merge(df_oecd_fltrd, cur_df, all.x = T) %>% atb() %>%
        merge(df_wb, all.x = T) %>% atb() %>%
        mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
               constant_usd = (value/inyixx999i)/xlcusx999i_2021, # hope it makes sense (Blanchet_2017_conversions)
               source = "oecd_table11")
               
    
    ## df_oecd_mrgd <- merge(df_oecd_fltrd,
    ##                       select(df_wb, iso3c, year, NY.GDP.MKTP.CN), all.x = T) %>% atb() %>%
    ##     mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
    ##            source = "oecd_table11")

    ## return(select(df_oecd_mrgd, iso3c, year, pct_value, source))
    return(select(df_oecd_mrgd, iso3c, year, constant_usd, pct_value, source))
}

ggplot(get_oecd_table11(), aes(x=year, y=pct_value)) +
    geom_line() +
    facet_wrap(~iso3c, scales = "free")


## *** imf table 11 archive

get_oecd_table11_archive <- function() {
    #' generate the oecd table 11 archive data

    
    oced_table11_arc <- atb(read.csv(paste0(OECD_DATA_DIR, "SNA_TABLE11_ARCHIVE")))

    oecd_table11_arc_fltrd <- filter(oced_table11_arc, SECTOR == "GS13", TRANSACT == "TLYCG") %>%
        mutate(value = ObsValue*10^POWERCODE) %>%
        select(iso3c = LOCATION, year = Time,  currency = UNIT, value)

    ## filter out colombia in not COP: WID seems to assume COP 
    oecd_table11_arc_fltrd <- filter(oecd_table11_arc_fltrd, iso3c != "COL" | (iso3c == "COL" & currency == "COP"))

    oecd_table11_arc_mrgd <- merge(oecd_table11_arc_fltrd, cur_df, all.x = T) %>% atb() %>% 
        merge(df_wb, all.x = T) %>% atb() %>%
        mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
               constant_usd = (value/inyixx999i)/xlcusx999i_2021, # hope it makes sense (Blanchet_2017_conversions)
               source = "oecd_table11_arc")


    ## oecd_table11_arc_mrgd <- merge(oecd_table11_arc_fltrd,
    ##                                select(df_wb, iso3c, year, NY.GDP.MKTP.CN), all.x = T) %>% atb() %>%
    ##     mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
    ##            region = countrycode(iso3c, "iso3c", "un.region.name"),
    ##            source = "oecd_table11_arc")
    
    ## viz_lines(oecd_table11_arc_mrgd, y="pct_value", facets = "region")
    ## looks somewhat plausible

    return(select(oecd_table11_arc_mrgd, iso3c, year, constant_usd, pct_value,  source))

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

    un_df_cbn <- merge(un_df_clpsd, cur_df, all.x = T) %>% atb() %>%
        merge(., df_wb) %>% atb() %>%
        mutate(constant_usd = (value/inyixx999i)/xlcusx999i_2021,
               pct_value = (value/NY.GDP.MKTP.CN)*100,
               source = "un") %>%
        select(iso3c, year, constant_usd, pct_value, source)
    
    return(un_df_cbn) 

}



    
    


viz_lines(x, y="pct_value", duration = 1, facets = "iso3c")

whack_crys <- c("BEL", "CHL", "CUW", "DEU", "ECU", "HRV", "HUN", "IND", "IRL", "MAC", "MDA", "MLT", "MWI", "SVK", "SVN", "SWE")
mult_series_crys <- c("JPN", "GBR", "GRL", "DNK", "MNG", "PAN")
weird_series_crys <- c("JPN", "GRL", "PAN")

data_japan <- list(
    get_un_data() %>% filter(iso3c == "JPN"), # just focus on japan anyways 
    get_oecd_table11() %>% filter(iso3c %in% all_of(weird_series_crys)),
    get_oecd_table11_archive() %>% filter(iso3c %in% all_of(weird_series_crys)),
    get_imf_data() %>% filter(iso3c %in% all_of(weird_series_crys)),
    get_eurostat() %>% filter(iso3c %in% all_of(weird_series_crys))) %>%
    Reduce(\(x,y) rbind(x,y), .) %>% atb()



ggplot(data_japan, aes(x=year, y=constant_usd, color = source)) +
    geom_line()

filter(x, iso3c %in% all_of(whack_crys)) %>% 
    viz_lines(y="constant_usd", duration = 1, facets = "iso3c")

x <- get_un_data()
filter(x, pct_value > 10)

## viz_lines(get_oecd_table11_archive(), y="pct_value", facets = "iso3c", duration = 1)


## ggplot(get_oecd_table11_archive(), aes(x=year, y=pct_value)) +
##     geom_line() +
##     facet_wrap(~iso3c, scales = "free")

## *** imf

imf_df <- get_imf_data()

## cult_cbn <- merge(df_cult %>% mutate(smorc = 1),
##                   filter(imf_df, Item == "IMF_GF08") %>% mutate(imf = 1), all=T) %>% atb()
                  
## filter(cult_cbn, is.na(smorc), imf==1, year >= 1995)
## ## 340 huuuuuuuuuuuuu nice

## *** eurostat
get_eurostat <- function() {
    #' generate the eurostat data
    


    df_eurostat <- atb(read.csv(paste0(PROJECT_DIR, "data/eurostat/gov_10a_exp__custom_2811962_page_linear.csv")))

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
               source = "eurostat") %>%
        select(iso3c, year, constant_usd, pct_value = PC_GDP, source)
               
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

## cult_cbn <- merge(df_cult %>% mutate(smorc = 1),
##                   df_euro_fltrd %>% mutate(euro=1), all = T) %>% atb()
       
## filter(cult_cbn, is.na(smorc), euro==1, year >= 1995) %>% adf()
## has 2020 data, UN apparently doesn't? only for handful of countries

## ** source comparison


df_cprn <- atb(Reduce(\(x,y) rbind(x,y),
                      list(
                          get_un_data(),
                          get_oecd_table11(),
                          get_oecd_table11_archive(),
                          get_imf_data(),
                          get_eurostat())))

df_cprn_long <- df_cprn %>% pivot_longer(cols = c(constant_usd, pct_value))

df_cprn_wide_usd <- filter(df_cprn_long, name == "constant_usd") %>%
    pivot_wider(names_from = source, values_from = value)


df_cprn_wide_pct <- filter(df_cprn_long, name == "pct_value") %>%
    pivot_wider(names_from = source, values_from = value)

df_cprn_wide_usd %>% select(all_of(unique(df_cprn_long$source))) %>%  chart.Correlation()
    ## cor(use = "pairwise.complete.obs")
## %>% ggcorrplot(method = "circle", type = "lower")

df_cprn_wide_pct %>% select(all_of(unique(df_cprn_long$source))) %>% cor(use = "pairwise.complete.obs")
## %>%  ggcorrplot(method = "circle", type = "lower")





## compare OECD (final consumption expenditure) and UN (government final consumption expenditure)
oecd_p3cg <- filter(oecd_table11, TRANSACT == "P3CG", SECTOR == "GS13") %>%
    mutate(value = ObsValue * 10^POWERCODE) %>% 
    select(iso3c = LOCATION, year = Time,  currency = UNIT, value)

df_oecd_p3cg <- merge(oecd_p3cg, 
                      select(df_wb, iso3c, year, NY.GDP.MKTP.CN), all.x = T) %>% atb() %>%
    mutate(pct_value = (value/NY.GDP.MKTP.CN)*100,
           source = "oecd_p3cg") %>%
    select(iso3c, year, pct_value, source)

df_cprn2 <- rbind(
    df_cult %>% mutate(source = "un") %>% select(iso3c, pct_value = pct_fx, year, source),
    df_oecd_p3cg) %>% atb()

df_cprn2_wide <- pivot_wider(df_cprn2, names_from = source, values_from = pct_value)
cor(df_cprn2_wide$un, df_cprn2_wide$oecd_p3cg, use = "complete.obs")
cor(df_cprn_wide$un, df_cprn_wide$oecd_table11, use = "complete.obs")

## *** plotting time
sample_crys <- sample(unique(df_cprn$iso3c), 40)



df_cprn_vis <- df_cprn_wide %>% na.omit() %>% pivot_longer(cols = unique(df_cprn$source))

p_cult_spend_sources <-  ggplot(df_cprn_vis, aes(x=year, y=value, group=name, color = name, shape = name)) +
    geom_point(size = 1)+ 
    geom_line() +
    facet_wrap(~iso3c, scales = "free")
                                                        
pdf(paste0(FIG_DIR, "cult_spending_sources.pdf"), width = 12, height = 9)
plot(p_cult_spend_sources)
dev.off()
    
df_cprn_vis %>% group_by(name) %>% summarize(mean = mean(value))




ggplot(filter(df_cprn, iso3c %in% all_of(sample_crys)), aes(x=year, y=pct_value, group=source, color = source)) +
    geom_line() +
    facet_wrap(~iso3c, scales = "free")
                                                        
       
## ** data, japan

japan_mof <- list(
    c(2005, 352.7),
    c(2004, 352.0),
    c(2003, 356.3),
    c(2002, 372.0),
    c(2001, 383.3),
    c(2000, 369.0),
    c(1999, 363.1),
    c(1998, 357.0),
    c(1997, 353.1),
    c(1996, 332.1),
    c(1995, 308.9),
    c(1994, 295.2),
    c(1993, 281.3),
    c(1992, 261.6),
    c(1991, 241.9),
    c(1990, 218.0)) %>% do.call(rbind, .) %>% adf()
names(japan_mof) <- c("year", "value")
