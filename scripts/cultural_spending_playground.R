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

## *** oecd table  11 

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

ggplot(get_oecd_table11(), aes(x=year, y=pct_value)) +
    geom_line() +
    facet_wrap(~iso3c, scales = "free")


## *** oecd table 11 archive

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

## cult_cbn <- merge(df_cult %>% mutate(smorc = 1),
##                   df_euro_fltrd %>% mutate(euro=1), all = T) %>% atb()
       
## filter(cult_cbn, is.na(smorc), euro==1, year >= 1995) %>% adf()
## has 2020 data, UN apparently doesn't? only for handful of countries

## ** source comparison

sort_by_priority <- function(dfx, priority_vec, pos) {
    #' pick all obs that adhere to priority_vec[pos], but only if not also matched by higher priority
    
    prty_vlu <- priority_vec[pos]
    vlus_to_disregard <- priority_vec[0:(pos-1)]

    dfx %>% group_by(iso3c, year) %>%
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


    df_tlycg_fltrd <- lapply(seq_along(source_priority), \(pos) sort_by_priority(
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


## i think it's not feasible to maintain source: p3cg and tlycg often come from different sources 
df_cult_cbn <- rbind(df_p3cg_fltrd, df_tlycg_fltrd) %>% atb()

df_cult_impute <- df_cult_cbn %>% 
    select(iso3c, year, format, measure, value) %>% 
    pivot_wider(names_from = format)

df_cult_usd <- filter(df_cult_impute, measure == "constant_usd")
df_cult_pct <- filter(df_cult_impute, measure == "pct_value")

## df_cult_impute %>%
##     mutate(diff1 = p3cg/tlycg,
##            diff2 = tlycg/p3cg) %>%
##     summarize(diff1_median = median(diff1, na.rm = T),
##               diff2_median = median(diff2, na.rm = T),
##               diff1_mean = mean(diff1, na.rm = T),
##               diff2_mean = mean(inf.omit(diff2), na.rm = T))

## *** JointAI imputation
library(JointAI)

df_cult_pct <- df_cult_pct %>%
    arrange(iso3c, year) %>% 
    group_by(iso3c) %>%
    mutate(p3cg_lag1 = dplyr::lag(p3cg, 1),
           p3cg_lag2 = dplyr::lag(p3cg, 2),
           tlycg_lag1 = dplyr::lag(tlycg, 1),
           tlycg_lag2 = dplyr::lag(tlycg, 2))

jointai_test <- lme_imp(formula = tlycg ~ p3cg + p3cg_lag1 + p3cg_lag2 + (1 | iso3c), data = adf(df_cult_pct),
                        monitor_params = c(imps = T),
                        n.iter = 500)


plot(jointai_test)
traceplot(jointai_test)
summary(jointai_test)
coef(jointai_test)

names(jointai_test)


df_imptd <- get_MIdat(jointai_test, m=30, minspace = 5) %>% atb()
table(df_imptd$Imputation_)


imptd_info <- filter(df_imptd, Imputation_ == 0) %>%
    mutate(tlycg_imptd = ifelse(is.na(tlycg), T, F)) %>%
    select(iso3c, year, tlycg_imptd)

imptd_means <- df_imptd %>% group_by(iso3c, year) %>%
    filter(Imputation_ > 0) %>% 
    summarize(value = mean(tlycg)) %>%
    mutate(source = "joint_ai")

imptd_means <- merge(imptd_means, imptd_info) %>% atb()


p3cg_to_tlycg_scalar <- df_cult_impute %>% mutate(scaler = tlycg/p3cg) %>%
    pull(scaler) %>% inf.omit() %>% median(na.rm = T)
imptd_scalar <- df_cult_pct %>%
    mutate(tlycg_pred = ifelse(is.na(tlycg), p3cg*p3cg_to_tlycg_scalar, tlycg),
           tlycg_imptd = ifelse(is.na(tlycg), T, F)) %>%
    select(iso3c, year, value = tlycg_pred, tlycg_imptd) %>%
    mutate(source = "scalar_imputation")

p3cg_data <- df_cult_pct %>% select(iso3c, year, value = p3cg) %>%
    mutate(tlycg_imptd = F, source = "p3cg") %>% na.omit()

Reduce(\(x,y) rbind(x,y), list(imptd_means, imptd_scalar, p3cg_data)) %>% atb() %>% 
    group_by(iso3c) %>% 
    mutate(any_imptd = any(tlycg_imptd)) %>%
    filter(any_imptd) %>% 
    ggplot(aes(x=year, y=value, color = interaction(source, tlycg_imptd)))  + 
    geom_line() +
    geom_point() +
    facet_wrap(~iso3c, scales = "free")



df_imptd_cbn <- merge(df_imptd %>% select(iso3c, year, Imputation_, tlycg_imp_vlu = tlycg),
      df_cult_pct %>% mutate(tlycg_imptd = ifelse(is.na(tlycg), T, F))) %>% atb()

filter(df_imptd_cbn, tlycg_imptd)

summary(df_imptd_cbn$tlycg_imp_vlu)
## still NAs??? at least not everywhere, but seems like one "batch" didn't produce imputed values

df_imptd_cbn %>%
    group_by(iso3c) %>% 
    mutate(any_imptd = any(tlycg_imptd)) %>%
    filter(any_imptd) %>%
    ggplot(aes(x=year, y=tlycg_imp_vlu, group = Imputation_, color = tlycg_imptd)) +
    geom_line() +
    ## geom_point() +
    facet_wrap(~iso3c, scales = "free")



## df_imptd %>% filter(iso3c %in% c("DEU", "USA", "FRA", "ITA", "CHN")) %>%
##     ggplot(aes(x=year, y=tlycg, color = factor(Imputation_))) +
##     geom_line() +
##     geom_jitter() +
##     facet_wrap(~iso3c, scales = "free")



## *** mice imputation
library(mice)

df_cult_usd_wide <- df_cult_usd %>% select(iso3c, year, p3cg, tlycg) %>%
    filter(year >= 2010) %>% 
    pivot_wider(values_from = c(p3cg, tlycg), names_from = year)

apply(df_cult_usd_wide, 2, \(x) sum(is.na(x)))
apply(df_cult_usd_wide, 1, \(x) sum(is.na(x)))


mice_test = mice(df_cult_usd_wide, method = "lasso.norm")

## *** scaler imputation 2


select_proper_tlycg_series <- function(df_cult_cbn) {
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

    ## get countries where series overlap at least at some points to calculate country-specific scalars
    crys_ovlp <- filter(df_cult_sclr, nbr_formats_c == 2, nbr_formats_cy_max == 2)
    crys_ovlp_scaler <- crys_ovlp %>%
        group_by(iso3c) %>%
        summarize(scaler_cry = median(inf.omit(scaler), na.rm = T)) %>% na.omit()

    
    ## add the country-level scalar of those countries that have them 
    df_cult_sclr_cbn <- merge(df_cult_sclr, crys_ovlp_scaler, all.x = T) %>% atb()

    ## compute the different tlycg values 
    df_cult_sclr_cbn2 <- df_cult_sclr_cbn %>%
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
    df_tlycg_fnl <- lapply(seq_along(tlycg_priority_vec), \(pos) sort_by_priority(dfx = df_cult_sclr_cbn3,
                                                                                  priority_vec = tlycg_priority_vec,
                                                                                  pos = pos)) %>%
        Reduce(\(x,y) rbind(x,y), .)

    return(df_tlycg_fnl)
}



## df_tlycg_fnl %>%
##     ## filter(iso3c %in% c("RUS", "SMR")) %>% 
##     ggplot(aes(x=year, y=value)) +
##     geom_line() +
##     geom_point(aes(color = source)) +
##     facet_wrap(~iso3c, scales = "free")




## each cy just has one, but overall series has multiple -> yeet them lol
## or rather, impute tlycg with general scaler_med
## crys_interrupt <- filter(df_cult_sclr, nbr_formats_c == 2, nbr_formats_cy == 1, nbr_formats_cy_max == 1)

## crys_interrupt %>%
##     filter(measure == "constant_usd") %>%
##     pivot_longer(cols = c(p3cg, tlycg), names_to = "format") %>%
##     ggplot(aes(x=year, y=value, group = format, color = format)) +
##     geom_line() +
##     geom_jitter() +
##     facet_wrap(~iso3c, scales = "free")






## *** virgin scaler imputation 



df_cult_impute_scaler <- df_cult_impute %>% mutate(scaler = tlycg/p3cg) %>% 
    filter(scaler < 10 & scaler > -10)

df_cult_impute_scaler %>% 
    ggplot(aes(x=scaler, fill=measure)) +
    geom_histogram() +
    facet_wrap(~measure, ncol = 1)
    
filter(df_cult_impute_scaler) %>% pull(scaler) %>% mean()



p3cg_to_tlycg_scalar <- df_cult_impute %>% mutate(scaler = tlycg/p3cg) %>%
    pull(scaler) %>% inf.omit() %>% median(na.rm = T)

df_cult_usd2 <- df_cult_usd %>%
    mutate(tlycg_pred = ifelse(is.na(tlycg), p3cg*p3cg_to_tlycg_scalar, tlycg),
           tlycg_imptd = ifelse(is.na(tlycg), T, F))



df_cult_usd2 %>% group_by(iso3c) %>%
    mutate(nbr_sources = len(unique(tlycg_imptd))) %>%
    filter(nbr_sources > 1) %>%
    ## filter(tlycg_imptd) %>% 
    ggplot(aes(x=year, y=tlycg_pred, color = tlycg_imptd)) +
    geom_point() +
    facet_wrap(~iso3c, scales = "free")



## *** virgin manual regression imputation 

glm(p3cg ~ tlycg + factor(iso3c), data = df_cult_usd)

lm_tlycg_usd <- screenreg(lm(tlycg ~ p3cg, filter(df_cult_impute, measure == "constant_usd")))
lm_p3cg_usd <- lm(p3cg ~ tlycg, filter(df_cult_impute, measure == "constant_usd"))

lm_tlycg_pct <- lm(tlycg ~ p3cg, filter(df_cult_impute, measure == "pct_value"))
lm_p3cg_pct <- screenreg(lm(p3cg ~ tlycg, filter(df_cult_impute, measure == "pct_value")))


df_cult_usd$pred <- predict.lm(lm_p3cg_usd, newdata = df_cult_usd)

df_cult_usd %>% filter(pred < 0)


## hmm why only 828?
## had not dropped source before -> pivot_wider would create additional columns 
## when source dropped, I get 1189 (previously 1193 in impute choice)

filter(impute_choice_df, tlycg==1, p3cg==1)
filter(df_cult_impute, measure == "constant_usd")
filter(df_cult_impute, measure == "pct_value", !is.na(p3cg), !is.na(tlycg))
filter(df_cult_impute, measure == "pct_value", is.na(p3cg), !is.na(tlycg))
filter(df_cult_impute, measure == "pct_value", !is.na(p3cg), is.na(tlycg))
## nrow between pct_value and constant_usd are not the same reeeeeeeeeeeeeeeee
## is because in get_un_df some pre-1995 CYs have no observation for GDP in current LCU (df_wb), should be ok tho:
## pct value is just for checking plausibility




## *** manual checks 



p3cg_mismatch_t11a_dis <- c("CHE", "DEU", "ITA", "LVA", "PRT", "SWE")
p3cg_mismatch_un_dis <- c("CHL", "NLD", "SVN")
p3cg_mismatch_t11_dis <- c("EST", "IRL", "JPN", "KOR")
p3cg_mismatch_crys <- c(p3cg_mismatch_t11a_dis, p3cg_mismatch_un_dis, p3cg_mismatch_t11_dis)


## look at countries manually visually identified as mismatching 
df_cprn %>% filter(format == "p3cg", measure == "pct_value", iso3c %in% p3cg_mismatch_crys) %>%
    ## pivot_wider(names_from = source) %>% na.omit() %>%
    ## pivot_longer(cols = c(un, oecd_table11, oecd_table11_arc), names_to = "source") %>% 
    ggplot(aes(x=year, y=value, group=source, color = source)) +
    geom_line() +
    geom_jitter() + 
    facet_wrap(~iso3c, scales = "free")

## choose which p3cg series to pick in case of disagreement -> oecd_table11 always turns out to  be preferable 
p3cg_choices <- list(
    c("CHE", "oecd_table_11"),
    c("CHL", "oecd_table_11"),
    c("DEU", "oecd_table_11"),
    c("EST", "oecd_table_11"),
    c("IRL", "oecd_table_11"),
    c("ITA", "oecd_table_11"), ## t11 is lower, but difference not huge 
    c("JPN", "oecd_table_11"),
    c("KOR", "oecd_table_11"),
    c("LVA", "oecd_table_11"),
    c("SWE", "oecd_table_11"))
## actually always use oecd_table 11 -> can automate it

## compare with ratio, not that many countries
df_cprn %>% filter(format == "p3cg", measure == "pct_value") %>%
    pivot_wider(names_from = source) %>% na.omit() %>%
    mutate(ratio = oecd_table11/un) %>%
    filter(ratio < 0.9 | ratio > 1.1) %>%
    pull(iso3c) %>% table()



## see how large the coverage of each variable combination is 
impute_choice_df <- merge(    
    df_cprn %>% filter(format == "p3cg") %>%
    group_by(iso3c, year) %>%
    summarize(p3cg = 1),
    df_cprn %>% filter(format == "tlycg") %>%
    group_by(iso3c, year) %>%
    summarize(tlycg = 1), all = T) %>% atb()


filter(impute_choice_df, is.na(p3cg), tlycg==1) %>% nrow() ## 351 have no p3cg (those not covered by UN)
filter(impute_choice_df, is.na(tlycg), p3cg==1) %>% nrow() ## 925 have no tlycg (those only covered by UN)
filter(impute_choice_df, tlycg==1, p3cg==1) %>% nrow() ## 1223 have both: for training

filter(df_cprn, iso3c %in% c("DEU", "FRA", "ITA", "JPN", "KOR"), measure == "pct_value") %>%
    ggplot(aes(x=year, y=value, color = interaction(format, source), linetype = format)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    facet_wrap(~iso3c, scales = "free")


df_cprn_wide_usd <- filter(df_cprn, measure == "constant_usd") %>%
    pivot_wider(names_from = source, values_from = value)

df_cprn_wide_pct <- filter(df_cprn_long, name == "pct_value") %>%
    pivot_wider(names_from = source, values_from = value)

df_cprn_wide_usd %>% select(all_of(unique(df_cprn_long$source))) %>%  chart.Correlation()
    ## cor(use = "pairwise.complete.obs")
## %>% ggcorrplot(method = "circle", type = "lower")

df_cprn_wide_pct %>% select(all_of(unique(df_cprn_long$source))) %>% chart.Correlation()
## %>%  ggcorrplot(method = "circle", type = "lower")

x <- get_un_data()
y <- get_imf_data()
filter(df_cprn_wide_pct,un > 2) %>% adf()


df_cprn_long %>% filter(name == "constant_usd", source %in% all_of(c("eurostat", "imf", "un"))) %>%
    ggplot(aes(x=year, y=value, color = source, group = source)) +
    geom_line() +
    facet_wrap(~iso3c, scales = "free")



## *** look at diffs:
df_cprn_wide_pct %>%
    mutate(diffx = un-imf) %>%
    filter(!is.na(diffx)) %>%
    select(iso3c, year, un, imf, diffx) %>%
    pivot_longer(cols = c(un, imf, diffx)) %>%
    ggplot(aes(x=year, y=value, color = name, group = name)) +
    geom_line() +
    facet_wrap(~iso3c, scales = "free")
## maybe it's just pct calculations, and amounts are actually the same? do after lunch

cpr_usd_minus <- df_cprn_wide_usd %>%
    mutate(diffx = imf-un) %>%
    filter(!is.na(diffx)) %>%
    select(iso3c, year, un, imf, diffx) %>%
    pivot_longer(cols = c(un, imf, diffx))


ggplot(cpr_usd_minus, aes(x=year, y=value, color = name, group = name)) +
    geom_line() +
    geom_hline(yintercept = 0) + 
    facet_wrap(~iso3c, scales = "free") 
    



cpr_usd_ratio <- df_cprn_wide_usd %>%
    select(iso3c, year, un, imf) %>% na.omit() %>%
    mutate(diffx = un/imf,
           region = countrycode(iso3c, "iso3c", "un.region.name"))

viz_lines(cpr_usd_ratio, y="diffx", facets = "region", duration = 1, max_lines = 4)

hist(cpr_usd_ratio$diffx, breaks = 40)



xtsum(cpr_usd, diffx, iso3c)
## more between difference (0.22) than within (0.14), but that's still kinda similar


            







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

## *** compare original values
## JPN

cur_df_jpn <- filter(cur_df, iso3c == "JPN", year >= 2005)
df_cprn_jpn <- filter(df_cprn_long, name =="constant_usd", iso3c == "JPN")

data_jpn <- list(
    c(2005, 1515.0),
    c(2006, 1498.3),
    c(2007, 1485.7),
    c(2008, 1405.3),
    c(2009, 1361.8),
    c(2010, 1355.1),
    c(2011, 1378.2),
    c(2012, 1360.2),
    c(2013, 1361.4),
    c(2014, 1506.2),
    c(2015, 1568.7),
    c(2016, 1624.2),
    c(2017, 1637.0),
    c(2018, 1683.8),
    c(2019, 1676.1),
    c(2020, 1654.9)) %>% do.call(rbind, .) %>% adf()
names(data_jpn) <- c("year", "value")
data_jpn$source <- "mof"



data_jpn <- list(
    c(2015, 1532.6),
    c(2014, 1477.3),
    c(2013, 1343.2),
    c(2012, 1346.3),
    c(2011, 1362.7),
    c(2010, 1346.1),
    c(2009, 1356.2),
    c(2008, 1395.3),
    c(2007, 1475.6),
    c(2006, 1491.3),
    c(2005, 1509.6)) %>% do.call(rbind, .) %>% adf()
names(data_jpn) <- c("year", "value")
data_jpn$source <- "mof"

df_jpn <- merge(data_jpn, cur_df_jpn)

df_jpn <- df_jpn %>% mutate(usd_constant = ((value*1e9)/inyixx999i)/xlcusx999i_2021,
                    name = "constant_usd") %>%
    select(iso3c, year, source, name, value = usd_constant)


oecd_table11 <- atb(read.csv(paste0(OECD_DATA_DIR, "SNA_TABLE11")))


oecd_cpoents_jpn <- filter(oecd_table11, LOCATION == "JPN", SECTOR == "GS13", ObsValue > 1000000) %>%
    select(iso3c = LOCATION, year = Time, source = TRANSACT, value = ObsValue)

oecd_cpoents_jpn2 <- merge(oecd_cpoents_jpn, cur_df_jpn) %>%
    mutate(value = ((value*1e6)/inyixx999i)/xlcusx999i_2021,
           name = "constant_usd") %>%
    select(iso3c, year, source, name, value) %>% atb()

Reduce(\(x,y) rbind(x,y), list(df_cprn_jpn, df_jpn, oecd_cpoents_jpn2)) %>%
    ggplot(aes(x=year, y=value, color = source, shape = source)) +
    geom_line() +
    geom_point(size=5)

## un_df3 <- construct_gvt_consumption_expenditure()
## filter(un_df3, iso3c == "JPN", year >= 2005, Series == 1000)

    
    ggplot(aes(x=Time, y=ObsValue, group = TRANSACT, color = TRANSACT)) +
    geom_line()

## *** try figuring out with regression the relation among the components 

oecd_reg <- filter(oecd_table11,  SECTOR == "GS13") %>%
    select(iso3c = LOCATION, year=Time, value = ObsValue, TRANSACT) %>%
    pivot_wider(names_from = TRANSACT)

screenreg(lm(TLYCG ~ P3CG, data = oecd_reg))

 + D3CG + D62_D631XXCG + P2_D29D5D8CG + D7CG, data = oecd_reg))




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

## ** old way of selecting the most appropriate series, functionalized with `sort by priority`


df_p3cg_source_choice <- df_cprn %>% filter(format == "p3cg", iso3c != "CHL" | (iso3c=="CHL" & source != "un")) %>%
    group_by(iso3c, year) %>% 
    mutate(oecd_t11_there = ifelse("oecd_table11" %in% source, T, F),
           un_there = ifelse("un" %in% source, T, F),
           oecd_t11_arc_there = ifelse("oecd_table11_arc" %in% source, T, F))


df_p3cg_cbn <- rbind(Reduce(\(x,y) rbind(x,y),
             list(
                 filter(df_p3cg_source_choice, oecd_t11_there, source == "oecd_table11") %>%
                 select(iso3c, year, source, format, measure, value),
                 filter(df_p3cg_source_choice, !oecd_t11_there & un_there, source == "un") %>%
                 select(iso3c, year, source, format, measure, value),
                 filter(df_p3cg_source_choice, !oecd_t11_there & !un_there & oecd_t11_arc_there,
                        source == "oecd_table11_arc") %>%
                 select(iso3c, year, source, format, measure, value)))) %>% atb()


df_p3cg_cpr <- merge(df_p3cg_fltrd, df_p3cg_cbn, all.x = T) %>% atb() %>%
    mutate(diff = value2 - value)
min(df_p3cg_cpr$diff)
max(df_p3cg_cpr$diff)

## ## check CY counts, seems to add up 
## df_p3cg_cbn %>% group_by(iso3c, year) %>%
##     summarize(x=1)
## df_p3cg_source_choice %>% group_by(iso3c, year) %>%
##     summarize(x=1)
