## * testing wbstats

library(wbstats)

x <- wb_indicators(lang="en")

gdp_vars <- x[which(grepl("gdp", x$indicator, ignore.case = TRUE)),]
gdp_vars$indicator
gdp_vars$indicator_id

filter(gdp_vars, indicator_id == "NY.GDP.MKTP.CD")$indicator_desc
filter(gdp_vars, indicator_id == "NY.GDP.PCAP.CD")$indicator_desc
filter(gdp_vars, scramblematch("PPP", indicator_desc))
filter(gdp_vars, scramblematch("constant", indicator_desc))$indicator_id

## looking for best way to get GDP pcap in constant 2021 USD
filter(gdp_vars, scramblematch("NY.GDP", indicator_id) & scramblematch("constant", indicator)) %>% select(indicator_id, indicator) %>% as.data.frame()

filter(gdp_vars, scramblematch("deflat", indicator)) %>% select(indicator_id, indicator) %>% as.data.frame()


filter(gdp_vars, indicator_id == "NY.GDP.PCAP.PP.CD")$indicator_desc



## try C1.12

indicatorx <- "C1.12"
indicatorx <- "C1.9"
indicatorx <- "1.0.HCount.1.90usd"
indicatorx <- "NY.GDP.MKTP.CD"

z <- wb_data(indicator=indicatorx, start_date=2015, end_date=2000)
y <- wb_data(indicator=indicatorx, mrv=10000)

table(z$date)
table(y$date)

## ** check WB for culture variables, not there
culture_vars <- x[which(grepl("culture", x$indicator, ignore.case = TRUE) & !grepl("agricultur", x$indicator, ignore.case = TRUE)),]
culture_vars$indicator_id
culture_vars$indicator_desc
culture_vars$source

## 9110000
## "FC.XPD.TOUR.CR"

y <- wb_data(indicator = "9110000", start_date = 1985, end_date = 2020) ## not there
y <- wb_data(indicator = "FC.XPD.TOUR.CR", start_date = 1985, end_date = 2020) ## "Indonesia Database for Policy and Economic Research"

## ** check for tourism
tourism_vars <- x[which(grepl("tourism", x$indicator, ignore.case = TRUE)),]
tourism_vars$indicator_desc
tourism_vars$indicator

tourism_df <- get_WB_data(tourism_vars$indicator_id[-c(2)])
summary(tourism_df)

## at least 50% for all of the indicators, but still not too bad, I guess? 


## * getting gdp to work

indicatorx <- "NY.GDP.PCAP.CD"

gdp_api <- wb_data(indicator = indicatorx, start_date = STARTING_YEAR, end_date = ENDING_YEAR)

df_gdp_pcap_molt$year <- as.numeric(as.character(df_gdp_pcap_molt$year))
df_gdp_pcap_molt <- filter(df_gdp_pcap_molt, year > 1984)

gdp_api_min <- gdp_api[,c("iso3c","country", "date", "NY.GDP.PCAP.CD")]
names(df_gdp_pcap_molt) <- c("country", "iso3c", "date", "gdp_pcap")

gdp_cpr <- as_tibble(merge(gdp_api_min, df_gdp_pcap_molt))
gdp_cpr$diff <- gdp_cpr$NY.GDP.PCAP.CD - gdp_cpr$gdp_pcap

table(filter(gdp_cpr, diff > 1000)$country)
table(filter(gdp_cpr, diff > 100)$country)
table(filter(gdp_cpr, diff > 1000)$date)

as.data.frame(filter(gdp_cpr, diff > 1000))

hist(gdp_cpr$diff, breaks = 1000)
mean(gdp_cpr$diff, na.rm=T)

nrow(filter(gdp_cpr, diff < 0.01))/nrow(filter(gdp_cpr, diff < 1e08))

png(paste0(FIG_DIR, "myanmar_twitter.png"), width=800, height=600)
filter(gdp_api, country %in% c("Myanmar", "United States", "Luxembourg")) %>%
    ggplot(aes(x=date, y=NY.GDP.PCAP.CD, group=country, color = country)) +
    geom_line(size=2) +
    theme(legend.text = element_text(size=13))
dev.off()

filter(df_gdp_pcap_molt, country %in% c("Myanmar", "United States", "Germany", "Luxembourg")) %>%
    ggplot(aes(x=date, y=gdp_pcap, group=country, color = country)) +
    geom_line()

## * comparisons
## ** gini
df_wb_gini_molt$year <- as.numeric(as.character(df_wb_gini_molt$year))
filter(df_wb_gini_molt, year > 1984)

gini_cpr_prep <- df_wb_gini_molt
names(gini_cpr_prep) <- c("country", "iso3c", "date", "gini")

gini_cpr <- as_tibble(merge(x[,c("iso3c", "country", "date", "SI.POV.GINI")], gini_cpr_prep))
gini_cpr$diff <- gini_cpr$gini - gini_cpr$SI.POV.GINI

summary(gini_cpr$diff)
hist(gini_cpr$diff)

## gini didn't change at all

## ** population


df_wb_population_molt$year <- as.numeric(as.character(df_wb_population_molt$variable))
filter(df_wb_population_molt, year > 1984)

population_cpr_prep <- df_wb_population_molt[,c("V1", "V2", "year", "value")]
names(population_cpr_prep) <- c("country", "iso3c", "date", "population")

population_cpr <- as_tibble(merge(x[,c("iso3c", "country", "date", "SP.POP.TOTL")], population_cpr_prep))
population_cpr$diff <- population_cpr$population - population_cpr$SP.POP.TOTL

summary(population_cpr$diff)
hist(population_cpr$diff, breaks = 100)

as.data.frame(filter(population_cpr, diff > 10000))



## * old code of sheets

## ** read in some wb gdp data for basic testing
## *** gdp_pcap: gdp per capita
## probably need in long_format
gdp_pcap <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_gpd_pcap/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2916517.csv", header = F))
## no gdp data for 2021 yet, no shit
## actually fine, don't need that data for now: don't have museums opened in 2022 yet

## -1 at the end to have the weird 2021 column gone that is NA everywhere
df_gdp_pcap <- gdp_pcap[3:nrow(gdp_pcap),c(1,2,5:ncol(gdp_pcap)-1)]

names(df_gdp_pcap)[3:ncol(df_gdp_pcap)] <- unlist(df_gdp_pcap[1,3:ncol(df_gdp_pcap)])
## drop first row which has the column names 
df_gdp_pcap <- df_gdp_pcap[-1,] 


## yeeting all the country codes that aren't countries but some larger stuff 
df_gdp_pcap$countrycode_actual <- countrycode(df_gdp_pcap$V2, "wb", "wb")
df_gdp_pcap <- df_gdp_pcap[-which(is.na(df_gdp_pcap$countrycode_actual)),]
## yeet the selection column
df_gdp_pcap2 <- df_gdp_pcap[,-which(names(df_gdp_pcap) %in% c("countrycode_actual", "Indicator Code"))]


## melting into long format
df_gdp_pcap_molt <- as_tibble(melt(df_gdp_pcap2, id=c('V1', 'V2')))
names(df_gdp_pcap_molt) <- c('country', 'countrycode', 'year', 'gdp_pcap')
## print(df_gdp_pcap_molt[which(df_gdp_pcap_molt$country == 'United States'),], n=1000)
## df_gdp_pcap_molt <- df_gdp_pcap_molt[which(df_gdp_pcap_molt$country %in% df_country_years$country),]

## XKX: Kosovo not included in iso3c, gets solved when using worldbank schema
## countrycode(unique(df_gdp_pcap_molt$countrycode), "wb", "country.name")

## testing of ambiguous country codes
## ambig_country_codes2 <- c("AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU", "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC", "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD")
## as.data.frame(df_gdp_pcap[which(df_gdp_pcap$V2 %in% ambig_country_codes2),c("V1", "V2")])


## need to check completeness: visualization by line
## ehh just do some aggregation

## some NA tests
## df_gdp_pcap_molt_lmt <- df_gdp_pcap_molt[which(df_gdp_pcap_molt$country %in% df_country_years$country),]
## df_gdp_pcap_molt_drop <- na.omit(df_gdp_pcap_molt_lmt)
## max(aggregate(as.integer(as.character(df_gdp_pcap_molt_drop$year)), list(df_gdp_pcap_molt_drop$country), min)$x)

## *** gini
wb_gini <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_gini/API_SI.POV.GINI_DS2_en_csv_v2_2916486.csv", header = F))

df_wb_gini <- wb_gini[3:nrow(wb_gini), c(1,2,5:ncol(wb_gini)-1)]
names(df_wb_gini)[3:ncol(df_wb_gini)] <- unlist(df_wb_gini[1,3:ncol(df_wb_gini)])
df_wb_gini <- df_wb_gini[-1,]
## more effective way of yeeting all the "countries" that aren't countries
df_wb_gini2 <- df_wb_gini[which(df_wb_gini$V2 %in% countrycode(unique(df_wb_gini$V2), "wb", "wb")),]

df_wb_gini_molt <- as_tibble(melt(df_wb_gini2[,-3], id=c('V1', 'V2')))

names(df_wb_gini_molt) <- c('country', 'countrycode', 'year', 'gini')
## df_wb_gini_molt <- df_wb_gini_molt[which(df_wb_gini_molt$country %in% df_country_years$country),]
aggregate(gini ~ year, data = df_wb_gini_molt, function(x){sum(is.na(x))}, na.action = NULL)
aggregate(gini ~ country, data = df_wb_gini_molt, function(x){sum(is.na(x))}, na.action = NULL)
## whole bunch of NAs.. should check aggregation to some spell
## will remove many observations tho -> need to have function/systematic

## *** population
wb_population <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_population/API_SP.POP.TOTL_DS2_en_csv_v2_3158886.csv", header = F))
df_wb_population <- wb_population[3:nrow(wb_population), c(1,2,5:ncol(wb_population)-1)]

names(df_wb_population)[3:ncol(df_wb_population)] <- unlist(df_wb_population[1,3:ncol(df_wb_population)])
df_wb_population <- df_wb_population[-1,]
## more effective way of yeeting all the "countries" that aren't countries
df_wb_population2 <- df_wb_population[which(df_wb_population$V2 %in% countrycode(unique(df_wb_population$V2), "wb", "wb")),]

df_wb_population_molt <- as_tibble(melt(df_wb_population2[,-3], id=c('V1', 'V2')))

names(df_wb_population_molt) <- c('country', 'countrycode', 'year', 'population')

aggregate(population ~ year, data = df_wb_population_molt, function(x){sum(is.na(x))}, na.action = NULL)
aggregate(population ~ country, data = df_wb_population_molt, function(x){sum(is.na(x))}, na.action = NULL)
## population seems good, some missing stuff for Gaza/Kuwait, nobody cares


## * check GDP missing values
indx <- c("NY.GDP.PCAP.CD", "SP.POP.TOTL")
x <- get_WB_data(indx)

## look at gdp gaps -> compare with WID: most GDP gaps are also WID gaps -> not really worth looking for more data

cvrg_gdp <- x %>% filter(year >= 1995) %>%
    group_by(iso3c) %>% 
    mutate(nbr_nas = sum(is.na(NY.GDP.PCAP.CD))) %>%
    filter(nbr_nas > 0) %>%
    select(iso3c, nbr_nas) %>% 
    summarize(nbr_nas = mean(nbr_nas)) %>% arrange(nbr_nas) %>% adf() %>%
    mutate(countryname = countrycode(iso3c, "wb", "country.name"))

lapply(gsub("pct_cutoff", "hnwi_nbr", hnwi_names), \(x)
       cpltns_checker(df_reg[,c("iso3c", "year", x)], varx = x)) %>%
    rbindlist()

cvrg_wid <- df_reg %>% select(iso3c, year, hnwi_nbr_30M) %>%
    filter(year >= 1995) %>% 
    group_by(iso3c) %>%
    summarize(nbr_nas_hnwi = sum(is.na(hnwi_nbr_30M))) %>%
    filter(nbr_nas_hnwi > 0)

merge(cvrg_gdp, cvrg_wid) %>% arrange(nbr_nas)


## other approach
filter(df_reg, is.na(NY.GDP.PCAP.CD) & !is.na(hnwi_nbr_30M)) %>% select(country, year) %>% adf()
## hmm 57 country years, and mostly with countries that existed then... Somalia, Afghanistan, Vuvuzela, Liberia

## * year combinations

df_reg[,c("iso3c", "year", rel_lngtd_vars)] %>%
    pivot_longer()

na_nbr_df <- (1-is.na(df_reg[rel_lngtd_vars])) %>% atb()

na_nbr_df$id_cbn <- apply(na_nbr_df, 1, \(x) paste(x, collapse = "-"))
na_nbr_df$sum <- apply(na_nbr_df[rel_lngtd_vars], 1, sum)

table(na_nbr_df$id_cbn) %>% sort()

## hmm
## question is if a country-year is included in combination
## need to generate all combinations first 
## country-years are nested: if cy has variable a, and also variable a and b, i want it has part of a-b variable set
## can pick the highest combination: but that's what I already have with id_cbn 

## now don't have the total count of a combination: 939 have at most, but don't know how many have 0-0-0-0-0-1-1


vrbl_cnbs <- list(
    all_vars = rel_lngtd_vars,
    no_cult_spending = rel_lngtd_vars[rel_lngtd_vars != "smorc_dollar_fx"],
    no_cult_spend_and_mitr = rel_lngtd_vars[rel_lngtd_vars %!in% c("smorc_dollar_fx", "tmitr_approx_linear_2020step")],
    controls = c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm"))

    
