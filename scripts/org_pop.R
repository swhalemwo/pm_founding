library("readxl")
library(tibble)
library(reshape2)

df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database.xlsx")
## removing header stuff 
nrows <- nrow(df)-1
df <- df[2:nrows,]


tbl <- table(df$Country)

df$country <- df$"Country where museum is located"

df$name <- df$Museumname
df$year_opened <- df$"Opening year"
df$year_closed <- df$"Closing year"
df$museum_closed <- df$"Museum closed"


tbl <- table(df$country)
tbl2 <- tbl[rev(order(tbl))]
tbl2


df[which(df$country == "USA"),]$country <- "United States"
df[which(df$country == "Missouri"),]$country <- "United States"


## ** basic table preparation
## just clean everything by taking first 4, drop everything that doesn't work lmao
df$year_opened_int <- as.integer(lapply(df$year_opened, function(x)(substring(x, 0,4))))
## have 59 NAs, oh well
## let's say for now 1985-2021
# plot(table(df$year_opened_int), type='h')


df$year_opened_int2 <- df$year_opened_int
df$year_opened_int2[which(df$year_opened_int2 < 1985 | df$year_opened_int2 > 2021)] <- NA
## up to 100 dropped atm 

df_open <- na.omit(df[,c('name', 'country', 'year_opened_int2')])
# plot(table(df_open$year_opened_int2), type='l')
df_open$ctr <- 1

df_country_year_agg <- as_tibble(aggregate(df_open$ctr, by=list(df_open$country, df_open$year_opened_int2), FUN = sum))
names(df_country_year_agg) <- c('country', 'year', 'nbr_opened')




## make some combinations with countries x years join what I have
df_country_years_empty <- as_tibble(expand.grid(unique(df_open$country), unique(df_open$year_opened_int2)))
names(df_country_years_empty) <- c('country', 'year')

## join everything nice together
df_country_years <- as_tibble(merge(df_country_year_agg, df_country_years_empty, by = c('country', 'year'), all=TRUE))

## fill up NAs with 0s
df_country_years$nbr_opened[which(is.na(df_country_years$nbr_opened))] <- 0



## ** read in some wb gdp data for basic testing
## gdp_pcap: gdp per capita
## probably need in long_format
gdp_pcap <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_gpd_pcap/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2916517.csv", header = F))

df_gdp_pcap <- gdp_pcap[3:nrow(gdp_pcap),c(1,2,5:ncol(gdp_pcap))]

names(df_gdp_pcap)[3:ncol(df_gdp_pcap)] <- unlist(df_gdp_pcap[1,3:ncol(df_gdp_pcap)])

## melting into long format
df_gdp_pcap_molt <- as_tibble(melt(df_gdp_pcap, id=c('V1', 'V2')))
names(df_gdp_pcap_molt) <- c('country', 'countrycode', 'year', 'gdp_pcap')
print(df_gdp_pcap_molt[which(df_gdp_pcap_molt$country == 'United States'),], n=1000)

## need to check completeness: visualization by line
## ehh just do some aggregation

df_gdp_pcap_molt_lmt <- df_gdp_pcap_molt[which(df_gdp_pcap_molt$country %in% df_country_years$country),]
df_gdp_pcap_molt_drop <- na.omit(df_gdp_pcap_molt_lmt)
max(aggregate(as.integer(as.character(df_gdp_pcap_molt_drop$year)), list(df_gdp_pcap_molt_drop$country), min)$x)
## complete data from 1995 onwards
## can see what kind of different ways I can use to remove NAs:
## remove stuff before 1995
## remove countries with NAs
## maybe I can also have different starting dates per country? idk, will see when i check the method

## ** merge basic opening data with gdp data

df_anls <- as_tibble(merge(df_country_years, df_gdp_pcap_molt, by=c('country', 'year'), all.x= TRUE))
na_agg_cnt <- aggregate(df_anls$gdp_pcap, list(df_anls$country), is.na)

aggregate(gdp_pcap ~ country, data = df_anls, function(x){sum(is.na(x))}, na.action = NULL)
## crappy names not the same







