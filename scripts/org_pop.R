## ** libraries/opening data
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database.xlsx")
## removing header stuff 
nrows <- nrow(df)-1
df <- df[2:nrows,]


# tbl <- table(df$Country)

df$country <- df$"Country where museum is located"

df$name <- df$Museumname
df$year_opened <- df$"Opening year"
df$year_closed <- df$"Closing year"
df$museum_closed <- df$"Museum closed"


## tbl <- table(df$country)
## tbl2 <- tbl[rev(order(tbl))]
## tbl2


df[which(df$country == "USA"),]$country <- "United States"
df[which(df$country == "Missouri"),]$country <- "United States"
df[which(df$country == "England"),]$country <- "United Kingdom"


df$countrycode <- recode(df$country, "United Kingdom" = "GBR", "Spain" = "ESP", "United States" = "USA", "Switzerland" = "CHE" , "India" = "IND", "Greece" = "GRC", "Lebanon" = "LBN", "France" = "FRA", "Estonia" = "EST", "Azerbaijan" = "AZE", "Latvia" = "LVA", "Madagascar" = "MDG", "Indonesia" = "IDN", "Slovakia" = "SVK", "Romania" = "ROU","Argentina" = "ARG","South Korea" = "KOR", "Japan" = "JPN", "Benin" = "BEN", "Bangladesh" = "BGD", "Australia" = "AUS", "Norway" = "NOR", "New Zealand" = "NZL", "Poland" = "POL", "Nigeria" = "NGA", "Portugal" = "PRT", "Serbia" = "SRB","Czech Republic" = "CZE","Senegal" = "SEN", "Puerto Rico" = "PRI", "Taiwan" = "TWN", "Israel" = "ISR", "England" = "GBR", "China" = "CHN", "Germany" = "DEU", "Netherlands" = "NLD", "Italy" = "ITA", "Russia" = "RUS", "Canada" = "CAN", "Hungary" = "HUN", "Belgium" = "BEL", "Sweden" = "SWE", "Finland" = "FIN","Malaysia" = "MYS","Philippines" = "PHL", "Turkey" = "TUR", "Austria" = "AUT", "South Africa" = "ZAF","Thailand" = "THA", "Denmark" = "DNK",  "Mexico" = "MEX", "United Arab Emirates" = "ARE","Brazil" = "BRA", "Hong Kong" = "HKG", "Ukraine" = "UKR", "Kuwait" = "KWT",  "Cyprus" = "CYP", "Monaco" = "MCO", "Iceland" = "ISL", "Kenya" = "KEN", "Singapore" = "SGP", "Iran" = "IRN", "Lithuania" = "LTU", .default= NA_character_)


## ** basic table preparation
## add year: 
## just clean everything by taking first 4, drop everything that doesn't work lmao
df$year_opened_int <- as.integer(lapply(df$year_opened, function(x)(substring(x, 0,4))))
## have 59 NAs, oh well
## let's say for now 1985-2021
# plot(table(df$year_opened_int), type='h')


df$year_opened_int2 <- df$year_opened_int
df$year_opened_int2[which(df$year_opened_int2 < 1985 | df$year_opened_int2 > 2021)] <- NA
## up to 100 dropped atm 

df_open <- na.omit(df[,c('name', 'country', 'countrycode', 'year_opened_int2')])
# plot(table(df_open$year_opened_int2), type='l')
df_open$ctr <- 1

df_country_year_agg <- as_tibble(aggregate(df_open$ctr, by=list(df_open$country, df_open$countrycode, df_open$year_opened_int2), FUN = sum))

names(df_country_year_agg) <- c('country', 'countrycode', 'year', 'nbr_opened')


## make some combinations with countries x years join what I have
df_country_years_empty <- as_tibble(expand.grid(unique(df_open$countrycode), unique(df_open$year_opened_int2)))
names(df_country_years_empty) <- c('countrycode', 'year')
df_country_years_empty$countrycode <- as.character(df_country_years_empty$countrycode)


df_country_years_empty2 <- as_tibble(merge(df_country_years_empty, unique(df_open[,c('country', 'countrycode')]) , by='countrycode', all.x = TRUE))


## join everything nice together
df_country_years <- as_tibble(merge(df_country_year_agg, df_country_years_empty2, by = c('country', 'countrycode', 'year'), all=TRUE))

## fill up NAs with 0s
df_country_years$nbr_opened[which(is.na(df_country_years$nbr_opened))] <- 0


## ** read in some wb gdp data for basic testing
## *** gdp_pcap: gdp per capita
## probably need in long_format
gdp_pcap <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_gpd_pcap/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2916517.csv", header = F))
## no gdp data for 2021 yet, no shit
## actually fine, don't need that data for now: don't have museums opened in 2022 yet

df_gdp_pcap <- gdp_pcap[3:nrow(gdp_pcap),c(1,2,5:ncol(gdp_pcap))]

names(df_gdp_pcap)[3:ncol(df_gdp_pcap)] <- unlist(df_gdp_pcap[1,3:ncol(df_gdp_pcap)])

## melting into long format
df_gdp_pcap_molt <- as_tibble(melt(df_gdp_pcap, id=c('V1', 'V2')))
names(df_gdp_pcap_molt) <- c('country', 'countrycode', 'year', 'gdp_pcap')
print(df_gdp_pcap_molt[which(df_gdp_pcap_molt$country == 'United States'),], n=1000)
df_gdp_pcap_molt <- df_gdp_pcap_molt[which(df_gdp_pcap_molt$country %in% df_country_years$country),]

## need to check completeness: visualization by line
## ehh just do some aggregation

## some NA tests
## df_gdp_pcap_molt_lmt <- df_gdp_pcap_molt[which(df_gdp_pcap_molt$country %in% df_country_years$country),]
## df_gdp_pcap_molt_drop <- na.omit(df_gdp_pcap_molt_lmt)
## max(aggregate(as.integer(as.character(df_gdp_pcap_molt_drop$year)), list(df_gdp_pcap_molt_drop$country), min)$x)


## complete data from 1995 onwards
## can see what kind of different ways I can use to remove NAs:
## remove stuff before 1995
## remove countries with NAs
## maybe I can also have different starting dates per country? idk, will see when i check the method

## *** gini
wb_gini <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/wb_gini/API_SI.POV.GINI_DS2_en_csv_v2_2916486.csv", header = F))

df_wb_gini <- wb_gini[3:nrow(wb_gini), c(1,2,5:ncol(wb_gini))]
names(df_wb_gini)[3:ncol(df_wb_gini)] <- unlist(df_wb_gini[1,3:ncol(df_wb_gini)])
df_wb_gini_molt <- as_tibble(melt(df_wb_gini, id=c('V1', 'V2')))

names(df_wb_gini_molt) <- c('country', 'countrycode', 'year', 'gini')
df_wb_gini_molt <- df_wb_gini_molt[which(df_wb_gini_molt$country %in% df_country_years$country),]
aggregate(gini ~ year, data = df_wb_gini_molt, function(x){sum(is.na(x))}, na.action = NULL)
aggregate(gini ~ country, data = df_wb_gini_molt, function(x){sum(is.na(x))}, na.action = NULL)
## whole bunch of NAs.. should check aggregation to some spell
## will remove many observations tho -> need to have function/systematic

## *** WID
## country codes
library(countrycode)

countrycodes2c <- countrycode(unique(df_country_years$countrycode), "iso3c", "iso2c")
for (code in countrycodes2c){
    print(code)
    cry_data <- as_tibble(read.csv(paste("/home/johannes/Dropbox/phd/papers/org_pop/data/wid/WID_data_", code, ".csv", sep=""), sep=";"))
    }

## variables wanted:
## - income inequality: top 10%
## - income inequality: top 1% share
## - wealth inequality: top 10
## - wealth inequality: top 1%
## - per adult national wealth

## https://wid.world/codes-dictionary/#general-presentation
## unique(unlist(lapply(unique(cry_data$variable), function(x){substr(x,0,1)})))



## *** join predictor data together
# For Inner Join
multi_inner <- as_tibble(Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list(df_wb_gini_molt, df_gdp_pcap_molt)
))
multi_inner
## how the fuck does that work? how does it know what variables to use to join?
## need to check reduce


## ** merge basic opening data with gdp data, add lagged values

df_anls <- as_tibble(merge(df_country_years[,c('countrycode', 'year', 'nbr_opened')],
                           df_gdp_pcap_molt,
                           by=c('countrycode', 'year'),
                           all.x= TRUE))




## lag gdp by a year, also number of openings for good measure, 

df_anls <- df_anls %>% group_by(countrycode) %>% mutate(gdp_pcap_lag1 = lag(gdp_pcap))
df_anls <- df_anls %>% group_by(countrycode) %>% mutate(nbr_opened_lag1 = lag(nbr_opened))


## drop taiwan and other NAs
df_anls <- df_anls[-which(df_anls$countrycode == "TWN"),]
df_anls_omit <- na.omit(df_anls)




## ** checking NAs

aggregate(gdp_pcap ~ countrycode, data = df_anls, function(x){sum(is.na(x))}, na.action = NULL)
## around 10% missing :(
## might have to kick out some countries/years


unique(df_open$countrycode)[which(unique(df_open$countrycode) %!in% (unique(df_gdp_pcap_molt$countrycode)))]
## seems ok,
## taiwan not separate country in WB.. just 1 PM tho, so shouldn't be big impact

## ** PCSE

library(pcse)

found.lm <- lm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1 + as.factor(year), data = df_anls_omit)
summary(found.lm)
screenreg(found.lm)

# necessary to have countrycode as factor
found.pcse <- pcse(found.lm, groupN=factor(df_anls_omit$countrycode), groupT = df_anls_omit$year)
summary(found.pcse)
screenreg(found.pcse)

## what is the interpretation of PCSE?

## ** FE
library(lme4)

## ** negative binomial
## https://rdrr.io/cran/lme4/man/glmer.nb.html

set.seed(101)
dd <- expand.grid(f1 = factor(1:3),
                  f2 = LETTERS[1:2], g=1:9, rep=1:15,
          KEEP.OUT.ATTRS=FALSE)
summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
str(dd)
require("MASS")## and use its glm.nb() - as indeed we have zero random effect:
## Not run: 
m.glm <- glm.nb(y ~ f1*f2, data=dd, trace=TRUE)
summary(m.glm)
m.nb <- glmer.nb(y ~ f1*f2 + (1|g), data=dd, verbose=TRUE)
m.nb
## The neg.binomial theta parameter:
getME(m.nb, "glmer.nb.theta")
LL <- logLik(m.nb)
## mixed model has 1 additional parameter (RE variance)
stopifnot(attr(LL,"df")==attr(logLik(m.glm),"df")+1)
plot(m.nb, resid(.) ~ g)# works, as long as data 'dd' is found










