## ** libraries/opening data
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)
library(lme4)
library(texreg)
library(ggplot2)
library(countrycode)
library(stargazer)
library(gridExtra)
library(parallel)
library(RClickhouse)
library(docstring)
library(DBI)
ds <- docstring


options(show.error.messages = TRUE)
options(show.error.locations = TRUE)

'%!in%' <- function(x,y)!('%in%'(x,y))
len <- length

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

## ** looking into how messy variables can be sanitized automatically, little success so far
df$"Collection genre focus"
summary(df$"Collection genre focus")
table(is.na(df$"Collection genre focus"))

table(is.na(df$"Floor size"))

df$"Floor size"[!is.na(df$"Floor size")]

df$floor_size <- df$"Floor size"

df[which(df$floor_size == "NA"),]$floor_size <- NA

table(df$floor_size)

df$collection_genre_focus <- df$"Collection genre focus"

df$activities <- df$"Educational / outreach / social / artistic programs"
strsplit(df$activities, split=c(",|:|;"))
strsplit(df$activities[553], ',')
lapply(df$activities, strsplit, split=c(","))

strsplit("asdf,jk;l:l", c(",|;|:"))

strsplit2 <- function(x, min_len){
    chars <- c()
    ## for (i in (nchar(x)-min_len -1):nchar(x)){
    for (end in min_len:nchar(x)){
        chars <- c(chars, substring(x, 1:(nchar(x)-min_len +1), end))
    }
    return(unique(chars[which(lapply(chars, nchar) >= min_len)]))
}
         
strsplit2("abcdefg", 3)
## strsplit2(df$activities[553], 3)[4728]
## x <- df$activities[553]


df$mission <- df$"Mission / vision"
hist(unlist(lapply(df$mission, nchar)), breaks = 40)

library(topicmodels)
library(tm)

vector_source <- VectorSource(df$mission)
corpus <- Corpus(vector_source)

review_corpus = tm_map(corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
## review_corpus = tm_map(review_corpus, removeWords, c("art", "museum", "contemporary", "collection", "artists", "exhibitions", "works"))
review_corpus = tm_map(review_corpus, removeWords, c(stopwords("german")))
review_corpus =  tm_map(review_corpus, stripWhitespace)

dtm <- DocumentTermMatrix(review_corpus)
## dropping documents with 0 terms
dtm <- dtm[which(apply(dtm, 1, sum) !=0),]

lda_res <- LDA(dtm, k=2)

library(tidytext)
lda_topics <- tidy(lda_res, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

## top_terms %>%
##   mutate(term = reorder_within(term, beta, topic)) %>%
##   ggplot(aes(beta, term, fill = factor(topic))) +
##   geom_col(show.legend = FALSE) +
##   facet_wrap(~ topic, scales = "free") +
##   scale_y_reordered()


beta_wide <- lda_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide2 <- rbind(beta_wide[order(beta_wide$log_ratio)[c(1:10)],],
                    beta_wide[rev(order(beta_wide$log_ratio))[c(1:10)],])

beta_wide2 <- beta_wide2[order(beta_wide2$log_ratio),]
beta_wide2$term <- factor(beta_wide2$term, levels = beta_wide2$term[order(beta_wide2$log_ratio)])

## ggplot(beta_wide2, aes(x=term, y=log_ratio)) +
##     geom_bar(stat="identity") +
##     coord_flip()





## ** set static vars
FIG_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/figures/"
TABLE_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/tables/"
STARTING_YEAR <- 1985
WID_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/wid/"
WID_FILES <- list.files(WID_DIR)

## ** custom funcs


## https://stats.stackexchange.com/questions/123366/lmer-standardized-regression-coefficients
lm.beta.lmer <- function(mod) {
    ## extract standardized effects
   b <- fixef(mod)[-1]
   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
   sd.y <- sd(getME(mod,"y"))
   b*sd.x/sd.y
}


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


## complete data from 1995 onwards
## can see what kind of different ways I can use to remove NAs:
## remove stuff before 1995
## remove countries with NAs
## maybe I can also have different starting dates per country? idk, will see when i check the method

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




## *** WID

## **** setting up CH
library(countrycode)

countrycodes2c <- na.omit(countrycode(unique(df_gdp_pcap_molt$countrycode), "iso3c", "iso2c"))

con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

countrycodes3c <- na.omit(unique(df_gdp_pcap_molt$countrycode))

## issues with CHI and XKX
## need to check what they actually refer in WB data
## then probably write some manual exceptions so that they get correctly translated from WB to WDI
## also need to check whether there are other mismatches between iso2c(wb) and WDI -> paste WDI (https://wid.world/codes-dictionary/#country-code)

## **** check country codes compatibility between WID and WB 
df_crycd <- unique(df_gdp_pcap_molt[,c("country", "countrycode")])
df_crycd$iso2c <- countrycode(df_crycd$countrycode, "wb", "iso2c")


wid_crycds <- as_tibble(read.csv(paste0(WID_DIR, "countrycodes.csv"), sep = "\t", header = F))[,c("V1", "V2")]
names(wid_crycds) <- c("countrycode", "wid_cry")

## idk why this doesn't work, dfs are all string values, should be just fine
df_crycd_mrg <- as_tibble(merge(wid_crycds, df_crycd, by.x = "countrycode", by.y = "iso2c"))
## manual inspection
as.data.frame(df_crycd_mrg[which(df_crycd_mrg$wid_cry != df_crycd_mrg$country),])
## results: WID and WB data seem to be in agreement: possible to use iso2c of WB codes for WID
## just need manual exceptions for Kosovo and Channel Islands

## only re-read CH data when especially asking for it

## **** read into CH
READ_IN_CH <- FALSE
if (READ_IN_CH == TRUE){

    for (code in countrycodes3c){
        ## manual exceptions for channel 
        if (code == "XKX") {
            cry_code2c <- "KV"
        } else if (code == "CHI"){
            cry_code2c <- "XI"
        } else {
            cry_code2c <- countrycode(code, "iso3c", "iso2c")
        }
        
        print(code)
        
        filename <- paste0("WID_data_", cry_code2c, ".csv")

        if (filename %in% WID_FILES){

            cry_data <- as_tibble(read.csv(paste(WID_DIR, "WID_data_", cry_code2c, ".csv", sep=""), sep=";"))

            cry_data$country <- code
            cry_data$varx <- substring(cry_data$variable, 2, 6)
            cry_data$first_letter <- substring(cry_data$variable, 1,1)

            ## only write schema with first table, otherwise append
            if (code == "ABW"){
                DBI::dbWriteTable(con, "wdi", cry_data)
            }
            else {
                DBI::dbWriteTable(con, "wdi", cry_data, append=TRUE)
                
            }
        }
    }

}
print("done")

## cry_rel_vars_df <- filter(cry_data, varx == "labsh" | varx == "wealp" | varx == "wealg")
## cry_dfs[[code]] <- cry_rel_vars_df


## percentile table
## pctl_tbl <- table(cry_data$percentile)
## pctl_tbl[rev(order(pctl_tbl))]

## ggplot(filter(cry_data, varx=="wealg"), aes(x=year, y=value)) +
##     geom_line()


## *** join predictor data together
## For Inner Join
multi_inner <- as_tibble(Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list(df_wb_gini_molt, df_wb_population_molt, df_gdp_pcap_molt)
))
## how the fuck does that work? how does it know what variables to use to join?
## need to check reduce


multi_inner$year <- as.numeric(as.character(multi_inner$year))
multi_inner <- multi_inner[which(multi_inner$year >= STARTING_YEAR),]

filter(multi_inner, is.na(population))
## population only missing for stuff where I don't have gdp/gini data anyways


## ** basic DV table preparation
## add year: 
## just clean everything by taking first 4, drop everything that doesn't work lmao
df$year_opened_int <- as.integer(lapply(df$year_opened, function(x)(substring(x, 0,4))))
## have 59 NAs, oh well
## let's say for now 1985-2021
# plot(table(df$year_opened_int), type='h')



df$year_opened_int2 <- df$year_opened_int
df$year_opened_int2[which(df$year_opened_int2 < STARTING_YEAR | df$year_opened_int2 > 2021)] <- NA
## up to 100 dropped atm 

df_open <- na.omit(df[,c('name', 'country', 'countrycode', 'year_opened_int2')])
# plot(table(df_open$year_opened_int2), type='l')
df_open$ctr <- 1


df_country_year_agg_cnt <- as_tibble(aggregate(ctr ~ countrycode + year_opened_int2, df_open, FUN = sum))
names(df_country_year_agg_cnt) <- c('countrycode', 'year', 'nbr_opened')

## df_country_year_agg_names <- as_tibble(aggregate(name ~ countrycode + year_opened_int2, df_open, c))

## df_country_year_agg_names <- as_tibble(aggregate(name ~ countrycode + year_opened_int2, df_open, function(x){paste(x, collapse = "----")}))

df_country_year_agg_names <- df_open %>% group_by(countrycode, year_opened_int2) %>% summarise(name = list(name))


names(df_country_year_agg_names) <- c("countrycode", "year", "name")

df_country_year_agg <- as_tibble(merge(df_country_year_agg_cnt, df_country_year_agg_names))



## ** merge basic opening data with gdp data

df_anls <- as_tibble(merge(multi_inner, df_country_year_agg,
                           by=c('countrycode', 'year'),
                           all.x= TRUE))

## fill up NAs up with 0s
df_anls$nbr_opened[which(is.na(df_anls$nbr_opened))] <- 0

df_anls$wv <- 0

df_anls$gdp_pcapk <- df_anls$gdp_pcap/1000

## check if WB and my country codes are the same, they are 
## x <- merge(df_gdp_pcap2[,c("V1", "V2")],
##       unique(df_country_year_agg[,c("country", "countrycode")]),
##       by.x = c("V2"),
##       by.y = c("countrycode"))
## x[which(x$V1 != x$country),]

## cumulative number of opened
df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$countrycode, FUN = cumsum)
df_anls$nbr_opened_cum_sqrd <- (df_anls$nbr_opened_cum^2)/100
## have to divide by 100 otherwise R glmer.nb complains


## PMs opened per 1m people -> rate
df_anls$nbr_opened_prop <- df_anls$nbr_opened/(df_anls$population/1000000)

## iceland, monaco, cyprus LUL
filter(df_anls, nbr_opened_prop > 1)




## ** directions of trade
## *** exploring completeness
## **** across years
## compare degree of coverage across years

con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

pairs_1985 <- as_tibble(dbGetQuery(con, "SELECT DISTINCT(CONCAT(toString(country_code), '-', toString(counterpart_country_code))) AS pair from dots_prep WHERE time_period='1985'"))

pairs_2020 <- as_tibble(dbGetQuery(con, "SELECT DISTINCT(CONCAT(toString(country_code), '-', toString(counterpart_country_code))) AS pair from dots_prep WHERE time_period='2020'"))

unique_pairs_1985 <- pairs_1985[which(pairs_1985$pair %!in% pairs_2020$pair),]
sample(unique_pairs_1985$pair, 200)
## seems mostly countries that don't exist anymore in 2020: USSR, East Germany, Czechoslovakia

tbl_imf <- table(countrycode(unlist(strsplit(unique_pairs_1985$pair, '-')), 'imf', 'country.name'))
tbl_imf[rev(order(tbl_imf))][c(0:30)]

## **** country coded completeness check: done 

## hmm a bunch of country codes don't get clearly matched
## maybe i can group by country_code and counterpart_country_code rather than by my string stuff
## also easier to which countrycodes produce mistakes in conversion

unq_crys1 <- as_tibble(dbGetQuery(con, "SELECT tpl.1 AS country_name, tpl.2 AS country_code FROM (SELECT DISTINCT(country_name, country_code) AS tpl FROM dots_prep GROUP BY country_name, country_code)"))

unq_crys2 <- as_tibble(dbGetQuery(con, "SELECT tpl.1 AS country_name, tpl.2 AS country_code FROM (SELECT DISTINCT(counterpart_country_name, counterpart_country_code) AS tpl FROM dots_prep GROUP BY counterpart_country_name, counterpart_country_code)"))

unq_crys <- unique(rbind(unq_crys1, unq_crys2))
unq_crys$conversion <- countrycode(unq_crys$country_code, "imf", "country.name")

imf_list <- apply(unq_crys, 1, function(x) c(x))
imf_list <- unq_crys$country_name
names(imf_list) <- unq_crys$country_code

as.data.frame(unq_crys[which(is.na(unq_crys$conversion)),])
## conversion problems mostly about regional groupings/associations (USSR, world, Emerging countries, Asia not specified) etc

as.data.frame(unq_crys[which(!is.na(unq_crys$conversion)),c("country_name", "conversion")])
## conversion between imf countrycode and name seems to work well 

## **** type of time period: done

## also need to compare coverage across time intervals


dots_time_cprn <- function(year){
    pairs_yx_qs <- as.data.frame(matrix(ncol = 2, nrow = 0))
    names(pairs_yx_qs) <- c("ccd", "ctrccd")

    print(year)

    for (i in seq(4)){
        cmd <- "SELECT tpl.1 AS ccd, tpl.2 AS ctrccd FROM (SELECT  Distinct(country_code, counterpart_country_code) AS tpl from dots_prep where time_period = '{year}Q{i}' group by country_code, counterpart_country_code)"

        pairs_yx_qx <- as_tibble(dbGetQuery(con, glue(cmd)))
        pairs_yx_qs <- rbind(pairs_yx_qs, pairs_yx_qx)
    }
    
    
    pairs_yx_qs <- unique(pairs_yx_qs)

    year_cmd <- "SELECT tpl.1 AS ccd, tpl.2 AS ctrccd FROM (SELECT  Distinct(country_code, counterpart_country_code) AS tpl from dots_prep where time_period = '{year}' group by country_code, counterpart_country_code)"
    pairs_yx <- as_tibble(dbGetQuery(con, glue(year_cmd)))

    names(pairs_yx) <- names(pairs_yx_qs)
    
    return(list(
        year=year,
        yearly_pairs= nrow(pairs_yx),
        quarter_pairs= nrow(pairs_yx_qs),
        yearly_unique= nrow(setdiff(pairs_yx, pairs_yx_qs)),
        quarterly_unique= nrow(setdiff(pairs_yx_qs, pairs_yx)),
        common_pairs= nrow(union(pairs_yx_qs, pairs_yx))))
}

dots_cpltns_res <- lapply(seq(1985,2020), dots_time_cprn)

dots_cpltns_df <- do.call(rbind.data.frame, dots_cpltns_res)
sum(dots_cpltns_df$yearly_unique)
sum(dots_cpltns_df$quarterly_unique)
## using yearly data seems ok, quarterly has slightly more unique, but could easily be that coverage is not complete for all quartiles
## could check but don't think there's much need at this stage
## could also rewrite function to save df R objects, but really no need atm





## *** computation

## figuring out meaning

vlu_cmd <- "SELECT * from dots 
where ((country_code='134' and counterpart_country_code='138') 
or (country_code='138' and counterpart_country_code='134')) and year=2020"

dots_test <- as_tibble(dbGetQuery(con, vlu_cmd))

as.data.frame(dots_test)

dots_test[,c("country_name", "counterpart_country_name", "indicator_code", "value")]

(filter(dots_test, country_name == "Germany" & indicator_code == "TXG_FOB_USD")$value - filter(dots_test, country_name == "Germany" & indicator_code == "TMG_CIF_USD")$value)/filter(dots_test, country_name == "Germany" & indicator_code == "TBG_USD")$value


vlu_cmd2 <- "SELECT country_code, counterpart_country_code, concat(toString(country_code), '-', toString(counterpart_country_code)) AS link1, concat(toString(counterpart_country_code), '-', toString(country_code)) AS link2, year, indicator_code, value FROM dots WHERE year='2020'"

fob_cif_cprn <- as_tibble(dbGetQuery(con, vlu_cmd2))

fob_df <- filter(fob_cif_cprn, indicator_code=="TXG_FOB_USD")[,c("country_code", "counterpart_country_code", "year", "value")]
names(fob_df) <- c("country_code", "counterpart_country_code", "year", "fob_value")
cif_df <- filter(fob_cif_cprn, indicator_code=="TMG_CIF_USD")[,c("country_code", "counterpart_country_code", "year", "value")]
## somehow need to change direction,
## try first with renaming
## then with link columns
names(cif_df) <- c("counterpart_country_code", "country_code", "year", "cif_value")

fob_cif_cprn2 <- as_tibble(merge(fob_df, cif_df))

dots_test[,c("country_name", "counterpart_country_name", "indicator_code", "value")]

filter(fob_cif_cprn2, (country_code==134 & counterpart_country_code==138) |  (country_code==138 & counterpart_country_code==134))
## huh NL imports more to DE than DE to NL, pattern also elsewhere: 
## https://tradingeconomics.com/netherlands/exports/germany
## https://tradingeconomics.com/germany/exports/netherlands
## numbers are off tho




fob_cif_cprn2$diff <- fob_cif_cprn2$fob_value - fob_cif_cprn2$cif_value
fob_cif_cprn2$diff_log <- log(fob_cif_cprn2$fob_value) - log(fob_cif_cprn2$cif_value)
hist(fob_cif_cprn2$diff_log, breaks = 1000)
summary(fob_cif_cprn2$diff)
hist(fob_cif_cprn2$diff, breaks = 1000)
boxplot(fob_cif_cprn2$diff, breaks = 500)

iqr <- IQR(fob_cif_cprn2$diff)
Q <- quantile(fob_cif_cprn2$diff, probs = c(0.25, 0.75))
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated <- subset(fob_cif_cprn2, fob_cif_cprn2$diff > (Q[1] - 1.5*iqr) & fob_cif_cprn2$diff < (Q[2]+1.5*iqr))
hist(eliminated$diff, breaks = 50)

## wide range of FOB and CIF diffs, most in ones/tens of millions, but some also hundreds of billions



## largest ones:



## first exclude regions/entities that don't translate properly, it's mostly regional groupings that don't make much sense anyways
fob_cif_cprn2$country_name <- countrycode(fob_cif_cprn2$country_code, 'imf', 'country.name')
fob_cif_cprn2$counterpart_country_name <- countrycode(fob_cif_cprn2$counterpart_country_code, 'imf', 'country.name')

fob_cif_cprn2_naomit <- na.omit(fob_cif_cprn2)
hist(fob_cif_cprn2_naomit$diff, breaks = 500)

top_diffs <- c(order(fob_cif_cprn2_naomit$diff)[c(1:15)], rev(order(fob_cif_cprn2_naomit$diff))[c(1:15)])
as.data.frame(fob_cif_cprn2_naomit[top_diffs,c("country_name", "counterpart_country_name", "diff")])

## whole bunch of China in both plus and minus on, also on both sides
## but also large differences between Canada/US, US/Mexico, Malaysia/US, Russia/NL
## but really seems like ~25/30 are about China


countrycode(c("DEU", "NLD"), "iso3c", "imf")


fob_cif_cprn2_naomit$ratio <- fob_cif_cprn2_naomit$fob_value/fob_cif_cprn2_naomit$cif_value
fob_cif_cprn2_naomit$ratio2 <- fob_cif_cprn2_naomit$cif_value/fob_cif_cprn2_naomit$fob_value
hist(fob_cif_cprn2_naomit$ratio, breaks = 1000)
hist(fob_cif_cprn2_naomit$ratio2, breaks = 1000)

summary(fob_cif_cprn2_naomit$ratio)

hist(fob_cif_cprn2_naomit$ratio[which(fob_cif_cprn2_naomit$ratio < 4)], breaks = 400)
## largest peak actually not at 1, but slightly below, maybe ~0.95

nrow(filter(fob_cif_cprn2_naomit, ratio < 0.5 | ratio > 2))/nrow(fob_cif_cprn2_naomit)
## 33% of observations have ratio <0.5 or >2


ifs <- read.csv("/home/johannes/Downloads/ifs/IFS_11-08-2021 14-30-52-56.csv")
## oof 1.5k indicators -> won't have bilateral values



## ** checking NAs

aggregate(gdp_pcap ~ countrycode, data = df_anls, function(x){sum(is.na(x))}, na.action = NULL)
## around 10% missing :(
## might have to kick out some countries/years


unique(df_open$countrycode)[which(unique(df_open$countrycode) %!in% (unique(df_gdp_pcap_molt$countrycode)))]
## seems ok,
## taiwan not separate country in WB.. just 1 PM tho, so shouldn't be big impact




## *** WDI completeness checks
x <- tbl(con, "wdi") %>%
    filter(varx == "hweal") %>%
    group_by(percentile) %>%
    summarise(length(percentile))



base_cmd <- "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where year >= 1985"

base_df <- as_tibble(dbGetQuery(con, base_cmd))


check_wid_cpltns <- function(varx, percentile){
    #' check how well WID variables cover PM foundings

    print(varx)
    varz <- varx ## need to assign to own objects to be able to filter on them 

    if(missing(percentile)){

        res <- filter(base_df, varx==varz)
    
    } else {
        
        print(percentile)
        
        pctz <- percentile
        res <- filter(base_df, variable == varz & percentile == pctz)
       
    }

    ## some exception to throw when too many variables
    if (length(table(res$variable)) > 1){
        print(varx)
        stop("too many variables")
    }

    print(nrow(res))
    if (nrow(res)!=0){
        
    ## make df base to merge WDI data to 
    dfb <- df_anls[,c("countrycode", "year", "nbr_opened")]

    dfc <- as_tibble(merge(dfb, res, by = c("year", "countrycode"), all.x = TRUE))
    cry_cvrg <- aggregate(year ~ countrycode, na.omit(dfc), length)
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$countrycode

    cry_pm_crvg_actual <- aggregate(nbr_opened ~ countrycode, na.omit(dfc), sum)
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ countrycode, dfc, sum)
    names(cry_pm_crvg_ideal) <- c("countrycode", "nbr_opened_ideal")
    
    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ## maybe need to collapse them instead of having them as vector 
    most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
                                        function(x) (filter(cry_pm_cvrg_cprn, diff == x)$countrycode)))

    ## country-year coverage of countries which have at least 3 WDI observations AND which have WDI data for that year
    PMs_covered_raw <- sum(na.omit(dfc[which(dfc$countrycode %in% crys_geq3),])$nbr_opened)
    ## huh 280 for labsh, not too bad


    ## coverage of countries which have at least three values, even if they don't have WDI data for years of museum founding
    cry_cvrg_geq3 <- sum(filter(dfc, countrycode %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- filter(aggregate(nbr_opened ~ countrycode, dfc, sum), countrycode %in% crys_geq3) %>%
        filter(nbr_opened >= 1) %>%
        nrow()

    return(list(
        variable = varx,
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
    }
}


REDO_WID_CPLTNS_CHK <- FALSE
if (REDO_WID_CPLTNS_CHK){


    check_wid_cpltns("wealg")
    check_wid_cpltns("labsh")
    check_wid_cpltns("wealp")
    check_wid_cpltns("weali")
    check_wid_cpltns("wealc")
    check_wid_cpltns("wealh")
    check_wid_cpltns("sfiinc992i", "p90p100")
    check_wid_cpltns("sfiinc992j", "p90p100")

    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

    shr_variables_p90 <- dbGetQuery(con, "select distinct(variable) from wdi where percentile='p90p100' and first_letter='s'")[[1]]
    wid_cpltns_res_90 <- lapply(as.list(shr_variables_p90), check_wid_cpltns, "p90p100")
    wid_df_res_90 <- do.call(rbind.data.frame, wid_cpltns_res_90)
    wid_df_res_90_tbl <- wid_df_res_90[,-c(which(names(wid_df_res_90) == "most_affected_crys"))]
    names(wid_df_res_90_tbl)[c(2:5)] <- lapply(names(wid_df_res_90_tbl)[c(2:5)], function(x) paste0(x, "_90"))


    shr_variables_p99 <- dbGetQuery(con, "select distinct(variable) from wdi where percentile='p99p100' and first_letter='s'")[[1]]
    wid_cpltns_res_99 <- lapply(as.list(shr_variables_p99), check_wid_cpltns, "p90p100")
    wid_df_res_99 <- do.call(rbind.data.frame, wid_cpltns_res_99)
    wid_df_res_99_tbl <- wid_df_res_99[,-c(which(names(wid_df_res_99) == "most_affected_crys"))]
    names(wid_df_res_99_tbl)[c(2:5)] <- lapply(names(wid_df_res_99_tbl)[c(2:5)], function(x) paste0(x, "_99"))

    wid_tbl <- merge(wid_df_res_90_tbl, wid_df_res_99_tbl, by=c("variable"))


    ## manual check that 99% percentile is as good as 90% percentile 
    wid_tbl[,c("variable", "PMs_covered_raw_90", "PMs_covered_raw_99",
               "cry_cvrg_geq3_90", "cry_cvrg_geq3_99",
               "nbr_of_crys_geq3_90", "nbr_of_crys_geq3_99",
               "nbr_of_crys_geq1pm_90", "nbr_of_crys_geq1pm_99")]
    ## but don't go further into putting it into table
                  

    wid_df_res_90_tbl <- wid_df_res_90_tbl[rev(order(wid_df_res_90_tbl$PMs_covered_raw))[c(1:10)],]
    names(wid_df_res_90_tbl) <- c("variable", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")

    wid_df_res_90_tbl$variable <- recode(wid_df_res_90_tbl$variable, 
           "sptinc992j" = "pretax income (equal-split adults = based on household)",
           "sdiinc992j" = "post-tax income (equal-split adults)",
           "scainc992j"= "post-tax disposable income (equal-split adults)",
           "sfiinc992t"= "fiscal income (threshold)",
           "sfiinc992j"= "fiscal income (equal-split adults)",
           "sfiinc992i"= "fiscal income (individuals)",
           "shweal992j"= "net wealth (equal-split adults)",
           "sptinc992i"= "pretax income (individuals)",
           "sdiinc992i"= "post-tax income (individuals)",
           "sfiinc999t"= "fiscal income (threshold)")
    
    xtbl <- xtable(
        wid_df_res_90_tbl,
        label="wid_cpltns",
        caption = "coverage variables for top decile in WID",
        align = c("l", "p{5.5cm}","p{2cm}", rep("p{3cm}", 3)),
        digits=0)

    

    library(xtable)
    print(
        xtbl,
        include.rownames = F,
        file = paste0(TABLE_DIR, "wid_cpltns.tex"),
        )

}
                    
## **** check whether coverage depends on percentile chosen, doesn't really

pctl_checker <- function(percentile){
    #' next level wrapper function: just use shr variables
    wdi_cpltns_x <- lapply(as.list(shr_variables), check_wdi_cpltns, percentile)
    wdi_res_df <- do.call(rbind.data.frame, wdi_cpltns_x)
    wdi_res_df$percentile <- percentile
    
    return(wdi_res_df)
}

## pctl_checker("p99.5p100")


if (REDO_WDI_CPLTNS_CHK){
    ## could have been that some variables are good at some exotic percentages, but they aren't
    ## sptinc992j still the only variable with decent coverage 

    percentiles <- as_tibble(dbGetQuery(con, "select percentile, count(percentile) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    pctls_cry <- as_tibble(dbGetQuery(con, "select percentile, count(distinct(country)) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    ## just select a bunch of percentiles for coverage checks?
    ## nah that would be stupid, there could be some good coverage in some that i'd miss
    ## use all that have first percentile above 80 and second == 100

    pctls_cry$pct1 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][2]})))
    pctls_cry$pct2 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][3]})))

    pctls_relevant <- filter(pctls_cry, pct1 >=80 & pct2==100)$percentile



    pctl_cpltns_res <- mclapply(pctls_relevant, pctl_checker, mc.cores = 6)
    
    pctl_cpltns_df <- as_tibble(Reduce(function(x,y,...) rbind(x,y,...), pctl_cpltns_res))

    as.data.frame(filter(pctl_cpltns_df, PMs_covered_raw > 250))[,-c(which(names(wdi_df_res_90) == "most_affected_crys"))]

    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, max)
    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, mean)

    as.data.frame(filter(pctl_cpltns_df, variable == "sptinc992j"))
}


## **** gini variables
if (REDO_WDI_CPLTNS_CHK == TRUE){
    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")
    gini_variables <- dbGetQuery(con, "select distinct(variable) from wdi where first_letter=='g'")

    gini_data <- as_tibble(dbGetQuery(con, "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where first_letter='g'"))

    table(gini_data$percentile)
    check_wdi_cpltns("gptinc992j", "p0p100")

    gini_res <- lapply(as.list(gini_variables$variable), check_wdi_cpltns, "p0p100")
    gini_res_df <- do.call(rbind.data.frame,gini_res)
}

## same with share data: only sufficient global coverage for ptinc992j, diinc992j second 


## dbDisconnect(con)


## dbListTables(con)


## checking whether i don't accidentally skip some country-years, apparently not
## aggregate(year ~ country, multi_inner, max)
## aggregate(year ~ country, multi_inner, min)
## aggregate(year ~ country, multi_inner, len)



## **** done: check how DEU has shweal992j, but only p0p100 percentiles: is because metadata is for all countries even if they don't have that data, actual data only there for handful of countries
## wealth_check <- as_tibble(DBI::dbGetQuery(con, "select distinct(varx) as varx from wdi where country='DEU' and varx like '%weal%'"))


## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where varx like '%weal%' and country = 'DEU' and year >= 1985"))

## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where first_letter='s' and country = 'DEU' and year >= 1985"))


## table(res$variable, res$percentile)



## **** average vs share check

## see if shares are always there when there are averages, turns out it's the other way around: there are shares, but not always the averages to compute them
## -> sticking to shares is fine: more complete and easier to handle

## con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

## cmd_cplt_check <- "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where (percentile='p90p100' or percentile='p0p90') and year>=1985"

## res_cplt_check <- as_tibble(dbGetQuery(con, cmd_cplt_check))

## res_s_p90p100 <- filter(res_cplt_check, first_letter=="s" & percentile=="p90p100")
## res_a_p0p90 <- filter(res_cplt_check, first_letter=="a" & percentile=="p0p90")
## res_a_p90p100 <- filter(res_cplt_check, first_letter=="a" & percentile=="p90p100")
## res_a_mrg <- as_tibble(merge(res_a_p0p90, res_a_p90p100, by=c("countrycode", "variable", "year")))
## res_a_mrg$ratio <- res_a_mrg$value.y/(9*res_a_mrg$value.x + res_a_mrg$value.y)

## res_a_mrg_cut <- res_a_mrg[,c("countrycode", "variable", "year", "ratio")]
## res_a_mrg_cut$varx <- res_a_mrg$varx.x


## ## there are only 16k with top 10, but 1.2m with entire population

## ## make ratio
## ## actually average is not good for making top 10% concentration ratios: average of p0p100 is lower than p90p100 -> that's why ratio was negative
## ## would either need total of p90p100 and p0p100
## ## or average of p0p90 -> can calculate top10 ratio as p90p100/(9*p0p90 + p90p100)
## ## maybe first casting is better? then column filtering is easier 

## table(res_s_p90p100$variable, res_s_p90p100$varx)
## table(res_a_mrg_cut$variable, res_a_mrg_cut$varx)
## ## varx only differ from variables in that variables sometimes are measured on different levels of analysis
## ## -> can do varx2: only remove the first letter
## res_s_p90p100$varx2 <- substring(res_s_p90p100$variable, 2, 100)
## res_a_mrg_cut$varx2 <- substring(res_a_mrg_cut$variable, 2, 100)


## cpr_shrs <- as_tibble(merge(res_s_p90p100, res_a_mrg_cut, by=c("countrycode", "varx2", "year")))
## cor(cpr_shrs$ratio, cpr_shrs$value, use = "complete.obs")
## ## nice now correlation is pretty much 1
## ## res_s_p90p100 has more observations (14.7k) than res_a_mrg_cut (11.9k), res_a_mrg_cut also has 90 NAs
## ## which observations (countries, variables, years) have shares but not complete averages?
## ## multiple aggregations

## agg_s_year <- as.data.frame(table(res_s_p90p100$year))
## names(agg_s_year) <- c("year", "share_cnt")
## agg_r_year <- as.data.frame(table(res_a_mrg_cut$year))
## names(agg_r_year) <- c("year", "ratio_cnt")

## year_comparison <- merge(agg_s_year, agg_r_year, using = c("year"))
## year_cprn_melt <- melt(year_comparison, id="year")

## ggplot(year_cprn_melt, aes(x=year, y=value, group=variable, color=variable)) +
##     geom_line()
## ## tbh differences are not that big, maybe 20-30 per year  -> 10% difference

## res_s_p90p100[,c("countrycode", "variable", "year")]

## ## could combine, remove all that occur twice

## s_r_cbin <- rbind(res_s_p90p100[,c("countrycode", "varx2", "year")], res_a_mrg_cut[,c("countrycode", "varx2", "year")])
## s_r_cbin$ctr <- 1

## sr_cbin_agg <- as_tibble(aggregate(ctr ~ countrycode + varx2 + year, s_r_cbin, sum))
## plot(table(filter(sr_cbin_agg, ctr==1)$year), type='l')
## ## year: seems kinda stable, but decreaes in recent years
## table(filter(sr_cbin_agg, ctr==1)$varx2)

## ## most differences in variables about "fiinc": fiscal income: there in share, but not in average
## ## fiinc992i
## ## fiinc992j
## ## fiinc992t

## ## also some in others
## ## diinc992i
## ## diinc992t
## ## fiinc999t
## ## hweal992i
## ## ptinc992j 
## table(filter(sr_cbin_agg, ctr==1)$countrycode)

## cry_cnt_a <- as.data.frame(table(res_a_mrg_cut$countrycode))
## names(cry_cnt_a) <- c("countrycode", "count")
## cry_cnt_a$countrycode <- as.character(cry_cnt_a$countrycode)
## cry_cnt_a$condition <- "ratio"

## cry_cnt_s <- as.data.frame(table(res_s_p90p100$countrycode))
## names(cry_cnt_s) <- c("countrycode", "count")
## cry_cnt_s$countrycode <- as.character(cry_cnt_s$countrycode)
## cry_cnt_s$condition <- "share"

## cry_cnt_cbn <- rbind(cry_cnt_a, cry_cnt_s)

## ggplot(cry_cnt_cbn, aes(x=countrycode, fill=condition, y=count)) +
##     geom_bar(position = "dodge", stat="identity")

## ## most countries have just 35 observations, differences is only for the countries that have more: more shares than ratios


## *** EGMUS data

EGMUS_DIR <- "/home/johannes/Dropbox/phd/data/EGMUS/"

df_1998 <- read.csv(paste0(EGMUS_DIR, "egmus_export_1998_seded.csv"), sep = ";", header = T, dec = ",") 
df_1999 <- read.csv(paste0(EGMUS_DIR, "egmus_export_1999_seded.csv"), sep = ";", header = T, dec = ",") 
df_2000 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2000_seded.csv"), sep = ";", header = T, dec = ",")
df_2001 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2001_seded.csv"), sep = ";", header = T, dec = ",")
df_2002 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2002_seded.csv"), sep = ";", header = T, dec = ",")
df_2003 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2003_seded.csv"), sep = ";", header = T, dec = ",")
df_2004 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2004_seded.csv"), sep = ";", header = T, dec = ",")
df_2005 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2005_seded.csv"), sep = ";", header = T, dec = ",")
df_2006 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2006_seded.csv"), sep = ";", header = T, dec = ",")
df_2007 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2007_seded.csv"), sep = ";", header = T, dec = ",")
df_2008 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2008_seded.csv"), sep = ";", header = T, dec = ",")
df_2009 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2009_seded.csv"), sep = ";", header = T, dec = ",")
df_2010 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2010_seded.csv"), sep = ";", header = T, dec = ",")
df_2011 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2011_seded.csv"), sep = ";", header = T, dec = ",")
df_2012 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2012_seded.csv"), sep = ";", header = T, dec = ",")
df_2013 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2013_seded.csv"), sep = ";", header = T, dec = ",")
df_2014 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2014_seded.csv"), sep = ";", header = T, dec = ",")
df_2015 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2015_seded.csv"), sep = ";", header = T, dec = ",")
df_2016 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2016_seded.csv"), sep = ";", header = T, dec = ",")
df_2017 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2017_seded.csv"), sep = ";", header = T, dec = ",")
df_2018 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2018_seded.csv"), sep = ";", header = T, dec = ",")
df_2019 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2019_seded.csv"), sep = ";", header = T, dec = ",")
df_2020 <- read.csv(paste0(EGMUS_DIR, "egmus_export_2020_seded.csv"), sep = ";", header = T, dec = ",")

df_egmus <- as_tibble(rbind(df_1998,df_1999,df_2000,df_2001,df_2002,df_2003,df_2004,df_2005,df_2006,
                  df_2007,df_2008,df_2009,df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016,
                  df_2017,df_2018,df_2019, df_2020))

df_egmus$private <- as.character(df_egmus$X3d..Ownership...private.owned.museums...Total)
df_egmus$private_clean <- gsub("\\.", "", df_egmus$private)
df_egmus$private_num <- as.numeric(df_egmus$private_clean)


## na.omit needed to remove all the points to still have line
ggplot(data = na.omit(df_egmus[,c("Year", "private_num", "Country")]), aes(x=Year, y=private_num, group=Country, color=Country)) +
    geom_line() 


aggregate(private_num ~ Country, df_egmus, mean, use="complete.obs")
filter(df_egmus, Country == "Italy")$private_num






## *** diffusion
## **** geographical proximity: use country boundaries
library(rgdal)
spdf <- readOGR(dsn = "/home/johannes/Dropbox/phd/papers/org_pop/data/boundaries/World_EEZ_v11_20191118/")


spdf_fltrd <- as_tibble(unique(spdf@data[,c("SOVEREIGN1", "SOVEREIGN2")]))
names(spdf_fltrd) <- c("cry1", "cry2")
## duplicate boundaries, turn to have alphabetically higher country first 
spdf_fltrd$cry1_cd <- countrycode(spdf_fltrd$cry1, "country.name", "iso3c", custom_match = c("Comores" = "COM", "Micronesia" = "FSM"))
spdf_fltrd$cry2_cd <- countrycode(spdf_fltrd$cry2, "country.name", "iso3c", custom_match = c("Comores" = "COM", "Micronesia" = "FSM"))

sea_boundaries <- rbind(spdf_fltrd[,c("cry1_cd", "cry2_cd")],
                        setNames(rev(spdf_fltrd[,c("cry1_cd", "cry2_cd")]),
                                 names(spdf_fltrd[,c("cry1_cd", "cry2_cd")])))


sea_boundaries <- na.omit(sea_boundaries)
sea_boundaries <- filter(sea_boundaries, cry1_cd < cry2_cd)


## https://stackoverflow.com/questions/19297475/simplest-way-to-get-rbind-to-ignore-column-names

land_boundaries <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/boundaries/geodatasource_land_boundaries.csv"))

land_boundaries$cry1_cd <- countrycode(land_boundaries$country_name, "country.name", "iso3c")
land_boundaries$cry2_cd <- countrycode(land_boundaries$country_border_name, "country.name", "iso3c")
land_boundaries <- na.omit(land_boundaries)

land_boundaries <- rbind(land_boundaries[,c("cry1_cd", "cry2_cd")],
                         setNames(rev(land_boundaries[,c("cry1_cd", "cry2_cd")]),
                                  names(land_boundaries[,c("cry1_cd", "cry2_cd")])))

land_boundaries <- unique(filter(land_boundaries, cry1_cd < cry2_cd))

all_boundaries <- unique(rbind(sea_boundaries, land_boundaries))

all_boundaries <- rbind(all_boundaries[,c("cry1_cd", "cry2_cd")],
                        setNames(rev(all_boundaries[,c("cry1_cd", "cry2_cd")]),
                                 names(all_boundaries)))

## *** create variable of number in neighbors

## need efficient way: for loops are slow
## expand, then aggregate: nbr_opened ~ country + year, mean

## also need year somehow
## first i need much more entries: merge all neighbor countries for each year 
geo_mrg1 <- as_tibble(merge(df_anls[c("countrycode", "year")], all_boundaries, by.x = "countrycode", by.y = "cry1_cd", all.x = TRUE))

filter(geo_mrg1, countrycode == "POL")
filter(geo_mrg1, cry2_cd == "POL")

table(as.data.frame(filter(geo_mrg1, countrycode == "POL"))$year)


## merge actual count to geo_mrg1
df_merge_prep <- df_anls[c("countrycode", "year", "nbr_opened")]
names(df_merge_prep) <- c("cry2_cd", "year", "geo_neib_opened")

geo_mrg2 <- as_tibble(merge(geo_mrg1, df_merge_prep, by = c("year", "cry2_cd"), all.x = TRUE))

as.data.frame(filter(geo_mrg2, countrycode == "POL" cry2_cd == "DEU"))
filter(geo_mrg2, cry2_cd == "DEU" & year == 2000)

## seems to have worked: yup: now proper duplication
## maybe previously just wrong query to check 


## aggregate by country-year
geo_agg1 <- as_tibble(aggregate(geo_neib_opened ~ countrycode + year, geo_mrg2, mean))
as.data.frame(filter(geo_agg1, countrycode == "POL"))

df_anls2 <- as_tibble(merge(df_anls, geo_agg1, by=c("countrycode", "year"), all.x = TRUE))
## pretty sure it's fine to set NA's to 0: means there are no neighbors
df_anls2$geo_neib_opened[which(is.na(df_anls2$geo_neib_opened))] <- 0

summary(df_anls2$geo_neib_opened)

df_anls <- df_anls2

## *** world values survey
df_wvs <- as_tibble(readRDS("/home/johannes/ownCloud/WVS/WVS_TimeSeries_1981_2020_R_v2_0.rds"))

    
## ** aggregating systematically

## specify variable-specific aggregation function 
vrbl_agg_func_dict <- list(gini=mean,
                           gdp_pcap=mean,
                           gdp_pcapk = mean, 
                           nbr_opened = sum,
                           nbr_opened_cum = max,
                           nbr_opened_cum_sqrd = max
                           )


agger <- function(varx, dfx){

    ## aggregate varx in df_anls by wv + countrycode, use aggregation function specified in vrbl_agg_func_dict
    ## dfx$agg_var <- as.numeric(unlist(dfx[,c(varx)]))
    funx <- vrbl_agg_func_dict[varx][[1]]
    dfx$agg_var <- as.numeric(unlist(dfx[,c(varx)]))
    ## df_aggx <- aggregate(agg_var ~ wv + countrycode, dfx, mean) ## old version with only mean 
    df_aggx <- aggregate(agg_var ~ wv + countrycode, dfx, FUN = funx)
    df_aggx[,varx] <- df_aggx$agg_var
    return(as_tibble(df_aggx[,c("countrycode", "wv", varx)]))}


## agger("nbr_opened_cum", filter(dfx, countrycode == "USA"))


agg_sys <- function(wave_lengthx, vrbls){
    ## wave_lengthx: spell length
    ## vrbls: vrbls to consider

    print(wave_lengthx)
    print(vrbls)

    dfx <- df_anls

    nbr_pm_ttl <- sum(dfx$nbr_opened)

    wv_ctr <- 0
    wv_nbr <- 1

    dfx$wv <- 0

    ## label the waves based on wavelength, not super elegant but works

    for (yearx in unique(dfx$year)){
        wv_ctr <- wv_ctr + 1
        ## print(c(wv_ctr, wv_nbr))
        dfx[which(dfx$year == yearx),"wv"] <- wv_nbr
        if (wv_ctr == wave_lengthx){
            wv_ctr <- 0
            wv_nbr <- wv_nbr + 1}
    }

    ## add nbr_opened by default 
    vrbls_all <- c("nbr_opened", vrbls)

    ## aggregate the variables duh 
    dfx_agg_vrbls <- as_tibble(Reduce(
        function(x,y, ...) merge(x,y, all = TRUE),
        ## lapply(c("gini", "gdp_pcap", "gdp_pcapk"), agger, dfx=dfx)
        lapply(vrbls_all, agger, dfx=dfx)
    ))

    ## dfx_agg_cnts <- as_tibble(aggregate(nbr_opened ~ countrycode + wv, dfx, sum))

    dfx_agg_names <- dfx %>% group_by(countrycode, wv) %>% summarise(name = list(unlist(name)))

    dfx_agg <- as_tibble(Reduce(
        function(x,y, ...) merge(x,y,all.x=TRUE),
        ## list(dfx_agg_means, dfx_agg_cnts, dfx_agg_names)))
        list(dfx_agg_vrbls, dfx_agg_names)))
        
    ## dfx_agg <- as_tibble(merge(dfx_agg_cnts, dfx_agg_means, by=c("countrycode", "wv"), all.x = T))

    
    return(list(df=dfx_agg,
                vrbls=vrbls,
                nbr_pm_ttl = nbr_pm_ttl,
                wave_lengthx = wave_lengthx,
                dfx_agg_names = dfx_agg_names,
                dfx_agg_vrbls = dfx_agg_vrbls
                ))
    }




df_agg2 <- agg_sys(2, c("gdp_pcapk", "gini"))
filter(df_agg2$dfx_agg_vrbls, countrycode == "USA")
df_agg4 <- agg_sys(4, c("gini", "gdp_pcap", "gdp_pcapk"))
df_agg8 <- agg_sys(8, c("gini", "gdp_pcapk"))





## could check the museum names in detail, but atm no unexpected behavior, small dips along the may could be longitudinal gerrymandering 
## unlist(na.omit(df_agg2$df)$name)[!is.na(unlist(na.omit(df_agg2$df)$name))]
## unlist(na.omit(df_agg4$df)$name)[!is.na(unlist(na.omit(df_agg4$df)$name))]
## unlist(na.omit(df_agg8$df)$name)[!is.na(unlist(na.omit(df_agg8$df)$name))]


score_agg <- function(agg_obj){
    
    dfx_agg <- na.omit(agg_obj$df)
    ## percent of entities covered
    pct_ent_cvrd <- nrow(dfx_agg)/nrow(agg_obj$dfx_agg_vrbls)
    ## pct_ent_cvrd <- nrow(dfx_agg)/nrow(agg_obj$dfx_agg_cnts)
    pct_pms_cvrd <- sum(dfx_agg$nbr_opened)/agg_obj$nbr_pm_ttl

    return(list(wave_lengthx = agg_obj$wave_lengthx,
                pct_ent_cvrd = pct_ent_cvrd,
                pct_pms_cvrd = pct_pms_cvrd,
                vrbls = paste(agg_obj$vrbls, collapse = "-")
                ))
}

score_agg(df_agg2)
score_agg(df_agg4)
score_agg(df_agg8)


## *** evaluating coverage

cvrg_evaluation <- function(vrbls, wvlen_start, wvlen_end){
    ## vrbls <- c("gini", "gdp_pcapk", "nbr_opened_cum")
    cbns <- do.call("c", lapply(seq_along(vrbls), function(i) combn(vrbls, i, FUN = list)))

    ## cfgs <- expand.grid(cbns, seq(1,10))
    cfgs <- expand.grid(cbns, seq(wvlen_start,wvlen_end))
    names(cfgs) <- c("vrbls", "wavelength")
    cfg_list <- apply(cfgs, 1, c)
    cover_res <- mclapply(cfg_list, function(x) score_agg(agg_sys(x$wavelength, x$vrbls)), mc.cores = 4)

    ## cover_res <- apply(cfgs, 1, function(x) score_agg(agg_sys(x$wavelength, x$vrbls)))
    res_df <- do.call(rbind, cover_res)

    ## for some reason necessary to unlist the columns 
    res_df2 <- as.data.frame(apply(res_df, 2, unlist))

    names(res_df2) <- c("wave_length", "prop. spells covered", "prop. PM founding covered", "vrbls")


    res_melt <- melt(res_df2, id=c("wave_length", "vrbls"))
    res_melt$value <- as.numeric(res_melt$value)
    res_melt$wave_length <- as.numeric(res_melt$wave_length)

    return(res_melt)
}

vrbls <- c("gini", "gdp_pcapk", "nbr_opened_cum")
wvlen_start <- 1
wvlen_end <- 10
res_melt <- cvrg_evaluation(vrbls, wvlen_start, wvlen_end)

pdf(paste(FIG_DIR,"completeness.pdf", sep = ""), height = 2.5, width = 5)

ggplot(res_melt, aes(x=factor(wave_length), y=value, group=interaction(variable, vrbls))) +
    geom_line(size=2, alpha = 0.6, position=position_jitter(w=0.15, h=0.015),
              mapping = aes(linetype = variable, color = vrbls)) +
    labs(x = "wave length", y="coverage") 

dev.off()



## hmm for some reason coverage goes down on higher values?
## could make sense for some differences, but multiples of lower values should be at least as complete as the lower values themselves, e.g. 8 should be as least as complete as 4
    
## error might have been agger fucking up by using df_anls instead of wave-length specific dfx
## now small fluctuations (also going down), but i think those should be able to happen
    


## ** add lagged values

## overly messy way of lagging variables that creates intermediary vars because mutate/lag doesn't accept variablies as input

lagger <- function(dfx, vrbls_to_lag){
    for (varx in vrbls_to_lag){
        lag_name = paste(varx, "_lag1", sep = "")
        ## eval(parse("lag_name"))
        ## df_anls$var_to_lag <- df_anls[,c(varx)]
        ## df_anls[,"var_lagged"] <- mutate(group_by(df_anls, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
        ## df_anls[,lag_name] <- df_anls$var_lagged
        ## df_anls <- df_anls[,-which(names(df_anls) %in% c("var_to_lag", "var_lagged"))]

        dfx[,"var_to_lag"] <- dfx[,c(varx)]
        dfx[,"var_lagged"] <- mutate(group_by(dfx, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
        dfx[,lag_name] <- dfx[,"var_lagged"]

        dfx <- dfx[,-which(names(dfx) %in% c("var_to_lag", "var_lagged"))]
    }
    return(dfx)
}

vrbls_to_lag <- c("gdp_pcap", "gdp_pcapk", "gini", "nbr_opened")

df_anls <- lagger(df_anls, vrbls_to_lag)

filter(df_anls[,c("countrycode", "year", "nbr_opened", "nbr_opened_lag1", "gini", "gini_lag1")], countrycode == "USA")


## ** negative binomial

## *** example
## https://rdrr.io/cran/lme4/man/glmer.nb.html

## set.seed(101)
## dd <- expand.grid(f1 = factor(1:3),
##                   f2 = LETTERS[1:2], g=1:9, rep=1:15,
##           KEEP.OUT.ATTRS=FALSE)
## summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
## dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
## str(dd)
## require("MASS")## and use its glm.nb() - as indeed we have zero random effect:
## ## Not run: 
## m.glm <- glm.nb(y ~ f1*f2, data=dd, trace=TRUE)
## summary(m.glm)
## m.nb <- glmer.nb(y ~ f1*f2 + (1|g), data=dd, verbose=TRUE)
## m.nb
## ## The neg.binomial theta parameter:
## getME(m.nb, "glmer.nb.theta")
## LL <- logLik(m.nb)
## ## mixed model has 1 additional parameter (RE variance)
## stopifnot(attr(LL,"df")==attr(logLik(m.glm),"df")+1)
## plot(m.nb, resid(.) ~ g)# works, as long as data 'dd' is found


## *** pglm

## found.pglm.nb1 <- pglm(nbr_opened ~ nbr_opened_lag1, data = df_anls,
##                         family=negbin,
##                         model = "within",
##                         index = "countrycode")

## found.pglm.nb2 <- pglm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb3 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1), data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb4 <- pglm(nbr_opened ~ nbr_opened_lag1 + gini_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## found.pglm.nb5 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + gini_lag1, data = df_anls,
##                     family=negbin,
##                     model = "within",
##                     index = "countrycode")

## screenreg(list(found.pglm.nb1,found.pglm.nb2,found.pglm.nb3,found.pglm.nb4,found.pglm.nb5))



## *** glmer.nb


## **** full df

stop("before models")

regger.nb <- function(list_of_models, data){
    #' batch processing of mah negative binomial regression models
    reg_res <- mclapply(list_of_models, glmer.nb, data = data, mc.cores = 6)
    return(reg_res)
}

df_anls$gdp_pcapd <- df_anls$gdp_pcap/100000

reg_res <- regger.nb(list(
    nbr_opened ~ (1 | countrycode),
    nbr_opened ~ nbr_opened_cum + (1 | countrycode),
    nbr_opened ~ gdp_pcapd + (1 | countrycode),
    nbr_opened ~ nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),
    nbr_opened ~ nbr_opened_lag1 + gini_lag1 + (1 | countrycode),
    nbr_opened ~ nbr_opened_lag1 + nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode)),
    df_anls)

screenreg(reg_res)

x <- glmer.nb(nbr_opened ~ gdp_pcapk + (1 | countrycode), data = df_anls)
screenreg(x)


mod <- reg_res[[1]]

screenreg(mod)

add_beta_modelsummary <- function(mod){
    #' add standardized effect to model in normal SE spot, use t/z stat
    bs <- fixef(mod)
    if (len(bs) > 2){
        betas <- c(NA, lm.beta.lmer(mod))
        names(betas) <- names(bs)

    ## add some NA entries for "SD (Intercept)" and "SD (Observations)"
        betas_padded <- c(betas, NA, NA)
        names(betas_padded)[(len(betas_padded)-1):len(betas_padded)] <- c("SD (Intercept)", "SD (Observations)")
    }
    else {
        betas_padded <- c(rep(NA, len(bs)), NA, NA)
        names(betas_padded) <- c(names(bs), c("SD (Intercept)", "SD (Observations)"))
    }
    modsum_std <- modelsummary(mod,
                               vcov = list(betas_padded),
                               output = "modelsummary_list")

    return(modsum_std)
        
}


mods_stds <- mclapply(reg_res, add_beta_modelsummary)
coef_map <- c("(Intercept)", "nbr_opened_cum", "nbr_opened_cum_sqrd", "gini_lag1", "nbr_opened_lag1",
              "SD (Intercept)", "SD (Observations)")

modelsummary(mods_stds[2:5], 
             estimate = "{estimate}[{statistic}]{stars}",
             coef_map = coef_map,
             output = "markdown")


## **** visualization
library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

plot_summs(reg_res[c(2,4:6)], exp=F)
plot_coefs(reg_res[3:6], facet.rows = 5)


## **** manual regression 



stop("functionalized models done")
## clean up the non-functionalized regressions below when I actually do them 

print("nb df_anls 1")
found.nb_fe_all <- glmer.nb(nbr_opened ~ (1 | countrycode), data = df_anls)
print("nb df_anls 2")
found.nb_nbr_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_anls)
found.nb_cum_all <- glmer.nb(nbr_opened ~ nbr_opened_cum + (1 | countrycode),  data = df_anls)
found.nb_cum_all_sqrd <- glmer.nb(nbr_opened ~ nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),  data = df_anls)
found.nb_org_pop <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + nbr_opened_cum + nbr_opened_cum_sqrd + (1 | countrycode),  data = df_anls)

print("nb df_anls 3")
found.nb_gdp_all <- glmer.nb(nbr_opened ~  gdp_pcapk_lag1 + (1 | countrycode), data = df_anls)
print("nb df_anls 4")
found.nb_gini_all <- glmer.nb(nbr_opened ~ gini_lag1  + (1 | countrycode), data = df_anls)
print("nb df_anls 5")
found.nb_all_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcapk_lag1 + gini_lag1 + (1 | countrycode), data = df_anls)

screenreg(list(found.nb_nbr_all, found.nb_cum_all, found.nb_cum_all_sqrd, found.nb_org_pop))
stargazer(list(found.nb_nbr_all, found.nb_cum_all, found.nb_cum_all_sqrd, found.nb_org_pop), type = "text")

found.nb_all_all_std <- createTexreg(coef.names = names(fixef(found.nb_all_all))[2:length(names(fixef(found.nb_all_all)))],
                                     coef = lm.beta.lmer(found.nb_all_all))


model_list_all <- list(found.nb_fe_all, found.nb_nbr_all, found.nb_gdp_all, found.nb_gini_all, found.nb_all_all, found.nb_all_all_std)


screenreg(model_list_all,
          custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list_all[1:5], function(x) sum(x@frame$nbr_opened))), 273)))


texreg(model_list_all,
          custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list_all[1:5], function(x) sum(x@frame$nbr_opened))), 273)),
       file = paste0(TABLE_DIR,"nb_all.tex"),
       label = "nb_all",
       caption = "Negative Binomial with full DF"
       )

## **** aggregate 4

df_lag4 <- df_agg4$df
df_lag4 <- lagger(df_lag4, vrbls_to_lag)

filter(df_lag4, countrycode=="USA")[,c("nbr_opened", "nbr_opened_lag1", "gini", "gini_lag1", "gdp_pcapk", "gdp_pcapk_lag1")]





print("lag4 1")
found.nb_fe <- glmer.nb(nbr_opened ~ (1 | countrycode), data = df_lag4)
print("lag4 2")
found.nb_nbr <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_lag4)
print("lag4 3")
found.nb_gdp <- glmer.nb(nbr_opened ~  gdp_pcapk_lag1 + (1 | countrycode), data = df_lag4)
print("lag4 4")
found.nb_gini <- glmer.nb(nbr_opened ~ gini_lag1  + (1 | countrycode), data = df_lag4)
print("lag4 5")
found.nb_all <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcapk_lag1 + gini_lag1 + (1 | countrycode), data = df_lag4)

found.nb_all_std <- createTexreg(coef.names = names(fixef(found.nb_all))[2:length(names(fixef(found.nb_all)))], coef = lm.beta.lmer(found.nb_all))

model_list <- list(found.nb_fe, found.nb_nbr, found.nb_gdp, found.nb_gini, found.nb_all, found.nb_all_std)

screenreg(model_list, custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list[1:5], function(x) sum(x@frame$nbr_opened))), 347)))



texreg(model_list,
       custom.gof.rows = list("nbr. PMs founding covered" = c(unlist(lapply(model_list[1:5], function(x) sum(x@frame$nbr_opened))), 347)),
       file = paste0(TABLE_DIR,"nb_agg4.tex"),
       label = "nb_agg4",
       caption = "Negative Binomial, aggregated to 4 year intervals"
       )

stop("models done")




## fastest way of standardized coefs: just add another model 

## screenreg(list(found.nb0, found.nb1, found.nb2,found.nb3,found.nb4,found.nb5))
## screenreg(list(found.nb1,found.pglm.nb1,found.nb3,found.pglm.nb3,found.nb4,found.pglm.nb4,found.nb5,found.pglm.nb5))


## model 2 (not log-transfomed gdp_pcap) doesn't work
## think the differences are smaller than in poisson:
## at least the general direction, but still bunch of differences with significance in nbr_opened_lag1 (between 3 and 4), gini (5)

## but i still don't know what's happening, and why things are different
## also need to understand the techniques: negative binomial/poisson especially

## *** interpreation
## can exp(coefs) to get ratios with a unit change
## A country's rate of founding PMs increases by exp(0.78) = 2.18 for each log(GDP) point
## A country's founding rate of PMs increases by exp(0.07) = 1.07 (7%) for each gini point


## https://cran.r-project.org/web/packages/effectsize/vignettes/from_test_statistics.html
library(effectsize)
anova(found.nb_all)

## anova result looks different 

## https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
## library(MuMIn)
## r.squaredGLMM(found.nb5x)
## marginal and conditional
## marginal: variance explained by fixed effects
## conditional: variance explained by entire model, including both FE/RE

## methods:
## - delta: for all distributions/links
## - lognormal, trigamma: only for logarithmic link
##   no idea if negbin has logarithmic link
## - trigamma recommended when available

## not all R^2 algorithms make sense?: https://stats.stackexchange.com/questions/250984/pseudo-r2-values-for-negative-binomial-regression-model-in-r-yields-inconsistent
## stata: https://stats.idre.ucla.edu/stata/output/negative-binomial-regression/: McFadden's pseudo R-squared means something different in negbin than in OLS -> interpret with caution

## why don't we do k-fold validation: precision, recall, and whatever performance techniques ML/AI has come up with
## https://stackoverflow.com/questions/63208120/how-can-i-use-k-fold-cross-validation-for-negative-binomial-regression-in-sklear
## could implement 





## ** visualization/inspection



library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

## plot_summs(found.nb1, found.nb3, found.nb4, found.nb5, plot.distributions = T)
## plot_summs looks nice, but idk if I shouldn't rather write my own ggplot visualization
## also no support for the pglm models -> idk if plot_summs has generic class that can be filled wiht arbitrary values
## also need standardized stuff
## also not that good at comparing models with different variables
## plotting distributions gets very full when more than a handful

## *** curves


## *** plot country trajectories with PM foundings
gdp_pcap_agg <- as_tibble(aggregate(gdp_pcapk ~ countrycode, df_anls, mean))
gini_agg <- as_tibble(aggregate(gini ~ countrycode, df_anls, mean))

var_means <- as_tibble(merge(gdp_pcap_agg, gini_agg, all.x = TRUE))

names(var_means) <- c("countrycode", "gdp_pcapk_mean", "gini_mean")

df_anls_vis <- as_tibble(merge(df_anls[,c("countrycode", "year", "nbr_opened", "gini", "gdp_pcapk")] , var_means, by='countrycode'))
df_anls_vis$gini_demeaned <- df_anls_vis$gini - df_anls_vis$gini_mean
df_anls_vis$gdp_pcapk_demeaned <- df_anls_vis$gdp_pcapk - df_anls_vis$gdp_pcapk_mean

## sum(df_anls_vis[which(df_anls_vis$countrycode == "DEU"),]$gdp_pcapk_demeaned)



plt1 <- ggplot(df_anls_vis, aes(x=year, y=gdp_pcapk_demeaned, group=countrycode, color = countrycode)) + 
    geom_line(alpha=0.15) +
    geom_line(df_anls_vis[which(df_anls_vis$countrycode %in% c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")),], mapping = aes(x=year, y=gdp_pcapk_demeaned, group=countrycode, color = countrycode), alpha = 0.8) +
    scale_color_discrete(breaks = c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")) + 
    geom_point(df_anls_vis[which(df_anls_vis$nbr_opened > 0),], mapping = aes(x=year, y=gdp_pcapk_demeaned, size = nbr_opened, color = countrycode), alpha = 0.8) +
    labs(y = "gdp_pcapk_demeaned")
plt1


plt2 <- ggplot(df_anls_vis, aes(x=year, y=gini_demeaned, group=countrycode, color = countrycode)) + 
    geom_line(alpha=0.15) +
    geom_line(df_anls_vis[which(df_anls_vis$countrycode %in% c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")),], mapping = aes(x=year, y=gini_demeaned, group=countrycode, color = countrycode), alpha = 0.8) +
    scale_color_discrete(breaks = c("DEU", "ITA", "USA", "KOR", "ESP", "FRA", "CHN")) + 
    geom_point(df_anls_vis[which(df_anls_vis$nbr_opened > 0),], mapping = aes(x=year, y=gini_demeaned, size = nbr_opened, color = countrycode), alpha = 0.8) +
    labs(y = "gini_demeaned")
plt2

pdf(paste0(FIG_DIR, "fe_viz.pdf"), height = 9, width = 9)
grid.arrange(plt1,plt2)
dev.off()


stop("plots and tables done")






## ** robust SEs
## https://stackoverflow.com/questions/26412581/robust-standard-errors-for-mixed-effects-models-in-lme4-package-of-r

## interpretation: the more points are above 0, the stronger the effect of that variable? 

## wow Italian gdp per capita, wtf happened


## maybe can plot hist of those points directly? 
## hist(df_anls_vis[which(df_anls_vis$nbr_opened > 0),]$gdp_pcapk_demeaned, breaks = 20)
## hist(df_anls_vis[which(df_anls_vis$nbr_opened > 0),]$gini_demeaned, breaks = 20)
## makes it looks murky af
## but wide spread doesn't mean that it can't be distinguished from 0
## but probably still too visualization-driven: it's a measure I only came up with because of visualization


## hmm actually you don't need lines, or can put high alpha there 
## raises question of how robust model is: is it all driven by US/Germany?



## ** poisson test

## function is
x <- seq(1,10,1)

poi.func <- function(x, lambda){
    return(((lambda^x)/(factorial(x))) * exp(1)^(-lambda))
    }


plot(poi.func(x,1), type='l')

for (i in seq(1,10,1)){
    lines(poi.func(x,i), type='l')
    }


plot(x/20, col='white')
lines(poi.func(x,2), type='l')
plot(2^x, type='l')
lines(factorial(x))




## *** pglm
library(plm)
## seems like poisson/negative binomial is implemented in https://cran.r-project.org/web/packages/pglm/pglm.pdf
## yves croissant, wrote some b

library(pglm)

found.pglm.poi1 <- pglm(nbr_opened ~ nbr_opened_lag1,
                        data = na.omit(df_anls[,c("countrycode", "nbr_opened", "nbr_opened_lag1")]),
                        family=poisson,
                        model = "within",
                        index = "countrycode")

found.pglm.poi2 <- pglm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi3 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1), data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi4 <- pglm(nbr_opened ~ nbr_opened_lag1 + gini_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")

found.pglm.poi5 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + gini_lag1, data = df_anls,
                    family=poisson,
                    model = "within",
                    index = "countrycode")


## *** comparison

summary(found.pglm.poi1)
screenreg(list(found.pglm.poi1,found.poi1))

screenreg(list(found.pglm.poi1,found.poi1,
               found.pglm.poi2,found.poi2,
               found.pglm.poi3,found.poi3,
               found.pglm.poi4,found.poi4,
               found.pglm.poi5,found.poi5))

## hmm different results
## overall tendencies are kinda the same, but not always: gini significant in one, not the other
## also differences in significance of nbr_opened_lag1
## only aggreement in significance in gdp_pcap_lag1 and log(gdp_pcap_lag1)






## ** visualization


df_plt <- df_anls
df_plt$region <- countrycode(df_plt$countrycode, "iso3c", "region")


year_selector <- function(x)(
    # convert cuts back to years
    substring(x, 2,5))


df_plt$cut <- cut(df_plt$year, seq(min(df_plt$year), max(df_plt$year)+5, by = 3))
df_plt$cut2 <- as.numeric(sapply(as.character(df_plt$cut), year_selector))

df_viz <- as_tibble(aggregate(nbr_opened ~ region + cut2, df_plt, sum))


## founding rates
## first country mean per cut
## then region sum 
df_viz_pop1 <- as_tibble(aggregate(population ~ countrycode + cut2 , df_plt, mean))
filter(df_viz_pop1, countrycode == "DEU")
df_viz_pop1$region <- countrycode(df_viz_pop1$countrycode, "iso3c", "region")

countrycode(unique(filter(df_viz_pop1, region == "South Asia")$countrycode), "iso3c", "country.name")


df_viz_pop2 <- as_tibble(aggregate(population ~ region + cut2, df_viz_pop1, sum))

## ggplot(df_viz_pop2, aes(x=cut2, y=population, group=region, color=region)) +
##     geom_line()



df_viz_pop3 <- as_tibble(merge(df_viz, df_viz_pop2))
df_viz_pop3$rate <- df_viz_pop3$nbr_opened/(df_viz_pop3$population/1e+8)


p_abs <- ggplot(df_viz, aes(x=cut2, y=nbr_opened, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") 

p_rel <- ggplot(df_viz_pop3, aes(x=cut2, y=rate, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") 

library(ggpubr)
ggarrange(p_abs, p_rel, nrow = 2)



## atm it's per hundred million
## even the absolute peak of founding was less than 5 per 100 million
## could even do per billion
## but not good: US/EU don't have more than a billion -> hard to understand what 30 per billion means; 3 per 100 million is much easier to imagine 


## hehehe Europe super strong
## weird how active Europe europe is in 80s


## * scrap
## ** test different kinds of aggregation

## wave_length <- 4
## wv_ctr <- 0
## wv_nbr <- 1

## df_anls$wv <- 0

## ## label the waves based on wavelength, not super elegant but works

## for (yearx in unique(df_anls$year)){
##     wv_ctr <- wv_ctr + 1
##     print(c(wv_ctr, wv_nbr))
##     df_anls[which(df_anls$year == yearx),"wv"] <- wv_nbr
##     if (wv_ctr == wave_length){
##         wv_ctr <- 0
##         wv_nbr <- wv_nbr + 1}
##     }



## merge test start
## df_agg_test <- as_tibble(cbind(c(1,1,1,2,2,2,3,3,3), c(NA,NA,1,NA,2,3,NA,NA,NA)))
## names(df_agg_test) <- c("group", "value")
## df_agg_test$value[which(is.na(df_agg_test$value))] <- "NA"
## aggregate(value ~ group, df_agg_test, mean)

## ## can't get NAs into the aggregation if there's no non-NA value 
## ## probably have to aggregate separately with na.omit, and then merge back to overall df


## df_agg_gini <- as_tibble(aggregate(gini ~ countrycode + wv, df_anls, mean, na.action = na.omit))

## ## make some arbitrary aggregation to merge other aggregations back to
## df_agg_gini2 <- as_tibble(merge(df_agg_cnts, df_agg_gini, by=c('countrycode', 'wv'), all.x = TRUE))


## df_agg_pcap <- as_tibble(aggregate(gdp_pcap ~ countrycode + wv, df_anls, mean))


## merge test end 


## ahh, what happened was that since I aggregated the variables in the same function they all got down to gini level
## yeah even though stuff is not deleted, it still means that all entries are fully complete -> get reduced down to lowest value

## 



## ## use reduce to join the means together 
## df_agg_means <- as_tibble(Reduce(
##     function(x,y, ...) merge(x,y, all = TRUE),
##     lapply(c("gini", "gdp_pcap", "gdp_pcapk"), agger, )
## ))


## df_agg_cnts <- as_tibble(aggregate(nbr_opened ~ countrycode + wv, df_anls, sum))

## ## ggplot(df_agg_means, aes(x=wv, y=gini, group = countrycode, color = countrycode)) +
## ##     geom_line()

## df_agg <- as_tibble(merge(df_agg_cnts, df_agg_means, by=c("countrycode", "wv"), all.x = T))


## ## checking how many museums are covered 

## ## not removing anything: 449
## sum(df_anls$nbr_opened)

## ## whenever I do gini with 5 years, I only have 260 or 263?? opening events
## sum(na.omit(df_anls[,c("gini","nbr_opened")])$nbr_opened)

## ## filtering for gdp_pcap: still 388
## sum(na.omit(df_anls[,c("gdp_pcap","nbr_opened")])$nbr_opened)

## ## even aggregating gini to 5 years: only get to 345,
## sum(na.omit(df_agg[,c("gini","nbr_opened")])$nbr_opened)
## ## with 2: 298
## ## with 3: 327
## ## with 5: 345
## ## with 8 to 345
## ## with 10 to 364
## sum(na.omit(df_agg[,c("gdp_pcap","nbr_opened")])$nbr_opened)
## ## pcap with 10: 388

## ## where the fuck are the museums founded for which I don't have data

## ## ggplot(df_anls, aes(x=year, y=countrycode, size = nbr_opened)) +
## ##     geom_point()

## df_agg$pm_preds_na <- 0
## df_agg[which(is.na(df_agg$gini)),]$pm_preds_na <- 1



## ggplot(df_agg, aes(x=factor(wv), y=countrycode, size = nbr_opened, color = factor(pm_preds_na))) +
##     geom_point()

## ## seems especially Korea, Russia -> country specific, not time-period effect
## ## for wave_length 2: also germany, india, japan
## mis <- aggregate(pm_preds_na ~ countrycode, df_agg[which(df_agg$nbr_opened > 0),], sum)
## mis[order(mis$pm_preds_na),]

## ## gdp_pcap seem to be mostly korea

## ** gtsummary: written for rstudio 
## mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)
## t1 <- tbl_regression(mod1, exponentiate = TRUE)
## gtsave(t1, filename = paste0(TABLE_DIR, "gtsummarytest.tex"))


## library(gtsummary); library(gt); library(dplyr)

## trial %>%
##   select(trt, age, grade) %>%
##   tbl_summary(by = trt) %>%
##     add_p() %>%
##     gt::gtsave(filename = paste0(TABLE_DIR, "gtsummarytest.tex")

## ** parallelization test 

## https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
## reg_obj <- nbr_opened ~ nbr_opened_lag1 + (1 | countrycode)
## reg_objs <- rep(list(reg_obj), 4)

## library(parallel)
## res_objs <- mclapply(reg_objs, glmer.nb, data = df_lag4, mc.cores = 4)

## * literature table
con_obvz <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "obvz")

dbSendQuery(con_obvz, "set joined_subquery_requires_alias=0")

lit_df <- as_tibble(dbGetQuery(con_obvz, "SELECT child, parent FROM (
    SELECT DISTINCT(child) AS child FROM bc WHERE parent IN ['ecology', 'legitimation', 'competition', 'founding', 'contagion', 'diffusion', 'density']
  ) JOIN (SELECT parent, child FROM bc WHERE parent NOT IN ['cls_papers', 'cls_toread', 'sbcls_B', 'sbcls_A', 'sbcls_C', 'cls_orgform']) USING child"))


lit_df$ctr <- 1


lit_df_cast <- as_tibble(dcast(lit_df, child ~ parent, fill=0))


## only selecting top 100 terms 
colsums <- apply(lit_df_cast[,c(2:len(lit_df_cast))], 2, sum)
colnames <-names(colsums[rev(order(colsums))][c(1:50)])
colpos <- which(names(lit_df_cast) %in% colnames)

rowsums <- apply(lit_df_cast[,c(2:len(lit_df_cast))], 1, sum)

lit_df_cast2 <- lit_df_cast[rev(order(rowsums))[c(1:100)],c(1,colpos)]

lit_df_melt <- as_tibble(melt(lit_df_cast2, id=c('child')))


## idk if much value
## doesn't help that paper titles are hard af to read, fucking hurts my eyes
## probably should use Holst directly, not some shitty wrapper like cluster_matrix
## also these manual reordering is bloat

lit_df_cast3 <- as.data.frame(lit_df_cast2)

rownames(lit_df_cast3) <- lit_df_cast3$child
lit_df_cast3 <- lit_df_cast3[,-1]


d_papers <- dist(lit_df_cast3)
d_tags <- dist(t(lit_df_cast3))
clust_papers <- hclust(d_papers, method = "ward.D2")
clust_tags <- hclust(d_tags, method = "ward.D2")


clust_cut_papers <- cutree(clust_papers,8)
clust_cut_tags <- cutree(clust_tags,6)

table(clust_cut_papers)
table(clust_cut_tags)

## should be able to change the factor order to that they correspond to clusters
lit_df_melt$child_srt <- factor(lit_df_melt$child, levels=names(clust_cut_papers[order(clust_cut_papers)]))
lit_df_melt$variable_srt <- factor(lit_df_melt$variable, levels=names(clust_cut_tags[order(clust_cut_tags)]))


## can draw boundaries now, still need function to calculate them properly 

boundary_df <- data.frame(matrix(ncol=4, nrow=0))
names(boundary_df) <- c("xmin", "ymin", "xmax", "ymax")

for (i in seq(1, max(clust_cut_papers))){
    for (k in seq(1, max(clust_cut_tags))) {
        xmin <- len(which(clust_cut_tags < k)) + 0.5
        ymin <- len(which(clust_cut_papers < i)) + 0.5

        xmax <- len(which(clust_cut_tags < k+1)) + 0.5
        ymax <- len(which(clust_cut_papers < i+1)) + 0.5

        boundary_df[nrow(boundary_df)+1,] <- c(xmin, ymin, xmax, ymax)
    }
}

## geom_rect(mapping = aes(xmin=0.5,xmax=50, ymin=0.5,ymax=10), size=0.1, fill=alpha("grey", 0), color="green")


p1 <- ggplot(NULL)+
    geom_tile(data=lit_df_melt, mapping = aes(x = variable_srt, y=child_srt, fill=value)) +
    geom_rect(data=boundary_df, mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=alpha("grey", 0), color="grey") +
    theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), axis.title.x = element_blank())

pdf(paste0(FIG_DIR, "lit_table.pdf"), width=12,  height = 12)
print(p1)
dev.off()
   


## flipped version 
ggplot(NULL)+
    geom_tile(data=lit_df_melt, mapping = aes(x = child_srt, y=variable_srt, fill=value))+
    geom_rect(data=boundary_df, mapping = aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill=alpha("grey", 0), color="grey") +
    theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), axis.title.x = element_blank())


hist(aggregate(value ~ variable, lit_df_melt, sum)$value)


## some fit stats
## 


wss2 <- function(d) {
      sum(scale(d, scale = FALSE)^2)
  }


wrap <- function(i, hc, x) {
    cl <- cutree(hc, i)
    spl <- split(x, cl)
    wss <- sum(sapply(spl, wss2))
    wss
}

hclustAIC <- function(to_cut, n, dfx){
    ## n2 <- nrow(clust.df)
    n2 <- len(clust_papers[[2]])
    m <- 2
    D <- wrap(n, hc=to_cut, x=dfx)

    return(data.frame(
        n = n,
        AIC = D + 2*m*n,
        BIC = D + log(n2)*m*n))
}




clust_res_papers <- lapply(seq(1,12), function(x) hclustAIC(clust_papers, x, lit_df_cast3))
clust_res_papers2 <- do.call(rbind, clust_res_papers)
clust_res_papers2$topic <- "papers"

clust_res_tags <-  lapply(seq(1,12), function(x) hclustAIC(clust_tags, x, t(lit_df_cast3)))
clust_res_tags2 <- do.call(rbind, clust_res_tags)
clust_res_tags2$topic <- "tags"

clust_res_cbn <- rbind(clust_res_papers2, clust_res_tags2)
clust_res_melt <- as_tibble(melt(clust_res_cbn, id=c("n", "topic")))


ggplot(clust_res_melt, aes(x=n, y=value, group=interaction(topic, variable), color=interaction(topic, variable))) +
    geom_line()


subsidies_lit_df <- as_tibble(dbGetQuery(con_obvz, "SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['state-support', 'subsidies', 'finance', 'policy', 'donation', 'tax-deduction', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child"))


subs_cast <- dcast(subsidies_lit_df, child ~ parent)
subs_cast$fully_read <- subs_cast$cls_papers
subs_cast <- subs_cast[,-c(which(names(subs_cast) %in% c("cls_papers", "cls_toread")))]

subs_cast_fltrd <- subs_cast[which(rowSums(subs_cast[,c(2:7)]) > 0),]
nrow(subs_cast_fltrd)

write.csv(subs_cast_fltrd, paste0(TABLE_DIR, "subsidies_lit.csv"))




## should also add tags -> columns
## also whether fully read or not 
