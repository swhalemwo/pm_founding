## ** libraries/opening data
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)
library(lme4)
library(texreg)

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
                           multi_inner,
                           by=c('countrycode', 'year'),
                           all.x= TRUE))




## lag gdp by a year, also number of openings for good measure, 


## overly messy way of lagging variables that creates intermediary vars because mutate/lag doesn't accept variablies as input
for (varx in c("gdp_pcap", "gini", "nbr_opened")){
    lag_name = paste(varx, "_lag1", sep = "")
    ## eval(parse("lag_name"))
    df_anls$var_to_lag <- df_anls[,c(varx)]
    df_anls[,"var_lagged"] <- mutate(group_by(df_anls, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
    df_anls[,lag_name] <- df_anls$var_lagged
    df_anls <- df_anls[,-which(names(df_anls) %in% c("var_to_lag", "var_lagged"))]
    }





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


found.fe <- lmer(nbr_opened ~ gdp_pcap_lag1 + nbr_opened_lag1 + gini_lag1 + (1 | countrycode), data = df_anls)
found.fe_wo_gini <- lmer(nbr_opened ~ gdp_pcap_lag1 + nbr_opened_lag1  + (1 | countrycode), data = df_anls)

summary(found.fe)
screenreg(found.fe)

screenreg(list(found.fe,found.fe_wo_gini))


## ** poisson

## *** glmer (lme4)
found.poi <- glmer(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + (1 | countrycode), data = df_anls, family=poisson)

found.poi1 <- glmer(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_anls, family=poisson)
found.poi2 <- glmer(nbr_opened ~ nbr_opened_lag1  + gdp_pcap_lag1 + (1 | countrycode), data = df_anls, family=poisson)
found.poi3 <- glmer(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + (1 | countrycode), data = df_anls, family=poisson)

found.poi4 <- glmer(nbr_opened ~ nbr_opened_lag1  + gini_lag1 + (1 | countrycode), data = df_anls, family=poisson)
found.poi5 <- glmer(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + gini_lag1 + (1 | countrycode), data = df_anls, family=poisson)


summary(found.poi)
screenreg(list(found.poi1,found.poi2,found.poi3,found.poi4, found.poi5))


## glmmPQL pretty much the same results
found.poi1b <- glmmPQL(nbr_opened ~ nbr_opened_lag1, random = list(countrycode = ~1), data = df_anls, family=poisson)

found.poi5b <- glmmPQL(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + gini_lag1, random = list(countrycode = ~1), data = df_anls, family=poisson)


screenreg(list(found.poi1, found.poi1b, found.poi5, found.poi5b))



## *** pglm
library(plm)
## seems like poisson/negative binomial is implemented in https://cran.r-project.org/web/packages/pglm/pglm.pdf
## yves croissant, wrote some b

library(pglm)

found.pglm.poi1 <- pglm(nbr_opened ~ nbr_opened_lag1, data = df_anls,
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



## ** negative binomial

## *** example
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


## *** pglm

found.pglm.nb1 <- pglm(nbr_opened ~ nbr_opened_lag1, data = df_anls,
                        family=negbin,
                        model = "within",
                        index = "countrycode")

found.pglm.nb2 <- pglm(nbr_opened ~ nbr_opened_lag1 + gdp_pcap_lag1, data = df_anls,
                    family=negbin,
                    model = "within",
                    index = "countrycode")

found.pglm.nb3 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1), data = df_anls,
                    family=negbin,
                    model = "within",
                    index = "countrycode")

found.pglm.nb4 <- pglm(nbr_opened ~ nbr_opened_lag1 + gini_lag1, data = df_anls,
                    family=negbin,
                    model = "within",
                    index = "countrycode")

found.pglm.nb5 <- pglm(nbr_opened ~ nbr_opened_lag1 + log(gdp_pcap_lag1) + gini_lag1, data = df_anls,
                    family=negbin,
                    model = "within",
                    index = "countrycode")

screenreg(list(found.pglm.nb1,found.pglm.nb2,found.pglm.nb3,found.pglm.nb4,found.pglm.nb5))

## *** glmer.nb

found.nb1 <- glmer.nb(nbr_opened ~  + (1 | countrycode), data = df_anls)

found.nb1 <- glmer.nb(nbr_opened ~ nbr_opened_lag1 + (1 | countrycode), data = df_anls)
found.nb2 <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcap_lag1 + (1 | countrycode), data = df_anls)
found.nb3 <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + (1 | countrycode), data = df_anls)
found.nb4 <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gini_lag1 + (1 | countrycode), data = df_anls)
found.nb5 <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + gini_lag1 + (1 | countrycode), data = df_anls)



screenreg(list(found.nb1,found.nb3,found.nb4,found.nb5))

screenreg(list(found.nb1,found.pglm.nb1,found.nb3,found.pglm.nb3,found.nb4,found.pglm.nb4,found.nb5,found.pglm.nb5))

## model 2 (not log-transfomed gdp_pcap) doesn't work
## think the differences are smaller than in poisson:
## at least the general direction, but still bunch of differences with significance in nbr_opened_lag1 (between 3 and 4), gini (5)

## but i still don't know what's happening, and why things are different
## also need to understand the techniques: negative binomial/poisson especially

## *** interpreation
## can exp(coefs) to get ratios with a unit change
## A country's rate of founding PMs increases by exp(0.78) = 2.18 for each log(GDP) point
## A country's founding rate of PMs increases by exp(0.07) = 1.07 (7%) for each gini point

df_anls$gdp_pcapk_lag1 <- df_anls$gdp_pcap_lag1/1000

found.nb5x <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + gdp_pcapk_lag1 + gini_lag1 + (1 | countrycode), data = df_anls)

screenreg(found.nb5x)
## for each 1k increase in average GDP, founding rate increases by exp(0.04) = 1.04 = 4%


## **** standardized effect sizes
## kinda sucks that there is no official way 

## standardized effects from: https://stats.stackexchange.com/questions/123366/lmer-standardized-regression-coefficients
lm.beta.lmer <- function(mod) {
   b <- fixef(mod)[-1]
   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
   sd.y <- sd(getME(mod,"y"))
   b*sd.x/sd.y
}

lm.beta.lmer(found.nb5x)

## https://cran.r-project.org/web/packages/effectsize/vignettes/from_test_statistics.html
library(effectsize)
anova(found.nb5x)
F_to_eta2(5.4014, 1)
## anova result looks different 

## https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
library(MuMIn)
r.squaredGLMM(found.nb5x)
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


## **** noise testing 
## see if i can get negative results by flipping GDP -> yup

df_anls$gdp_pcap_lag1_noise <- -jitter(df_anls$gdp_pcap_lag1, factor= 1000)
df_anls$gdp_pcap_lag1_noise <- df_anls$gdp_pcap_lag1_noise+max(df_anls$gdp_pcap_lag1,na.rm = T)
cor(df_anls$gdp_pcap_lag1_noise,df_anls$gdp_pcap_lag1, use="complete.obs")

found.nb5_noise <- glmer.nb(nbr_opened ~ nbr_opened_lag1  + log(gdp_pcap_lag1) + log(gdp_pcap_lag1_noise) + gini_lag1 + (1 | countrycode), data = df_anls)
screenreg(list(found.nb5, found.nb5_noise))



## ** visualization/inspection

FIG_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/figures/"

pdf(paste(FIG_DIR, "resids.pdf", sep=""), height = 4, width = 8)
par(mfrow=(c(1,2)))
plot(found.nb1)
plot(found.poi1)
dev.off()

library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

plot_summs(found.nb1, found.nb3, found.nb4, found.nb5, plot.distributions = T)
## plot_summs looks nice, but idk if I shouldn't rather write my own ggplot visualization
## also no support for the pglm models -> idk if plot_summs has generic class that can be filled wiht arbitrary values
## also need standardized stuff
## also not that good at comparing models with different variables
## plotting distributions gets very full when more than a handful


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


