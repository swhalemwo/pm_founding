
## * source other files
PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_org_pop.R")) ## startup: libraries, global vars
source(paste0(SCRIPT_DIR, "custom_funcs.R")) # random utils
source(paste0(SCRIPT_DIR, "wb_api.R")) ## World Bank data, has to be run before sourcing base_df_creationn since it provides the country-year structure
source(paste0(SCRIPT_DIR, "base_df_creation.R")) # function to read in excel data
source(paste0(SCRIPT_DIR, "WID_setup_and_checks.R"))
source(paste0(SCRIPT_DIR, "viz_opngs.R"))
source(paste0(SCRIPT_DIR, "oecd_api.R"))

source(paste0(SCRIPT_DIR, "mow.R"))
source(paste0(SCRIPT_DIR, "tax_incentives.R"))

df_excl <- create_excel_df()
df_open <- aggregate_openings(df_excl)
df_wb <- get_WB_data(c("NY.GDP.PCAP.CD", "SP.POP.TOTL"))
df_anls <- create_anls_df(df_wb, df_open)



## *** remaining variables construction, not directly related to structure, have to be functionalized somewhere

df_anls$wv <- 0

df_anls$gdp_pcapk <- df_anls$gdp_pcap/1000

## check if WB and my country codes are the same, they are 
## x <- merge(df_gdp_pcap2[,c("V1", "V2")],
##       unique(df_country_year_agg[,c("country", "countrycode")]),
##       by.x = c("V2"),
##       by.y = c("countrycode"))
## x[which(x$V1 != x$country),]

## cumulative number of opened
df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$iso3c, FUN = cumsum)


df_anls$nbr_opened_cum_sqrd <- (df_anls$nbr_opened_cum^2)/100
## have to divide by 100 otherwise R glmer.nb complains


## PMs opened per 1m people -> rate
df_anls$nbr_opened_prop <- df_anls$nbr_opened/(df_anls$population/1000000)

## iceland, monaco, cyprus LUL
filter(df_anls, nbr_opened_prop > 1)

## wealth_cur_df <- get_wealth_df()
## df_wealth <- get_wealth_cutoff_pct(wealth_cur_df, 5e+06)


## ** checking NAs

aggregate(gdp_pcap ~ countrycode, data = df_anls, function(x){sum(is.na(x))}, na.action = NULL)
## around 10% missing :(
## might have to kick out some countries/years


unique(df_open$countrycode)[which(unique(df_open$countrycode) %!in% (unique(df_gdp_pcap_molt$countrycode)))]
## seems ok,
## taiwan not separate country in WB.. just 1 PM tho, so shouldn't be big impact



    



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


## * pdf stuff
library(pdftools)
READINGS_DIR <- "/home/johannes/Dropbox/readings/"
x <- pdf_text(paste0(READINGS_DIR, "Walker_2019_collector.pdf"))

some_page <- x[[1]]

## would be necessary to make all the aliases of museum names


