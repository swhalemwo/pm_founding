
## * source other files
PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "org_pop_startup.R")) ## startup: libraries, global vars
source(paste0(SCRIPT_DIR, "custom_funcs.R")) # random utils
source(paste0(SCRIPT_DIR, "wb_api.R")) ## World Bank data, has to be run before sourcing base_df_creationn since it provides the country-year structure
source(paste0(SCRIPT_DIR, "base_df_creation.R")) # function to read in excel data
source(paste0(SCRIPT_DIR, "WID_setup_and_checks.R"))


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






## ** visualization




## *** n-year cuts

year_selector <- function(x)(
    # convert cuts back to years
    substring(x, 2,5))

set_geo_level <- function(df, geo_level) {
    #' set geo_level to either country or geo

    if (geo_level == "region") {
    
        ## df_plt$geo <- countrycode(df_plt$iso3c, "iso3c", "geo")
        df$geo_level <- countrycode(df$iso3c, "iso3c", "region")

    } else {
        df$geo_level <- df_plt$iso3c
    }
    return(df)
}


set_time_level <- function(df, time_level, duration) {
    #' setting time level: either cut or year (for rolling mean) calculation

    if (time_level == "cut") {
        ## just population calculation: could go into separate function, if rates are requested
        
        df$cut <- cut(df$year, seq(min(df$year), max(df$year)+5, by = duration))
        df$time_level <- as.numeric(sapply(as.character(df$cut), year_selector))
        ## df_viz <- as_tibble(aggregate(nbr_opened ~ region + cut2, df_plt, sum))

    } else {
        ##  maybe I have to put in function application here, not sure if I can put it properly away
        ## try first tho
        df$time_level <- df_plt$year
    }
    return(df)
}

prep_pop <- function(df_plt) {
    #' prepare the population variable: mean by time period, then sum by geo level (if it is region)
    
    ## founding rates
    ## first country mean per cut
    ## then region sum
    ## always need this one: first aggregate by country + time + geo with mean 
    df_pop_cry_mean <- as_tibble(aggregate(SP.POP.TOTL ~ iso3c + time_level + geo_level, df_plt, mean))

    ## df_viz_pop_agg$region <- countrycode(df_viz_pop_agg$iso3c, "iso3c", "region")

    ## countrycode(unique(filter(df_viz_pop1, region == "South Asia")$iso3c), "iso3c", "country.name")
    df_pop_reg_sum <- as_tibble(aggregate(SP.POP.TOTL ~ geo_level + time_level, df_pop_cry_mean, sum))
    ## ggplot(df_pop_reg_sum, aes(x=time_level, y=SP.POP.TOTL, group=geo_level, color=geo_level)) +
    ##     geom_line()

    return(df_pop_reg_sum)
    }


agg_opnd_cnts <- function(df_plt, time_level, duration) {
    #' aggregate the opening counts

    ## always sum by geo_level + time_level, is enough if cuts are provided as time_level
    df_plt_opnd <- as_tibble(aggregate(nbr_opened ~ geo_level + time_level, df_plt, sum))

    ## if time_level is ra, apply custom rollmean function (time_level is year in that case)
    if (time_level != "cut") {

        df_plt_opnd <- df_plt_opnd %>%
            group_by(geo_level) %>%
            mutate(nbr_opened_ra = rollmean_custom(nbr_opened, win_len = duration))
    }
    
    
    return(df_plt_opnd)
}


viz_cuts <- function(df_anls, time_level, duration, geo_level, cumulative, max_lines=12){
    #' visualize the founding
    #' 
    #' time_level: cut or rolling mean
    #' duration: length of cut/rolling mean
    #' geo_level: region or country
    #' cumulative: whether using per period or cumulative
    #' max_lines: if country
    #' also need something whether it should use the absolute counts or the population ratio 

    ## idk this becomes a whole bunch of parameters
    ## has the potential to be pretty good tho: much flexibility
    
    ## gonna be difficult to process each step independently
    ## may not be possible without much nesting
    ## also need consistent color schemes
    
    geo_level = "region"
    geo_level = "country"
    time_level <- "cut"
    time_level <- "rolling_mean"
    duration <- 10
    df_plt <- df_anls



    df_plt <- set_geo_level(df_plt, geo_level)
    df_plt <- set_time_level(df_plt, time_level, duration)
    
    df_pop_reg_sum <- prep_pop(df_plt)
    df_plt_opnd <- agg_opnd_cnts(df_plt, time_level, duration)

    ## ggplot(df_plt_opnd, aes(x=time_level, y=nbr_opened_ra, color = geo_level)) +
    ##     geom_line()



df_viz_pop3 <- as_tibble(merge(df_viz, df_viz_pop2))
df_viz_pop3$rate <- df_viz_pop3$nbr_opened/(df_viz_pop3$SP.POP.TOTL/1e+8)


p_abs <- ggplot(df_viz, aes(x=cut2, y=nbr_opened, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(x="year (3 year aggregate)", y= "number opened", title = 'absolute')

p_rel <- ggplot(df_viz_pop3, aes(x=cut2, y=rate, group = region, color = region)) +
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="foundings per 100m", x="year (3 year aggregate)", title = 'relative')


pdf(paste0(FIG_DIR, "foundings_cut3.pdf"), height = 9, width = 9)
ggarrange(p_abs, p_rel, nrow = 2)
dev.off()

## *** rolling average


df_viz_rol1 <- as_tibble(aggregate(cbind(SP.POP.TOTL, nbr_opened) ~ region + year, df_plt[,c("iso3c", "year", "region", "SP.POP.TOTL", "nbr_opened")], sum))

ROLLING_AVG_LEN <- 4

df_viz_rol2 <- df_viz_rol1  %>% group_by(region) %>% mutate(nbr_opened_rollavg = runMean(nbr_opened, ROLLING_AVG_LEN))
df_viz_rol2 <- df_viz_rol2  %>% group_by(region) %>% mutate(population_rollavg = runMean(SP.POP.TOTL, ROLLING_AVG_LEN))

df_viz_rol2$rate_rollavg <- df_viz_rol2$nbr_opened_rollavg/(df_viz_rol2$population_rollavg/1e+8)

p_ra_abs <- ggplot(df_viz_rol2, aes(x=year, y=nbr_opened_rollavg, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs (y = "number opened (4 year rolling average)", title = "absolute")

p_ra_rel <- ggplot(df_viz_rol2, aes(x=year, y=rate_rollavg, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="foundings per 100m (4 year rolling average)", title = "relative")

pdf(paste0(FIG_DIR, "foundings_ra4.pdf"), height = 9, width = 9)
ggarrange(p_ra_abs, p_ra_rel, nrow = 2)
dev.off()


## filter(df_viz_rol2, year > 2017 & region == "Middle East & North Africa")


## atm it's per hundred million
## even the absolute peak of founding was less than 5 per 100 million
## could even do per billion
## but not good: US/EU don't have more than a billion -> hard to understand what 30 per billion means; 3 per 100 million is much easier to imagine 


## hehehe Europe super strong
## weird how active Europe europe is in 80s


## *** rolling average country-wise rate
df_viz_rol_cry <- as_tibble(aggregate(cbind(SP.POP.TOTL, nbr_opened) ~ iso3c + year, df_plt[,c("iso3c", "year", "region", "SP.POP.TOTL", "nbr_opened")], sum))

ROLLING_AVG_LEN <- 5

df_viz_rol_cry <- df_viz_rol_cry %>%
    group_by(iso3c) %>%
    mutate(population_rollavg = rollmean_custom(SP.POP.TOTL, win_len=ROLLING_AVG_LEN, orientation = "left"),
           nbr_opened_rollavg = rollmean_custom(nbr_opened, win_len=ROLLING_AVG_LEN, orientation = "left")
           )


df_viz_rol_cry$rate_rollavg <- df_viz_rol_cry$nbr_opened_rollavg/(df_viz_rol_cry$population_rollavg/1e+8)

country_max <- aggregate(nbr_opened_cum ~ iso3c, df_plt, max)
country_max_codes <- country_max[rev(order(country_max$nbr_opened_cum))[c(1:12)],]$iso3c


df_viz_rol_cry <- filter(df_viz_rol_cry, iso3c %in% country_max_codes)

df_viz_rol_cry$country <- countrycode(df_viz_rol_cry$iso3c, "iso3c", "country.name")

p_ra_rate_cry <-
    ggplot(df_viz_rol_cry, aes(x=year, y=rate_rollavg, group=country, color=country)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y=paste0("foundings per 100m (", ROLLING_AVG_LEN ," years rolling average)"))

## p_ra_rate_cry

## *** rolling average country-wise absolute count 
p_ra_cnt_cry <- ggplot(df_viz_rol_cry, aes(x=year, y=nbr_opened_rollavg, group=country, color=country)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y=paste0("foundings per country (", ROLLING_AVG_LEN ," years rolling average)"))

pdf(paste0(FIG_DIR, "foundings_country_cnt_and_rate.pdf"), height = 9, width = 9)
ggarrange(p_ra_cnt_cry, p_ra_rate_cry, nrow = 2)
dev.off()




## *** cumulative

df_viz_rol1$nbr_opened_cum <- ave(df_viz_rol1$nbr_opened, df_viz_rol1$region, FUN = cumsum)

df_viz_rol1 <- df_viz_rol1 %>%
    group_by(region) %>%
    mutate(nbr_opened_max = max(nbr_opened_cum))

df_viz_rol1 <- mutate(df_viz_rol1, prop_cum = nbr_opened_cum/nbr_opened_max)

df_viz_rol1$alpha <- 1
df_viz_rol1[which(df_viz_rol1$nbr_opened_max < 50),'alpha'] <- 0.9


p_cum_abs <- ggplot(df_viz_rol1, aes(x=year, y=nbr_opened_cum, group=region, color=region)) + 
    geom_line(size=1) +
    scale_color_brewer(palette="Dark2") +
    labs(y="number opened (cumulative)")

p_cum_rel <-
    ggplot(df_viz_rol1, aes(x=year, y=prop_cum, group=region, color=region, alpha=alpha)) + 
    geom_line(size=1) +
    scale_alpha_continuous(range = c(0.3, 1), guide=FALSE) + 
    scale_color_brewer(palette="Dark2") +
    labs(y="proportion opened", caption = "regions with less PMs de-emphasized")


pdf(paste0(FIG_DIR, "foundings_cum.pdf"), height = 9, width = 9)
ggarrange(p_cum_abs, p_cum_rel, nrow = 2)
dev.off()

## europe slightly in front of NA, but East Asia (Korea, China) at same stage/ahead of Europe
## maybe due to korea? 
aggregate(nbr_opened ~ country, filter(df_plt, region == "East Asia & Pacific"), sum)
## -> do for countries with most PMs



## try selecting countries with highest PM concentration per population, end up with Monaco, and mostly other small countries that have like 2-4 PMs: "MCO" "ISL" "CYP" "BEL" "CHE" "KOR" "DEU" "EST" "LBN" "NLD" "AUT" "GRC"
## df_plt$rate_opened_cum <- df_plt$nbr_opened_cum/(df_plt$population/1e+08)
## country_max_prop <- aggregate(rate_opened_cum ~ countrycode, df_plt, max)
## country_max_prop_codes <- country_max[rev(order(country_max_prop$rate_opened_cum))[c(1:12)],]$countrycode
## country_max_codes <- country_max[rev(order(country_max_prop$rate_opened_cum))[c(1:12)],]$countrycode


df_plt_cry <- filter(df_plt, countrycode %in% country_max_codes)

df_plt_cry <- df_plt_cry %>%
    group_by(countrycode) %>%
    mutate(nbr_opened_max = max(nbr_opened_cum))

df_plt_cry$prop_cum <- df_plt_cry$nbr_opened_cum/df_plt_cry$nbr_opened_max

df_plt_cry$lt <- recode(df_plt_cry$region,
    "Europe & Central Asia" = "twodash",
    "East Asia & Pacific" = "solid",
    "North America" = "F1")

df_plt_cry$country <- countrycode(df_plt_cry$countrycode, "iso3c", "country.name")

p_cry_cum_abs <- ggplot(df_plt_cry, aes(x=year, y=nbr_opened_cum, group=country, color=country)) +
    geom_line(size=2) +
    scale_color_brewer(palette = "Paired") +
    labs(y="number opened (cumulative)")

p_cry_cum_rel <-
    ggplot(df_plt_cry, aes(x=year, y=prop_cum, group=country, color=country, linetype=region)) +
    geom_line(size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(y="proportion opened", caption = "12 countries with most PMs (70% of PMs)")


## also get cumulative rate: end is how many per 100m are there atm 
df_plt_cry$rate_opened_cum <- df_plt_cry$nbr_opened_cum/(df_plt_cry$population/1e+07)

p_cry_cum_rate <-
    ggplot(df_plt_cry, aes(x=year, y=rate_opened_cum, group=country, color=country)) +
    geom_line(size=2) +
    scale_color_brewer(palette = "Paired") +
    labs(y="number opened per 10m population (cumulative)")



pdf(paste0(FIG_DIR, "foundings_country_cumulative.pdf"), height = 12, width = 9)
ggarrange(p_cry_cum_abs, p_cry_cum_rel, p_cry_cum_rate, nrow = 3)
dev.off()

## China really late, Korea really early -> there's much variation within region
## but no idea how to visualize that.. ribbons? it's not an uncertainty estimate, i know the numbers
## think it makes rather sense to plot countries individually, maybe line-type by region 
## I don't trust these korea numbers
## US/Germany: have the most, and are weirdly in the middle, would have expected them to be leaders
## could be data issue: only good for them, and for the others the curves are fluctuating randomly because coverage incomplete






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



## * oecd
## ** library access, doesn't have labels tho 
library(OECD)
df_oced <- as_tibble(get_datasets())
## 1495 datasets noice

df_stan <- as_tibble(
    get_dataset("STANI4_2020", start_time = 2010, end_time = 2011,
                filter = list(c("DEU"))
                ))
## there's probably some query limit on the number of rows




dfx <- as_tibble(get_dataset("EPL_OV", 
                   filter = list(c("DEU", "FRA"), 
                                 c("EPRC_V1", "EPRC_V2")), 
                   start_time = 2008, end_time = 2010))


wdix <- read_dta("/home/johannes/ownCloud/wid/wid_2018_report/Computer Codes/Global Wealth Inequality/gpinterized.dta")
## only about   CN   FR   GB   US   WO

wdix <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/wid-data-25102021.dta"))
wdix_hist <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/WO_hist.dta"))

dfx2 <- as_tibble(get_dataset("SNA_TABLE11",
                              start_time = 2010, end_time = 2011,
                              filter = list(c("DEU"))
                ))

## ** sdmx parsing tests

SDMX_DIR <- "/home/johannes/ownCloud/oecd/SDMX/"

for (idx in df_oced$id){
    sdmx_url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", idx)
    dest_file <- paste0(SDMX_DIR, idx, ".xml")
    download.file(sdmx_url, dest_file)
}


library(rsdmx)

idx <- "STANI4"

sdmx_test <- readSDMX(dest_file, isURL = FALSE)


slotNames(sdmx_test)
## [1] "organisationSchemes" "concepts"            "codelists"          
## [4] "datastructures"      "xmlObj"              "schema"             
## [7] "header"              "footer"             
## these 8 overall slots seem to be general thing? 


cls <- slot(sdmx_test, "codelists")
slotNames(cls)
typeof(slot(cls, "codelists")) ## a list
len(slot(cls, "codelists"))


codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id")) #get list of codelists
codelist <- as.data.frame(slot(sdmx_test, "codelists"), codelistId = "CL_STANI4_IND") #get a codelist

## sdmx basically seems like annoyingly formatted  dict

codelist$idx <- idx

SDMX_TBL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_based_tables/"

write.csv(codelist, paste0(SDMX_TBL_DIR, idx, ".csv"))

## ** sdmx parsing proc

files_there <- list.files(SDMX_DIR)

SDMX_FAIL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_parsing_fail"
SDMX_FAIL_FILE <- paste0(SDMX_FAIL_DIR, "/fails.csv")

proc_codelist <- function(dsd, codelistx, sdmx_id) {
    #' convert sdmx codelist into df, outsourced to own function for better trycatching
    codelist_df <- as.data.frame(slot(dsd, "codelists"), codelistId = codelistx)
    names_codelist_df <- names(codelist_df)
    codelist_df$codelist <- codelistx
    codelist_df$sdmx_id <- sdmx_id
    ## reorder the columns so that I can easier grep/awk the databases/codelists where culture terms occur
    codelist_df <- codelist_df[,c("sdmx_id", "codelist", names_codelist_df)]

    filename <- paste0(SDMX_TBL_DIR, sdmx_id, "---", codelistx, ".csv")
    write.csv(codelist_df, filename)
}


proc_sdmx_file <- function(sdmx_file){
    sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
    dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
    ## print(slotNames(dsd)) overall slotnames are the same
    cls <- slot(dsd, "codelists")
    codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
    ## print(codelists) ## not every SDMX has the same codelist, unsurprisingly

    for (codelistx in codelists){
        print(codelistx)
        res <- tryCatch(
            proc_codelist(dsd, codelistx, sdmx_id),
            error=function(e) {
                FAILED_LIST <- c(failed_list, c(sdmx_id, codelistx))
                ## print(c(failed_list, c(sdmx_id, codelistx)))

                ## save which files failed to parse 
                write.table(paste(sdmx_id, codelistx, sep=","), file=SDMX_FAIL_FILE, append=TRUE,
                            col.names = FALSE, row.names = FALSE, quote = FALSE)
                
                }
            
        )
        ## codelist_df <- as.data.frame(slot(dsd, "codelists"), codelistId = codelistx)

        ## names_codelist_df <- names(codelist_df)
        ## codelist_df$codelist <- codelistx
        ## codelist_df$sdmx_id <- sdmx_id
        ## ## reorder the columns so that I can easier grep/awk the databases/codelists where culture terms occur
        ## codelist_df <- codelist_df[,c("sdmx_id", "codelist", names_codelist_df)]

        ## filename <- paste0(SDMX_TBL_DIR, sdmx_id, "---", codelistx, ".csv")
        ## write.csv(codelist_df, filename)
        
        ## print(codelist_df)
        ## print(paste(len(names(codelist_df)), paste(names(codelist_df), collapse = ", ")))
        ## not all codelist_dfs of the same sdmx file have the same variables,
        ## e.g. are about country, time, observation status,
        ## the "indicator" column seems to have all kinds of different names
        ## -> have to print everything and filter 
    }
}

mclapply(files_there, proc_sdmx_file, mc.cores = 6)

## ** working with parsing results
## get files with grepping in /home/johannes/ownCloud/oecd/sdmx_based_tables

sdmx_tables_musem <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'museum'"),
    intern = TRUE)
    
sdmx_tables_cultural_services <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'cultural services'"),
    intern = TRUE)

sdmx_tables_cultural <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv ' cultural'"),
    intern = TRUE)


relevant_sdmx_tables <- Reduce(union, list(sdmx_tables_musem, sdmx_tables_cultural_services, sdmx_tables_cultural))

relevant_sdmx_tables <- c(
    "AEA---CL_AEA_ACTIVITY.csv",
    "BIMTS_CPA---CL_BIMTS_CPA_CPA_VER_2_1.csv",
    "DIOC_OCCUPATION_DET---CL_DIOC_OCCUPATION_DET_DET_OCCUP.csv",
    "FATS_OUT3_SERV---CL_FATS_OUT3_SERV_SERV.csv",
    "FDI_CTRY_IND_SUMM---CL_FDI_CTRY_IND_SUMM_ECO_ACT.csv",
    "FDI_INC_IND---CL_FDI_INC_IND_ECO_ACT.csv",
    "FDI_POS_IND---CL_FDI_POS_IND_ECO_ACT.csv",
    "ERTR_ACC---CL_ERTR_ACC_ACT.csv",
    "FDI_FLOW_IND---CL_FDI_FLOW_IND_ECO_ACT.csv",
    "FATS_IN3_SERV---CL_FATS_IN3_SERV_SERV.csv",
    "FDI_CTRY_ECO_HIST---CL_FDI_CTRY_ECO_HIST_ECO_ACT.csv",
    "NCM_LIVE---CL_NCM_LIVE_INDICATOR.csv",
    "NCM_STAGING---CL_NCM_STAGING_INDICATOR.csv",
    "SNA_TABLE42---CL_SNA_TABLE42_ACTIVITY.csv",
    "SNA_TABLE8A_ARCHIVE---CL_SNA_TABLE8A_ARCHIVE_ACTIVITY.csv",
    "SNA_TABLE31---CL_SNA_TABLE31_ACTIVITY.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_TRANSACT.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_PRODUCT.csv",
    "SNA_TABLE6A_ARCHIVE---CL_SNA_TABLE6A_ARCHIVE_ACTIVITY.csv",
    "SDBS_BDI---CL_SDBS_BDI_SEC.csv",
    "SNA_TABLE7A_SNA93---CL_SNA_TABLE7A_SNA93_ACTIVITY.csv",
    "TEC1_REV4_COPY---CL_TEC1_REV4_COPY_SECTOR.csv",
    "SNA_TABLE8A---CL_SNA_TABLE8A_ACTIVITY.csv",
    "TEC5_REV4---CL_TEC5_REV4_SECTOR.csv",
    "SSIS_BSC---CL_SSIS_BSC_ISIC3.csv",
    "STANI4_2020---CL_STANI4_2020_IND.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_TRANSACT.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_PRODUCT.csv",
    "SNA_TABLE6A---CL_SNA_TABLE6A_ACTIVITY.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_TRANSACT.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_PRODUCT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_TRANSACT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_PRODUCT.csv",
    "SNA_TABLE7A_ARCHIVE---CL_SNA_TABLE7A_ARCHIVE_ACTIVITY.csv",
    "TEC9_REV4---CL_TEC9_REV4_SECTOR.csv",
    "SNA_TABLE9A---CL_SNA_TABLE9A_ACTIVITY.csv",
    "SSIS_BSC_ISIC4---CL_SSIS_BSC_ISIC4_ISIC4.csv",
    "STANI4_2016---CL_STANI4_2016_IND.csv",
    "SNA_TABLE41---CL_SNA_TABLE41_ACTIVITY.csv",
    "SDBS_BDI_ISIC4---CL_SDBS_BDI_ISIC4_SEC.csv",
    "TEC1_REV4---CL_TEC1_REV4_SECTOR.csv",
    "TEC6_REV4---CL_TEC6_REV4_SECTOR.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_ACTIVITY.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_PRODUCT.csv",
    "SNA_TABLE7A---CL_SNA_TABLE7A_ACTIVITY.csv",
    "STANI4---CL_STANI4_IND.csv",
    "STANINDICATORSI4---CL_STANINDICATORSI4_IND.csv")


sdmx_res_tbls <- lapply(relevant_sdmx_tables, function(x) as_tibble(read.csv(paste0(SDMX_TBL_DIR, x))))

sdmx_res_tbl_names <- unlist(lapply(sdmx_res_tbls, names))
table(sdmx_res_tbl_names)

col_names <- c("sdmx_id", "codelist", "id", "label.en", "description.en")

sdmx_res_cbn <- as_tibble(Reduce(function(x,y, ...) rbind(x[,col_names],y[,col_names]),sdmx_res_tbls))

## grepping multiple columns works best with apply, lapply on names(df) doesn't work properly for some reason 
sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl("museum", x)))>0),]


## seems using pipe I can use multiple terms with pipe 
sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl(" cultural|museum|cultural services", x)))>0),]

## sdmx_res_fltrd$id

## sdmx_res_fltrd[28,]
## hello "D90T92" my old friend
## let's see how to query you

## *** figuring out download 

x <- as_tibble(get_dataset("STANI4_2020", filter = list("D90T92")))

## maybe possible to first search which column is actually the indicator?

unlist(lapply(names(df_stan), function(x) grepl("D90T92", df_stan[,x])))
## for STAN, D90T92 is in the first column 

## trying to pass all for countries, just get specific indicator
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("AUTx"),c("PROD"), c("D90T92"))))
## this works for some reason

## passing all doesn't work tho :(
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("all"),c("PROD"), c("D90T92"))))

x <- as_tibble(get_dataset("STANI4_2020", filter="PROD.AUT", pre_formatted = TRUE))

## working url: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/AUT.PROD.D90T92/all
## not working: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/all.PROD.D90T92/all

## https://data.oecd.org/api/sdmx-json-documentation/: CAN USE EMPTY STRING TO SELECT ALL
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c("PROD"), c("D90T92"))))
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c(""), c("D90T92"))))


## AEA actually has the same structure huh 
x2 <- as_tibble(get_dataset("AEA", filter = list(c(""),c(""), c("R90-R92"))))

## FDI_POS_IND: doesn't have the same structure
## adding/removing empty filter strings doesn't seem to work

x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""), c("R91"))))
## actually does when just adding enough LUL 
x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""),c(""), c(""), c(""), c(""),  c(""), c(""), c("R91"))))

## *** functionalized download 


OECD_DATA_DIR <- "/home/johannes/ownCloud/oecd/api_data/"

download_oecd_df <- function(datasetx, filter_list){
    #' actual downloading 
    dfx <- get_dataset(datasetx, filter=filter_list)
    write.csv(dfx, paste0(OECD_DATA_DIR, datasetx))
    print("success")
    done <- TRUE
    }


datasets_already_there <- list.files(OECD_DATA_DIR)


options(timeout = 10)

download_oecd_dataset <- function(datasetx, idx) {
    print(paste0("dataset: ", datasetx, " id: ", idx))
    if (datasetx %!in% datasets_already_there){

        done <- FALSE
        filter_list <- list(c(idx))

        while (done==FALSE) {
            res <- tryCatch({
                print(paste0(filter_list, collapse = "-"))
                download_oecd_df(datasetx, filter_list)
                done <- TRUE
            },
            error=function(e) {
                print(paste0("error: ", length(filter_list)))
                filter_list <- rev(append(rev(filter_list), ""))
            })
            filter_list <- res
            Sys.sleep(2)
            
            if (length(filter_list) == 20) {
                done <- TRUE
                print("quit after too many tries")
            }
                
        }
    }
}      

## write to file to read from separate download_oecd.R
## write.csv(sdmx_res_fltrd, "/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv")


apply(sdmx_res_fltrd, 1, function(x) download_oecd_dataset(x["sdmx_id"], x["id"]))

## *** testing how to call data downloading best 

test_printer <- function(datasetx, idx){
    print(paste0("datasetx: ", datasetx))
    print(paste0("idx: ", idx))
    print("-----------")
    }

apply(sdmx_res_fltrd, 1, test_printer, datasetx=sdmx_id, idx=id)

apply(sdmx_res_fltrd, 1, function(x) test_printer(x$sdmx_id, x$id))
apply(sdmx_res_fltrd, 1, function(x) test_printer(x["sdmx_id"], x["id"]))

test_printer("asdf", "jjj")

apply(sdmx_res_fltrd, 1, function(x) print(names(x)))
apply(sdmx_res_fltrd, 1, function(x) print(x["sdmx_id"]))

## ** processing actual data
## maybe first get those that sufficient country coverage
## hope the label for the country code is the same, but probably isn't 


## find data set column sets 
namesx <- unlist(lapply(datasets_already_there, function(x) names(read.csv(paste0(OECD_DATA_DIR, x)))))
namesx_tbl <- table(namesx)
namesx_tbl[order(namesx_tbl)]

## country col is either: LOCATION (23), COU (12), COUNTRY(3)
## exceptions (so far): "BIMTS_CPA","TEC5_REV4"  ,"TEC6_REV4" ,"TEC9_REV4"

## find exceptions automatically where there's no column named "LOCATION", "COU", "COUNTRY"

namesx_exceptions <- c()

for (i in datasets_already_there){
    dfx <- read.csv(paste0(OECD_DATA_DIR, i))
    if (length(intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))) == 0) {
        print(i)
        namesx_exceptions <- c(namesx_exceptions, i)
    }
}

checked_exceptions <- c("BIMTS_CPA","TEC5_REV4","TEC6_REV4","TEC9_REV4","CRS1","CRS1_GREQ","DV_DCD_GENDER","DV_DCD_PPFD","MULTISYSTEM","RIOMARKERS","SOCX_DET","TSEC1","TSEC1_COPY")
lapply(setdiff(namesx_exceptions, checked_exceptions), print)

## check manually (should check whenever I change the datasets to be included)

## check snippet 
## dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR,


## first batch 
## "BIMTS_CPA"
## "TEC5_REV4"
## "TEC6_REV4"
## "TEC9_REV4"

## second batch 
## "CRS1" ## about agriculture anyways
## "CRS1_GREQ" ## about agriculture anyways
## "DV_DCD_GENDER" ## about agriculture anyways
## "DV_DCD_PPFD" ## about agriculture anyways 
## "MULTISYSTEM" ## about agriculture anyways 
## "RIOMARKERS"  ## about agriculture anyways
## "SOCX_DET" ## only 38 rows, seems all about spain 
## "TSEC1" ## not clear what it is, but only 134 rows
## "TSEC1_COPY" ## also unclear, but also only 134 rows



## names(dfx)

## sloppy coverage evaluations, better in functionalized form below (vague_cvrg)
## namesx_exceptions <- list("BIMTS_CPA"=1,"TEC5_REV4"  ,"TEC6_REV4"  ,"TEC9_REV4")
## for (i in datasets_already_there){
##     dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR, i)))
##     if (length(intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))) == 0) {
##         next
##     } else {
##         country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))
##         print(paste(i, nrow(unique(dfx[,country_col]))))
##     }
##     }

vague_cvrg <- function(namex){
    dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR, namex)))
    country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))
    country_nbr <- nrow(unique(dfx[,country_col]))
    year_min <- min(dfx$Time)
    year_max <- max(dfx$Time)
    time_cvrg <- year_max-year_min
    return(list(
        namex = namex,
        country_nbr = country_nbr,
        year_min = year_min,
        year_max = year_max,
        time_cvrg=time_cvrg))
}

vague_cvrg(datasets_already_there[1])

vague_cvrg_res <- lapply(setdiff(datasets_already_there, namesx_exceptions), vague_cvrg)

vague_res_df <- as_tibble(rbindlist(vague_cvrg_res))
as.data.frame(vague_res_df)


## focus on AEA
## STANI4_2016
## STANI4_2020


## make all the oecd dfs into named list for nicer access
oecd_dfs <- list()
for (i in datasets_already_there){
    dfx <-  as_tibble(read.csv(paste0(OECD_DATA_DIR, i)))
    oecd_dfs[[i]] <- dfx
}




## ** variable extraction 


cpltns_checker <- function(vx, varx) {
    #' assesses completeness of variable in terms of df_anls PM coverage 
    # there's still a bug with cry_cvrg_geq3, which can be higher than 217 sometimes 

    dfb <- df_anls[,c("countrycode", "year", "nbr_opened")]
    dfc <- as_tibble(merge(dfb, vx, by = c("year", "countrycode"), all.x = TRUE))

    cry_cvrg <- aggregate(year ~ countrycode, na.omit(dfc), length)
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$countrycode
    cry_pm_crvg_actual <- aggregate(nbr_opened ~ countrycode, na.omit(dfc), sum)
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ countrycode, dfc, sum)
    names(cry_pm_crvg_ideal) <- c("countrycode", "nbr_opened_ideal")

    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ## most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
    ##                                     function(x) (filter(cry_pm_cvrg_cprn, diff == x)$countrycode)))

    most_affected_crys <- cry_pm_cvrg_cprn$countrycode[order(cry_pm_cvrg_cprn$diff)[1:4]]
    

    PMs_covered_raw <- sum(na.omit(dfc[which(dfc$countrycode %in% crys_geq3),])$nbr_opened)

    cry_cvrg_geq3 <- sum(filter(dfc, countrycode %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- filter(aggregate(nbr_opened ~ countrycode, dfc, sum), countrycode %in% crys_geq3) %>%
        filter(nbr_opened >= 1) %>%
        nrow()

    return(list(
        varx=varx,
        nobs=nrow(vx),
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
 
}

## generate filter expression with eval(parse())
generate_sel_str <- function(combox){
    strs <- c()

    for (k in 1:ncol(combox)) {

        col_name <- names(combox)[k]
        col_vlu <- as.data.frame(combox)[1,k]
        
        print(paste(col_vlu, typeof(col_vlu)))
        if (is.na(col_vlu)) {
            strx <- paste0('is.na(dfx$', col_name, ")")

        } else if (typeof(col_vlu) == "character"){
            strx <- paste0('dfx["', col_name, '"]=="', col_vlu,'"')
        } else  {
            strx <- paste0('dfx["', col_name, '"]==', col_vlu)
        }
        strs <- c(strs, strx)
    }

    strx_cbn <- paste(strs,collapse =  " & ")
    return(strx_cbn)
}


filter(vague_res_df, country_nbr > 25 & time_cvrg > 25)

lapply(filter(vague_res_df, country_nbr > 25 & time_cvrg > 25)$namex, function(x) names(oecd_dfs[[x]]))


idx <- "AEA"
idx <- "TISP_EBOPS2010"
idx <- "STANI4_2020"


dfx <- oecd_dfs[[idx]]

namesx <- names(dfx)
country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))

dfx$ObsValue <- dfx$ObsValue * (10^dfx$POWERCODE)

combo_cols <- setdiff(namesx, c(country_col, "ObsValue", "X", "Time", "POWERCODE"))

combos <- unique(dfx[,combo_cols])

filter(dfx, is.na(OBS_STATUS) & )


combos <- unique(dfx[,c("MEASURE", "POLLUTANT")])


dfx[which(eval(parse(text=strx_cbn))),]

## which(dfx["IND"] == "D90T92" & is.na(dfx$OBS_STATUS))
    
res <- list()     

for (i in 1:nrow(combos)) {
    print(i)
    combox <- combos[i,]
    print(combox)

    strx_cbn <- generate_sel_str(combox)
    vx <- dfx[which(eval(parse(text=strx_cbn))),]


    ## vx <- dfx[which(dfx["MEASURE"]== as.data.frame(combox)[1,"MEASURE"] &
    ##                 dfx["POLLUTANT"]== as.data.frame(combox)[1,"POLLUTANT"]),]


    ## some flexible renaming, kinda like SQL
    vx2 <- vx %>%
        rename(countrycode=country_col, year=Time, value =ObsValue) %>%
        select(countrycode, year, value)
    
    varx <- paste(combox, collapse = "-")
    
    resx <- cpltns_checker(vx2, varx)
    res[[i]] <- resx
}

res_df <- as_tibble(rbindlist(res))

hist(res_df$cry_cvrg_geq3)

hist(res_df$nobs)

filter(res_df, PMs_covered_raw > 130 & cry_cvrg_geq3 > 200)$varx

i <- which(res_df$varx=="D90T92-NA-NA-P1Y-PER-SELF")
i <- which(res_df$cry_cvrg_geq3 > 250)



## ** debugging failed files, doesn't seem that many -> fine to ignore
proc_sdmx_file("REVPER.xml")
proc_sdmx_file("EO27_VINTAGE.xml")

lapply(list("EO27_VINTAGE.xml", "REVPER.xml"), proc_sdmx_file)



## each item is again a slot-carrier (?) with bunch of slots?

sdmx_file <- "EO27_VINTAGE.xml"
##  [1] "id"                  "agencyID"            "version"            
##  [4] "uri"                 "urn"                 "isExternalReference"
##  [7] "isFinal"             "validFrom"           "validTo"            
## [10] "Name"                "Description"         "Code"               

lapply(list_of_codelists, function(x) slotNames(x))
## need to compare with some item that I know works

sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
## print(slotNames(dsd)) overall slotnames are the same
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))

codelistx <- "CL_EO27_VINTAGE_LOCATION"

list_of_codelists <- slot(slot(dsd, "codelists"),"codelists")
location_codelist <- list_of_codelists[[1]]
slot(location_codelist)
## huh actually has no "Code" slot that could provide the rows
slotNames(location_codelist)

## maybe should see if all the sdmx files that don't work have the same structure
## first see where it works -> write exception





sdmx_file <- "REVPER.xml"
##  [1] "id"                  "agencyID"            "version"            
##  [4] "uri"                 "urn"                 "isExternalReference"
##  [7] "isFinal"             "validFrom"           "validTo"            
## [10] "Name"                "Description"         "Code"               
sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
## print(slotNames(dsd)) overall slotnames are the same
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))


## -> each of the names within the slots seem to be the same
list_of_codelists <- slot(slot(dsd, "codelists"),"codelists")
codelistx <- "CL_REVPER_TIME_FORMAT"
## index 8
## each item again codelist
time_format_codelist <- list_of_codelists[[8]]
## seems like slot "Code" is a list of things that gets converted to rows
slot(time_format_codelist, "Code")

row1 <- slot(time_format_codelist, "Code")[[1]]
## then uses slots id, label, description
## label and description are seem to be named lists -> should be able to loop over them


x <- slot(cls, "codelists")[[1]]
lapply(slotNames(x), function(i) {slot(x, i)})



## proc_sdmx_file(sdmx_file)




## need to compare one codelist that works properly with one that doesn't
## might need manual parsing, have to make sure that it produces the same result in a codelist that works than what is produced by overloaded as.data.frame



## ** concept stuff, not needed
slotNames(sdmx_test)
## [1] "organisationSchemes" "concepts"            "codelists"          
## [4] "datastructures"      "xmlObj"              "schema"             
## [7] "header"              "footer"             

slotNames(slot(sdmx_test, "concepts"))
## yo dawg i heard you like concepts in your concepts
slot(slot(sdmx_test, "concepts"), "concepts")
len(slot(slot(sdmx_test, "concepts"), "concepts"))


country_slot <- slot(slot(sdmx_test, "concepts"), "concepts")[[1]]
slotNames(country_slot)
slot(country_slot, "Name")
slot(country_slot, "Name")$en

year_slot <- slot(slot(sdmx_test, "concepts"), "concepts")[[2]]
slotNames(year_slot)
concept_slot <- slot(sdmx_test, "concepts")
typeof(concept_slot) ## -> S4
len(concept_slot) ## -> 1
slotNames(concept_slot)
[1] "concepts"       "conceptSchemes" "xmlObj"         "schema"        
[5] "header"         "footer"

concept_slots <- slot(concept_slot, "concepts")
lapply(concept_slots, function(x) {slotNames(x)})
unlist(lapply(concept_slots, function(x) {slot(x, "Name")$en}))
##  [1] "Country"            "Variable"           "Industry"          
##  [4] "Time"               "Observation Value"  "Time Format"       
##  [7] "Observation Status" "Unit"               "Unit multiplier"   
## [10] "Reference period"  
