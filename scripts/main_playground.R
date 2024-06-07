## cbn_df_dict$rates$cbn1 %>% adt %>% .[iso3c == "DEU"]

    
## ** check density numbers after v69


cbn_dfs_rates_uscld$cbn1 %>% adt() %>% .[iso3c %in% c("NLD", "DEU", "BEL", "ITA", "USA")] %>%
    ggplot(aes(x=year, y = smorc_dollar_fxm_lag0, color = iso3c)) + geom_line()

## ggsave(filename = "~/Dropbox/phd/teaching/master_thesis/smorc_crys.pdf")

cbn_dfs_rates$cbn_all %>%
    mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>% 
    viz_lines(y="pm_density_lag0", duration = 1, facets = "region")

    

## ** inspecting rates calculation 

df_reg_rts %>% adt() %>% .[iso3c %in% c("DEU", "KOR") & year == 2010,
                           .(iso3c, pop = SP.POP.TOTLm, pm_density, pm_density_sqrd)]

## ** comparing consistency of computation of squared variables and interactions
cbn_dfs_rates$cbn_all %>% adt() %>%
    ## .[, .(iso3c, year, ti_tmitr_interact_lag0, pm_density_sqrd_lag0, smorc_dollar_fxm_sqrd_lag0)] %>%
    melt(id.vars = c("iso3c", "year")) %>%
    .[, .(sd = sd(value), mean = mean(value), min = min(value), max = max(value)), variable] %>% print(n=220)

ggplot(df_anls, aes(x=year, y= pm_density, group = iso3c)) +
    geom_line() +
    geom_text(adt(df_anls)[, .SD[which.max(year)], iso3c], mapping = aes(x=year, y=pm_density, label = iso3c))
    





## ** more outlier search 



dt_rng_visl[value > 6] %>% print(n=80)
    
dt_rng_visl[, .N, iso3c] %>% print(n=200)
    

    
## ** check growth rates

hist(df_reg$NY.GDP.PCAP.KD.ZG, breaks = 30)



adt(df_reg)[NY.GDP.PCAP.KD.ZG > 50] %>%
    adt(df_reg)[., on = "iso3c"] %>%
    melt(id.vars = c("iso3c", "year"), measure.vars = c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG")) %>% 
    ggplot(aes(x=year, y=value)) + geom_line() +
    facet_grid(variable~iso3c, scales = "free")

adt(df_reg)[NY.GDP.PCAP.KD.ZG > 50] %>%
    ggplot(aes(x=year, y=NY.GDP.PCAP.KD.ZG)) +
    geom_line() + geom_point() + 
    facet_wrap(~iso3c)

             




get_all_descriptives()


## ** testing

test_me <- function(a,b) {

    a+b
}

TEST_DIR <- paste0(PROJECT_DIR, "scripts/tests/")

testing_res <- testthat::test_file(path = paste0(TEST_DIR, "test_testing.R")) # reporter = "list")
testing_res <- testthat::test_file(path = paste0(TEST_DIR, "test_artnews.R"), reporter = c("check"))
testing_res <- testthat::test_file(path = paste0(TEST_DIR, "test_artnews.R"), reporter = c("progress"))
testing_res[[1]]$results






## ** another shitty analysis

## select(df_reg, -matches("name")) %>%
##     mutate(iso3c_num = as.numeric(as.factor(iso3c))) %>% adt() %>%
##     fwrite(paste0(PROJECT_DIR, "data/processed/df_reg2.csv"))

df_reg1 <- df_reg[,c("iso3c", "nbr_opened", "SP.POP.TOTLm", "NY.GDP.PCAP.CDk", "pct_fx", "ghweal992j")] %>% na.omit()

x_within <- pglm(nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + pct_fx + ghweal992j, data = df_reg1, family = negbin, model = "within", index = "iso3c", method = "bfgs", effect = "individual")

x_random <- pglm(nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + pct_fx + ghweal992j, data = df_reg1, family = negbin, model = "random", index = "iso3c", method = "bfgs", effect = "individual")
x_between <- pglm(nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + pct_fx + ghweal992j, data = df_reg1, family = negbin, model = "between", index = "iso3c", method = "bfgs", effect = "individual")
x_pooling <- pglm(nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + pct_fx + ghweal992j, data = df_reg1, family = negbin, model = "pooling", index = "iso3c", method = "bfgs", effect = "individual")
x_random <- pglm(nbr_opened ~ pct_fx + ghweal992j, data = df_reg1, family = negbin, model = "random", index = "iso3c")

screenreg(x_pooling)
screenreg(x_within)
screenreg(x_random)
screenreg(x_between)

f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j + pct_cutoff_10M + cnt_contemp + tmitr_approx_linear_2020step + pct_fx + clctr_cnt_cpaer
res_start <- pglm(f, index = "iso3c", family = poisson, model = "within", effect = "individual",
     data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]))

f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j + pct_cutoff_10M + cnt_contemp + tmitr_approx_linear_2020step + pct_fx + clctr_cnt_cpaer
res_start2 <- pglm(f, index = "iso3c", family = poisson, model = "random", effect = "individual",
     data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]))

phtest(f, data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]))
## hausman test not available for pglm

f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j + pct_cutoff_10M + tmitr_approx_linear_2020step + pct_fx + clctr_cnt_cpaer + cnt_contemp_2000 +  sum_core 
res_crsc <- pglm(f, index = "iso3c", family = poisson, model = "random", effect = "individual",
                  data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]))

## drop the cultural spending
f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j + pct_cutoff_10M + tmitr_approx_linear_2020step + clctr_cnt_cpaer + cnt_contemp_2000 +  sum_core 
res_crsc2 <- pglm(f, index = "iso3c", family = poisson, model = "random", effect = "individual",
                  data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]))





screenreg(list(res_start, res_start2, res_crsc, res_crsc2))

## follow https://stackoverflow.com/questions/49033016/plm-or-lme4-for-random-and-fixed-effects-model-on-panel-data
## replicate plm in glm 

## first: fixed effects

df_reg$hwni_30M <- (df_reg$SP.POP.TOTL * df_reg$pct_cutoff_30M)/100

df_reg %>%
    mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>%
    select(iso3c, year, hwni_30M, region) %>%
    na.omit() %>% 
    viz_lines(y="hwni_30M", facets = "region")

f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j  + cnt_contemp + tmitr_approx_linear_2020step + pct_fx + clctr_cnt_cpaer + iso3c
res_poiglm_fe <- glm(f, data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", labels(terms(f)))]), family=poisson)

varnames <- as.list(labels(terms(f)))
names(varnames) <- varnames
screenreg(list(res_poiglm_fe, res_start), custom.coef.map = varnames)

## huh amazing, coefs and SEs are really the same
## AIC and LL aren't tho
## well this is LSVD or maybe MLVD

## random effects

f <- nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + ghweal992j + gptinc992j + hwni_30M + tmitr_approx_linear_2020step + clctr_cnt_cpaer + cnt_contemp_2000 +  sum_core + I(year-1995) + (1|iso3c) 

res_poiglm_re <- glmer(f,
                       ## data = na.omit(df_reg[,c("iso3c", "year", "nbr_opened", head(labels(terms(f)),-1))]),
                       data = df_reg,
                       family = poisson)

varnames <- as.list(head(labels(terms(f)),-1))
names(varnames) <- varnames
screenreg(res_poiglm_re)

screenreg(list(res_poiglm_re, res_crsc2), custom.coef.map = varnames, digits = 3)
## very similar, but some differences: ginis, cnt_contemp_2000, sum_core


## ** remaining variables construction, not directly related to structure, have to be functionalized somewhere




na.omit(df_reg[names(df_reg) %!in% c("PC1_all", "PC2_all")])





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


## * check density calculations after finding typo (actually isn't typo but test)

## ** before debugging
df_anls %>% viz_lines(y="pmdens_neigh")
df_anls %>% adt %>% .[, .(mean_pmdens_neigh = mean(pmdens_neigh)), iso3c] %>% .[order(-mean_pmdens_neigh)]

##  1 LIE               0.433
##  2 SWE               0.290
##  3 DNK               0.278
##  4 NLD               0.278
##  5 FRA               0.237
##  6 CZE               0.231
##  7 AUT               0.217
##  8 AUS               0.202
##  9 PRT               0.199
## 10 LUX               0.195


## ** after debugging: actually I think the iso3cxx is intentional, seems to be some check?


## ** diversityi stuff
## two weird drops
## look at orig, see if they are there already
dt_hief_prep5 %>% 
    .[year >= 1990] %>% 
    ggplot(aes(x=year, y=orig, group = iso3c)) + geom_line() + geom_point() + 
    facet_wrap(~src)#

                                        # is gone when I change the order in lfnb  (from nocb->locf to locf->nocb)
## but where do they come from?  maybe order?
dt_hief_prep5[year == 2020 & src == "nocb" & !is.na(orig)]
## DEUTSCHLAND
dt_hief_prep2[iso3c == "DEU"] %>% print(n=80) ## yup seems to be order issue



## look at example country: AFG
## i think the issue is that post 2013 it gets imputed, but there should be locf -> need impute all hief stuff

## hunting bugs: why lfnb has lower coverage than nocb/locf?
    
dt_cvrg_long[, .N, vrbl]
dt_cvrg_long[cvrd == 1, .N, vrbl]
## already lower lfnb coverage there after dt_cvrg_long
dt_cvrg_wide[hief_lfnb == 0 & hief_nocb == 1 & year > 2000, .N, year] %>% print(n=300)

dt_div[is.na(hief_lfnb) & !is.na(hief_nocb)]

## maybe wierd shifting? don't see how
dt_div_vrbls_wide[, .SD, .SDcols = patterns("iso3c|^hief_lfnb_lag|^hief_locf_lag")] %>% summary
## shifting leads to more NAs in lfnb
## maybe assignment of order with l_vrbls_div_lag construction
## seems to be the case
