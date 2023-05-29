## ** new version 

f <- nbr_opened ~ sum_core + cnt_contemp_1995 +  gptinc992j_lag4 + shweal992j_p90p100_lag4 + tmitr_approx_linear_2020step_lag2 + ti_tmitr_interact_lag2 + smorc_dollar_fxm_lag4 + NY.GDP.PCAP.CDk_lag1 + SP.POP.TOTLm_lag2 + clctr_cnt_cpaer_lag2 + (1|iso3c)

## hnwi_nbr_1M_lag4 +

dfx <- merge(cbn_dfs[["cbn_all"]], select(df_reg, iso3c, year, nbr_opened)) %>% atb() %>% select(all.vars(f))

## dfx2 <-
##     apply(dfx[2, 2, \(i) i)


res <- glmer.nb(f, dfx)



scale_vars <- c("hnwi_nbr_1M", "hnwi_nbr_5M", "hnwi_nbr_30M", "hnwi_nbr_200M", "hnwi_nbr_1B")
scale_vars <- all_rel_vars

c(
    apply(df_reg[scale_vars], 2, scale) %>% apply(2, sd, na.rm = T),
    apply(df_reg[setdiff(all_rel_vars, scale_vars)], 2, \(x) round(sd(x, na.rm = T),3))
    ) %>% as.list() %>% adf() %>%
pivot_longer(cols = names(.)) %>% na.omit() %>% 
ggplot(aes(x=name, y=value)) + geom_bar(stat = "identity") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

## try with rescaling all 



df_scl <- df_reg %>%
    select(all_of(c(base_vars, "nbr_opened", all_rel_vars)))%>%
    na.omit() %>% ## first select all relevant vars and omit to rescale relative to combination
    mutate(across(all_of(all_rel_vars), scale_wo_attr), ## scale the relevant ones
           iso3c_num = as.numeric(factor(iso3c)))

## apply(df_scl[all_rel_vars], 2, sd)


f <- paste0(c(reg_spec$vrbl, crscn_vars, "(1 | iso3c)"), collapse = " + ") %>% paste0("nbr_opened ~ ",.)  %>% as.formula()


## ** stata comparison 

stata_test_vars <- c("nbr_opened", "hnwi_nbr_30M", "gptinc992j", "ghweal992j", "tmitr_approx_linear20step", "ti_tmitr_interact", "smorc_dollar_fxm", "NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "clctr_cnt_cpaer", "sum_core", "cnt_contemp_1995")

df_scl %>% select(c(all_of(c(base_vars, stata_test_vars)))) %>%
    mutate(iso3c_num = as.numeric(factor(iso3c))) %>% 
    write.table(paste0(PROJECT_DIR, "data/processed/df_scl.csv"), sep = ",", row.names = F)

## *** R

f_stata_cprn <- paste0(stata_test_vars[2:len(stata_test_vars)], collapse = " + ")
f_stata_cprn <- paste0("nbr_opened ~ ", f_stata_cprn)
f_stata_cprn <- paste0(f_stata_cprn, " + ", "(1 | iso3c)")

res <- glmer.nb(f_stata_cprn, df_scl)
screenreg(res)


res_glm <- glm.nb(f_stata_cprn, df_scl)


library(pglm)

f_pglm <- paste0(stata_test_vars[2:len(stata_test_vars)], collapse = " + ")

f_pglm <- paste0("nbr_opened ~ ", f_pglm)

res2 <- pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
             model = "random", family = poisson, effect = "individual")

res3 <- pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
             model = "random", family = negbin, effect = "individual", method = "bfgs")

res4 <- pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
             model = "random", family = negbin, effect = "individual", method = "nr")

## res5 <- pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
##              model = "random", family = negbin, effect = "individual", method = "Sann")




screenreg(list(res2, res3, res4))

optim_methods <- c("nr", "bfgs", "bfgsr", "bhhh", "sann", "cg", "nm") ## all methods 
optim_methods <- c("bfgs", "bfgsr", "cg", "nm") ## methods actually working for negbin
optim_methods <- c("nr", "bhhh", "nm")


res_optim_methods <- mclapply(optim_methods, \(x) pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
                                                       model = "random", family = negbin, method = x), mc.cores = 7)




## screenreg(res_optim_methods[c(1:4, 6:7)], custom.model.names = optim_methods[c(1:4, 6:7)])
screenreg(res_optim_methods, custom.model.names = optim_methods)

reg_pltx <- plotreg(res_optim_methods, type = "forest", custom.model.names = optim_methods)

y <- edit_plotreg(reg_pltx)


vif_res1 <- plm(f_pglm, data = df_scl, index = c("iso3c", "year"), model = "pooling", family = negbin, method = "nm")
vif_res2 <- lm(f_pglm, data = df_scl)

vif(vif_res)
vif(vif_res2)




## **** testing optimization methods with poisson
## all optim methods seem to work, and to produce pretty similar results

poisson_optim <- mclapply(optim_methods, \(x) pglm(f_pglm, data = df_scl, index = c("iso3c", "year"),
                                                   model = "random", family = poisson, method = x), mc.cores = 7)

screenreg(poisson_optim, custom.model.names = optim_methods)

plotreg(poisson_optim)
edit_plotreg(plotreg(poisson_optim, type = "forest", custom.model.names = optim_methods))

## **** testing correlations



chart.Correlation(df_scl[stata_test_vars])
## some high ones:
## - hnwi and smorc_dollar_fxm/clctr_cnt_cpaer/cnt_contemp_1995
## - smorc_dollar_fxm and clctr_cnt_cpaer/cnt_contemp_1995 
## - clctr_cnt_cpaer and cnt_contemp 1995

## **** RStata


iv_vars <- stata_test_vars[2:8]
stata_output_vars <- c(iv_vars, c("cons", "ln_r", "ln_s"))


gof_names <- c("N", "log_likelihood", "N_g", "Chi2", "p", "df")

stata_res_raw <- get_stata_result(iv_vars, stata_output_vars, gof_names)

stata_res_parsed <- parse_stata_res(stata_res_raw, stata_output_vars, gof_names)





   
## x <- createTexreg(coef.names = stata_output_vars,
##              coef = filter(stata_res_parsed, is_coef)$value,
##              se = filter(stata_res_parsed, is_se)$value,
##              pvalues = p_values,
##              gof.names = gof_names,
##              gof = filter(stata_res_parsed, is_gof)$value)
             
## screenreg(x)
             





stata("ds", data.in = df_scl)

## gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995

## ** poisson fe 

stata_test_lngtd_vars <- setdiff(stata_test_vars, crscn_vars)

f_pglm_fe <- paste0(stata_test_lngtd_vars[2:len(stata_test_lngtd_vars)], collapse = " + ")
f_pglm_fe <- paste0("nbr_opened ~ ", f_pglm_fe)

library(MASS)

nb_fe <- pglm(f_pglm_fe, data = df_scl, index = c("iso3c", "year"),
             model = "within", family = negbin, effect = "individual")



nb_fe_ind <- pglm(f_pglm_fe, data = df_scl, index = c("iso3c", "year"),
             model = "within", family = negbin, effect = "individual")

nb_fe_year <- pglm(f_pglm_fe, data = df_scl, index = c("year"),
             model = "within", family = negbin, effect = "individual")

nb_fe_two1 <- pglm(f_pglm_fe, data = df_scl, index = c("iso3c", "year"),
             model = "within", family = negbin, effect = "twoways")

## probably need "pooling" for "twoways"
nb_fe_two2 <- pglm(f_pglm, data = df_scl, index = c("year", "iso3c"),
                   model = "pooling", family = negbin, effect = "twoways")

nb_fe_time1 <- pglm(f_pglm_fe, data = df_scl, index = c("year"),
             model = "within", family = negbin, effect = "time")

nb_fe_time2 <- pglm(f_pglm_fe, data = df_scl, index = c("iso3c"),
             model = "within", family = negbin, effect = "time")



screenreg(list(nb_fe_ind, nb_fe_year, nb_fe_two1, nb_fe_two2, nb_fe_time1, nb_fe_time2))
## wondering if time-invariant variables can be used with twoways
## probably not: probably FE are additive: unit FE + time FE, not multiplicative: unit fe * time fe
## strange that it works tho 


## **** SO posting
data("PatentsRDUS", package="pglm")


optim_methods <- c("nr", "bfgs", "bfgsr", "bhhh", "sann", "cg", "nm")

## la_res <- lapply(optim_methods, \(x) pglm(patents ~ lag(log(rd), 0:5) + scisect + log(capital72) + factor(year),
##                                           PatentsRDUS, family = negbin, model = "within", print.level = 3,
##                                           method = x, index = c('cusip', 'year')))

la_res <- mclapply(optim_methods, \(x) pglm(patents ~ log(rd) + scisect + log(capital72), 
                                          PatentsRDUS, family = negbin, model = "random", 
                                          method = x, index = c('cusip')), mc.cores = 7)

sel <- c(3,4,5,6,7)
screenreg(la_res[sel], custom.model.names = optim_methods[sel])


pglm(patents ~ log(rd) + scisect + log(capital72) + factor(year), PatentsRDUS, family = negbin, model = "random", index = c("cusip"), method = "nr")


data(Hedonic, package = "plm")
ra_res <- lapply(optim_methods, \(x) pglm(mv ~ crim + zn + indus + nox + age + rm, Hedonic, family = gaussian,
                                          model = "random", print.level = 3, method = x, index = "townid"))


screenreg(ra_res, custom.model.names = optim_methods)

write.table(PatentsRDUS, paste0(PROJECT_DIR, "data/processed/PatentsRDUS.csv"), row.names = F, sep=",")





## ** something lol

t1 = Sys.time()
res_scl <- glmer.nb(f, df_scl)
t2 = Sys.time()
t2-t1
screenreg(res_scl)


## 
names(cbn_dfs[["cbn_all"]])






%>% plot()




glmer.nb(nbr_opened ~ sum_core + cnt_contemp_1995 +  gptinc992j_lag4 + shweal992j_p90p100_lag4 + tmitr_approx_linear_2020step_lag2 + ti_tmitr_interact_lag2 +  NY.GDP.PCAP.CDk_lag1 + SP.POP.TOTLm_lag2 + clctr_cnt_cpaer_lag2 + (1|iso3c), dfx)


## ** new test

df_reg1 <- na.omit(df_reg[,c("iso3c", "year", "nbr_opened", "PC1_caf", "cnt_contemp_1985", "cnt_all_1985", "pct_cutoff_10M", "gptinc992j", "ghweal992j", "SP.POP.TOTLm", "NY.GDP.PCAP.CDk")])

df_reg1 <- filter(df_reg1, pct_cutoff_10M < 1)



chart.Correlation(df_reg1[,c("nbr_opened", "PC1_caf", "cnt_contemp_1985", "pct_cutoff_10M", "gptinc992j")])

ols_lm <-      lm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, df_reg1)
ols_glm <-   lmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1)
poi_glm <-    glm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, df_reg1, family = poisson)
poi_glmm <- glmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1, family = poisson)



screenreg(list(ols_lm, ols_glm, poi_glm, poi_glmm, nb, nb_all))


nb_interest <-    glmer.nb(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + (1|iso3c), df_reg1)

nb_all <- glmer.nb(nbr_opened ~ PC1_caf + cnt_contemp_1985 + cnt_all_1985 +
                         pct_cutoff_10M + gptinc992j + ghweal992j +
                         SP.POP.TOTLm + NY.GDP.PCAP.CDk + (1|iso3c), df_reg1)


nb_crols <- glmer.nb(nbr_opened ~ gptinc992j + ghweal992j + cnt_all_1985 + 
                         SP.POP.TOTLm + NY.GDP.PCAP.CDk + (1|iso3c), df_reg1)

nb_wb <- glmer.nb(nbr_opened ~ SP.POP.TOTLm + NY.GDP.PCAP.CDk + (1|iso3c), df_reg1)

screenreg(list(nb_wb, nb_crols, nb_interest, nb_all))
texreg(list(nb_wb, nb_crols, nb_interest, nb_all), file = paste0(TABLE_DIR, "nb.tex"))



         ,
          custom.coef.map = list("(Intercept)" = "(Intercept)", "PC1_caf" = "PC1_caf",
                                 "cnt_contemp_1985" = "cnt_contemp_1985",
                                 "pct_cutoff_10M" = "pct_cutoff_10M", "gptinc992j" = "gptinc992j"))

margins(poi_glmm)


## ** specification oddities

poi_glmmp1 <- glmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1, family = poisson)
## same as poi_glmm

poi_glmmbar <- glmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1 || iso3c), df_reg1, family = poisson)
## doesn't run 

screenreg(list(poi_glmm, poi_glmmp1,nb),
          custom.coef.map = list("(Intercept)" = "(Intercept)", "PC1_caf" = "PC1_caf",
                                 "cnt_contemp_1985" = "cnt_contemp_1985",
                                 "pct_cutoff_10M" = "pct_cutoff_10M", "gptinc992j" = "gptinc992j"))



coef_df <- coef(poi_glmm)$iso3c
coef_df$iso3c <- rownames(coef_df)

intercept_df <- as_tibble(merge(
    coef_df[,c("(Intercept)", "iso3c")],
    df_reg1))
    
intercept_df_fltrd <- unique(intercept_df[,c("iso3c", "PC1_caf", "cnt_contemp_1985", "(Intercept)")])
chart.Correlation(intercept_df_fltrd[,names(intercept_df_fltrd) != "iso3c"])

    
coef(poi_glmm)$iso3c %>%
                 mutate(iso3c = rownames(.)) %>%
                 select("iso3c", "(Intercept)") %>%
                 merge(., unique(df_reg[,c("iso3c", "PC1_caf", "cnt_contemp_1985")])) %>%
                 select("(Intercept)", "PC1_caf", "cnt_contemp_1985") %>%
                 as_tibble() %>%
                 chart.Correlation()
                 
                 chart.Correlation(.[,c("(Intercept)", "PC1_caf", "cnt_contemp_1985")])
                 



## muh staaaaaaaarssssssssss
## ** interpretation
## weird tho: now have time varying and cross-sectional variables here

coef(poi_glm)
coef(poi_glmm) ## intercept differs, all others are same

## ** pglm

library(pglm)

pglm_within <- pglm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, data = df_reg1,
                    family = poisson, model = "within", index = "iso3c")
## pglm_within seems how I think of FE: non-time-varying variables have ridiculously large SEs
pglm_pooling <- pglm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, data = df_reg1,
                 family = poisson, model = "pooling", index = "iso3c")
pglm_random <- pglm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, data = df_reg1,
                    family = poisson, model = "random", index = "iso3c")
pglm_between <- pglm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, data = df_reg1,
                 family = poisson, model = "between", index = "iso3c")


pglm_all <- pglm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + cnt_all_1985 +
                         pct_cutoff_10M + gptinc992j + ghweal992j +
                         SP.POP.TOTLm + NY.GDP.PCAP.CDk, family = poisson,
                 data= df_reg1, model = "random", index = "iso3c")

pglm_crol <- pglm(nbr_opened ~  SP.POP.TOTLm + NY.GDP.PCAP.CDk, family = poisson,
                 data= df_reg1, model = "random", index = "iso3c")


screenreg(list(pglm_within, pglm_pooling, pglm_random, pglm_between, pglm_crol, pglm_all))

## pglm_pooling and poi_glm are the same
## -> pooling just seems to be "i don't care" then, is basically treating observations as independent 
screenreg(list(pglm_pooling, poi_glm))

## *** inspecting pglm
coef(pglm_within)
coef(pglm_pooling)
coef(pglm_random)
coef(pglm_between)
## none have values for the individual stuff, e.g. country-level intercepts



## ** compare to factor/LSDV models
## this is a complete mess, not at all like poi_glmm/ols_glmm
ols_fctr <-    lm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + factor(iso3c), df_reg1)
poi_fctr <-   glm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + factor(iso3c), df_reg1, family = "poisson")

screenreg(list(ols_glm, ols_fctr, poi_glmm, poi_fctr),
          custom.coef.map = list("PC1_caf" = "PC1_caf", "cnt_contemp_1985" = "cnt_contemp_1985",
                                 "pct_cutoff_10M" = "pct_cutoff_10M", "gptinc992j" = "gptinc992j"))




## ** check that longitudinal vars are longitudinal and cross-sectional are cross-sectional, is the case

df_reg1 %>%
    group_by(iso3c) %>%
    summarise(nbr_unq = length(unique(pct_cutoff_10M))) %>%
    summary()




## ** old test 

mean(filter(df_reg, nbr_opened_prop < 100)$nbr_opened_prop)

var(filter(df_reg, nbr_opened_prop < 1)$nbr_opened_prop)
var(filter(df_reg, nbr_opened_prop < 10)$nbr_opened_prop)
var(filter(df_reg, nbr_opened_prop < 100)$nbr_opened_prop)

## variance is actually smaller than mean huh
## guess because I have so many 0s?

## if I include the Monaco 2014, variance gets super high lol



reg1 <- lmer(nbr_opened_prop ~ (1 | iso3c), df_reg)
reg2 <- lmer(nbr_opened_prop ~ pct_cutoff_10M + (1 | iso3c), df_reg)
reg3 <- lm(nbr_opened_prop ~ pct_cutoff_10M, df_reg)
reg4 <- glmer.nb(nbr_opened_prop ~ pct_cutoff_10M + (1 | iso3c), df_reg)
reg5 <- glmer(nbr_opened_prop ~ pct_cutoff_10M + (1 | iso3c), df_reg, family = "poisson")


screenreg(reg1)
screenreg(reg2)
screenreg(reg3)
screenreg(reg4)
screenreg(reg5)

screenreg(list(reg3, reg4, reg5))

plot(reg4q)
nrow(filter(df_reg, nbr_opened_prop > 0))

## ** tutorial Brown_2021_mixed

TUT_DIR <- "/home/johannes/Dropbox/supplements/Brown_2021_mixed/"

library(lme4)
library(tidyverse)
library(ggplot2)
library(texreg)
library(afex)

tut_df <- as_tibble(read.csv(paste0(TUT_DIR, "rt_dummy_data.csv")))

res_intercept <- lmer(RT ~ modality + (1 | PID) + (1 | stim), tut_df)

res_slopes <- lmer(RT ~ modality + ( 1 + modality | PID) + (1+modality|stim), tut_df,
                   control = lmerControl(optCtrl = list(maxfun = 1e10)))

res_slopes_bobyqa <- lmer(RT ~ modality + ( 1 + modality | PID) + (1+modality|stim), tut_df, control = lmerControl(optimizer = "bobyqa"))



res_slopes_reduced <- res_slopes <- lmer(RT ~  1+ ( 1 + modality | PID) + (1+modality|stim), tut_df)

screenreg(list(res_slopes_zerocor, res_slopes_zerocor2, res_slopes_zerocor3))
screenreg(list(res_slopes_zerocor, res_slopes))

screenreg(list(res_intercept, res_slopes, res_slopes_bobyqa))

all_fit(res_slopes_bobyqa)

screenreg(list(res_slopes_bobyqa, res_slopes_reduced))

anova(res_slopes, res_slopes_reduced)

mixed(RT ~ modality + ( 1 + modality | PID) + (1+modality|stim), tut_df,
      control = lmerControl(optimizer = "bobyqa"), method = "LRT")


tut_df_interact <- as_tibble(read.csv(paste0(TUT_DIR, "rt_dummy_data_interaction.csv")))

res_interact <- lmer(RT ~ 1 + modality + SNR + modality:SNR + (1 + modality| PID) + (1 + modality | stim), tut_df_interact)
res_interact2 <- lmer(RT ~ 1 + modality + SNR + modality:SNR + (1 + modality + SNR | PID) + (1 + modality + SNR | stim), tut_df_interact)

screenreg(list(res_interact, res_interact2))


## *** zeroslopes
res_slopes_zerocor <- lmer(RT ~ 1 + modality + ( 0 + modality | PID) + (1|PID) + (1+modality|stim), tut_df, control = lmerControl(optimizer = "bobyqa"))

res_slopes_zerocor2 <- lmer(RT ~ modality + ( 0 + modality | PID) + (1+modality|stim), tut_df)
res_slopes_zerocor3 <- lmer(RT ~ modality + ( 0 + modality | PID) + (0+modality|stim), tut_df)

res_slopes_zerocor4 <- lmer(RT ~ modality + ( 1 + modality || PID) + (1+modality||stim), tut_df, control = lmerControl(optCtrl = list(maxfun = 1e10)))
res_slopes_zerocor5 <- lmer(RT ~ modality + ( 1 + modality | PID) + (1+modality||stim), tut_df, control = lmerControl(optCtrl = list(maxfun = 1e10)))
res_slopes_zerocor6 <- lmer(RT ~ 1 + modality + (1+ modality || PID) + (1+modality |stim), tut_df, control = lmerControl(optCtrl = list(maxfun = 1e12), optimizer = "bobyqa"))

zerocor_allfit <- all_fit(res_slopes_zerocor6)


coef(res_slopes_zerocor)$PID %>%  correlate()

coef(res_slopes_zerocor6)$PID %>%  correlate()

lapply(zerocor_allfit, function(x) coef(x)$PID %>% correlate())
## only bobyqa has proper 0 correlations between intercepts and modalities

coef(zerocor_allfit$bobyqa.)$PID %>% correlate()

## bobyqa model of all_fit and res_slope_zerocor6 (also bobyqa) are different



coef(res_slopes_zerocor)$PID
cor(coef(res_slopes_zerocor)$PID$"(Intercept)", coef(res_slopes_zerocor)$PID$"modalityAudio-only") ## 0
cor(coef(res_slopes_zerocor)$PID$"(Intercept)", coef(res_slopes_zerocor)$PID$"modalityAudiovisual") ## 0 
cor(coef(res_slopes_zerocor)$PID$"modalityAudio-only", coef(res_slopes_zerocor)$PID$"modalityAudiovisual") ## -1
## idk why there are now slopes for both modalities (audio only and audio visual)
## correlation is -1 between them, and between each of them and intercept 0

## double bar notation

cor(coef(res_slopes_zerocor6)$PID$"(Intercept)", coef(res_slopes_zerocor6)$PID$"modalityAudio-only") # 0.75
cor(coef(res_slopes_zerocor6)$PID$"(Intercept)", coef(res_slopes_zerocor6)$PID$"modalityAudiovisual") # 0.8
cor(coef(res_slopes_zerocor6)$PID$"modalityAudio-only", coef(res_slopes_zerocor6)$PID$"modalityAudiovisual") # 0.2

library(corrr)
coef(zerocor_allfit$optimx.nlminb)$PID %>%
                                  correlate()



cor(coef(res_slopes)$PID$"(Intercept)", coef(res_slopes)$PID$"modalityAudiovisual")

coef(res_slopes_zerocor2)$PID ## huh really no random intercepts


## * fatalities
df_fatal <- as_tibble(read.csv("/home/johannes/Dropbox/supplements/Stock_2020_econometrics/Fatalities2.csv"))


filter(df_fatal, year %in% c(1982, 1988)) %>%
    mutate(fatal_rate = fatal/(pop/10000)) %>%
    select(state, year, fatal_rate, beertax) %>%
    ggplot(aes(x=beertax, y=fatal_rate)) +
    geom_point() +
    geom_smooth(method = lm) + 
    facet_wrap(~year, ncol=1, scales = "free")


filter(df_fatal, year %in% c(1982, 1988)) %>%
    mutate(fatal_rate = fatal/(pop/10000)) %>%
    select(state, year, fatal_rate, beertax) %>%
    pivot_wider(id_cols = state, names_from = year, values_from = c(fatal_rate, beertax)) %>%
    mutate(fatal_rate_diff = fatal_rate_1988 - fatal_rate_1982,
           beertax_diff = beertax_1988 - beertax_1982) %>%
    ggplot(aes(x=beertax_diff, y=fatal_rate_diff)) +
    geom_point() +
    geom_smooth(method = lm, show.legend = T) +
      stat_poly_eq(formula= y~x, 
      aes(label=paste(..eq.label.., ..rr.label.., sep="~~~")),
      parse=T)

df_fatal$fatal_rate = df_fatal$fatal/(df_fatal$pop/10000)


fe1 <- lmer(fatal_rate ~ beertax + (1 | state), df_fatal)
fe2 <- lm(fatal_rate ~ beertax, df_fatal,  index = "state"))
fe3.1 <- plm(fatal_rate ~ beertax, df_fatal)
fe3.2 <- plm(fatal_rate ~ beertax, df_fatal, effect = "time", index = "state")
fe3.3 <- plm(fatal_rate ~ beertax, df_fatal, effect = "twoways", index = "state")
fe3.4 <- plm(fatal_rate ~ beertax, df_fatal, model = "random", index = "state")
fe3.5 <- plm(fatal_rate ~ beertax, df_fatal, model = "between", index = "state")
screenreg(list(fe1, fe2, fe3.1, fe3.2, fe3.3, fe3.4, fe3.5))

r.squared(fe3.1, type = "cor", )

df_fatal %>% select(state, year, drinkage)

df_fatal$iso3c <- "USA"

viz_lines(df_fatal, y="drinkage", grp = "state", facets = "iso3c")
viz_lines(df_fatal, y="income", grp = "state", facets = "iso3c")
viz_lines(df_fatal, y="fatal_rate", grp = "state", facets = "iso3c")
## can't see a reason why you wouldn't use continuous measure for drinkage
## interpretation: if a state changes to drinking age 20, fatalities go up/down
## this seems completely garbage: can not really tell if stuff goes up or down


fe4.1 <- plm(fatal_rate ~ beertax, df_fatal, index = c("year", "state"))
fe4.2 <- plm(fatal_rate ~ beertax, df_fatal, index = c("state", "year"), effect = "twoways")
screenreg(list(fe3.1, fe4.1, fe4.2))
## fuck the second index gets ignored -> needs effect = "twoways"


## all variables 
fe_all_year <- plm(fatal_rate ~ beertax + drinkage + jail + I(miles/1000) + unemp + log(income) + year , df_fatal, index = "state")

fe_all_2index <- plm(fatal_rate ~ beertax + drinkage + jail + miles + unemp + log(income), df_fatal, index = c("state", "year"), effect = "twoways")
# this has pretty much all 

screenreg(list(fe_all_year, fe_all_2index))
fixef(fe_all_2index, effect = "twoways")

coeftest(fe_all_year, vcov = vcovHC(fe_all_year, cluster = "group"))
coeftest(fe_all_2index, vcov = vcovHC(fe_all_2index, cluster = "group"))

## quite different results
## maybe should compare to book results visually
## facet_wrap(~variable)
## with effect="twoways" the coefs are all like in book (except miles)
## standard errors are also all pretty close (within ~95% guestimate)

## * paralellization test


## parLapply: gets stuck and can't even quit 
library(doParallel)
## no_cores <- 5
## registerDoParallel(cores=no_cores)  
## cl <- makeCluster(no_cores, type="FORK")
## parLapply(cl, all_specs_flat[11:20], \(x) run_spec(x, base_vars))

## doParallel: also gets stuck and can't be quitted
no_cores <- 5
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  
result <- foreach(i=all_specs_flat[11:20]) %dopar% run_spec(i, base_vars)





run_vrbl_mdl_vars(mdl_vars = c("tmitr_approx_linear2020step_lag4", "NY.GDP.PCAP.CDk_lag3", "SP.POP.TOTLm_lag1"), 
                  df_cbn = cbn_dfs$cbn_all,
                  cbn_name = "cbn_all",
                  mdl_name = "tmitr_approx_linear_2020step_lag4",
                  reg_spec = reg_spec)


## timeout


timeout_test <- function(timeout_sec) {
    #' how long function runs 
    Sys.sleep(timeout_sec)
    return(T)
}

converged <- F


converged <- withTimeout(timeout_test(1), timeout = 2)
converged <- withTimeout(timeout_test(3), timeout = 2, onTimeout = "silent")

if (is.null(converged)) {print("not converged")} else {print("converged")}
                  
## ** debugging lag: no difference

## pick some model where only lag varies

reg_lag_test1 <- reg_spec_mdls[[3]]
reg_lag_test2 <- reg_spec_mdls[[34]]
reg_lag_test3 <- reg_spec_mdls[[127]]

run_vrbl_mdl_vars(reg_lag_test1, verbose = T)
run_vrbl_mdl_vars(reg_lag_test2, verbose = T)
run_vrbl_mdl_vars(reg_lag_test3, verbose = T)

select(cbn_dfs$cbn_all, iso3c, year, hnwi_nbr_1B_lag1, hnwi_nbr_1B_lag2, hnwi_nbr_1B_lag3, hnwi_nbr_1B_lag3) %>%
    filter(iso3c== "USA") %>% adf()
## lagging doesn't work 



gen_lag("hnwi_nbr_1B", 1) %>% filter(iso3c == "USA") %>% adf()
gen_lag("hnwi_nbr_1B", 2) %>% filter(iso3c == "USA") %>% adf()

## ** debugging convergence: improve timeout, add process-killing

started_mdls <- atb(read.csv(MDL_START_FILE, header = F))
finished_mdls <- atb(read.csv(MDL_END_FILE, header = F))

unfinished_mdls <- setdiff(started_mdls$V1, finished_mdls$V1)

mdl_test <- readRDS(paste0(REG_SPEC_DIR, unfinished_mdls[2]))

run_vrbl_mdl_vars(mdl_test, verbose = T)

lapply(unfinished_mdls, \(x) readRDS(paste0(REG_SPEC_DIR, x)) %>% run_vrbl_mdl_vars())

mclapply(unfinished_mdls, \(x) readRDS(paste0(REG_SPEC_DIR, x)) %>% run_vrbl_mdl_vars(), mc.cores = 4) 

stata_time_test <- function(n) {
    stata(paste0("sleep ", n*1000))
}

withTimeout({stata_time_test(2)}, timeout = 1)

withTimeout(Sys.sleep(2), timeout = 1, TimeoutException = function(ex) {message("Timeout. Skipping.")})

library(R.utils)

library(fscaret)
x <- fscaret::timeout(stata_time_test(2), seconds = 1)

x <- tryCatch({fscaret::timeout(Sys.sleep(2), seconds = 1)}, error = function(e) {NULL})

stata_inf_loop <- function() {
    pid <- Sys.getpid()

    print(pid)
    
    stata("while 1 == 1 {
ds
}")}



## get all processes
library(ps)
library(dplyr)
atb <- as_tibble
adf <- as.data.frame

ps1 <- ps() %>% select(pid, status1 = status, name)
ps1$t1 <- Sys.time()
Sys.sleep(10)
ps2 <- ps() %>% select(pid, status2 = status, name)
ps2$t2 <- Sys.time()
Sys.sleep(10)
ps3 <- ps() %>% select(pid, status3 = status, name)
ps3$t3 <- Sys.time()


ps_tbl <- Reduce(\(x,y) merge(x, filter(y, name == "R")), list(ps1, ps2, ps3)) %>% atb()

select(ps_tbl, pid, status1, status2, status3) %>% adf()



filter(ps_tbl, status1=="sleeping", status2=="sleeping", status3=="sleeping", pid != 2653630) %>% pull(pid) %>%
        lapply(\(x) ps_kill(ps_handle(x)))


timeout(stata_inf_loop(), seconds = 2)

## only run this when not converged: wait for 12 secs

## ** filtering random coefs

df_anls_prep_join <- df_anls_within_prep %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    summarize(base_lag_spec_id = sample(unique(base_lag_spec_id), 5))
df_anls_within <- merge(df_anls_within_prep, df_anls_prep_join) %>% atb()


df_anls_within %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    summarize(cnt = len(unique(base_lag_spec_id))) %>% adf()
## huh strange, the number of specs is the same in cbn_all and cbn_no_cult_spending_and_mitr
## fuck it, just use joins

df_anls_prep_join %>%
    group_by(cbn_name, vrbl_name_unlag) %>%

filter(df_anls_prep_join, vrbl_name_unlag == "hnwi_nbr_30M")

## maybe it's replacement? yeah seems so


## seq(len(unique)) can still result in non-unique base lag-specs being picked
df_anls_within <- df_anls_within_prep %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    ## filter(base_lag_spec_id %in% sample(base_lag_spec_id, 5))
    ## filter(base_lag_spec_id <= 20)
    filter(base_lag_spec_id %in% seq(len(unique(base_lag_spec_id))))


## try random filtering
## some models not converged, especially visible in the hnwi_nbr_1B case
## group by lag: some lags have less points then they should have
df_anls_within %>%
    group_by(cbn_name, vrbl_name_unlag, lag) %>%
    summarize(cnt = len(coef)) %>%
    ## pull(cnt) %>% table()
    filter(cnt < 5)
## here it's sptinc992j, but can be all kinds of variables, also can be none
## so there's enough data, but selection doesn't work properly 


## seeing how many unique base_lag_specs there are: quite enough 
df_anls_within_prep %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    summarize(cnt = len(unique(base_lag_spec_id))) %>% adf()



## group by base_lag_spec (line): every line has 5 obs
df_anls_within %>%
    group_by(vrbl_name_unlag, cbn_name, base_lag_spec) %>%
    summarize(cnt = len(coef)) %>%
    pull(cnt) %>% table()



## ** decently fast analysis of tmitr_interaction having same value as main value
jj <- lapply(reg_spec_mdls, \(x) t(x$lngtd_vrbls[x$lngtd_vrbls$vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact"),])[2,]) %>% Reduce(\(x,y) rbind(x,y), .) %>% atb(.name_repair = ~c("v1", "v2"))

jj <- mclapply(reg_spec_mdls, \(x) t(x$lngtd_vrbls[x$lngtd_vrbls$vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact"),])[2,], mc.cores = 6)
kk <- do.call(rbind, jj) %>% atb(.name_repair = ~c("v1", "v2"))
filter(kk, v1 != v2)


## ps() %>% filter(name == "R", pid != Sys.getpid()) %>% pull(ps_handle) %>% lapply(ps_kill)

## ** tax incentive interaction debugging
reg_spec <- reg_spec_mdls[[1]]



t1 <- Sys.time()
x <- lapply(reg_spec_mdls[1:100], \(x) x$lngtd_vrbls)
            ## \(x) filter(x$lngtd_vrbls, vrbl %in% c("tmitr_approx_linear20step", "ti_tmitr_interact")))
            ## %>%
            ## pivot_wider(names_from = vrbl, values_from = lag))
## %>% rbindlist()
t2 <- Sys.time()
print(t2-t1)

filter(x, tmitr_approx_linear20step != ti_tmitr_interact)
## not always the same 


## ** pid bugfixing

lapply(reg_spec_mdls[1:10],run_vrbl_mdl_vars)

timeout_test <- function() {
    cur_dir <- getwd()
    pid <- Sys.getpid()
    new_dir = paste0(PROJECT_DIR, "pid_dir/", pid)

    system(paste0("mkdir ", new_dir))
    setwd(new_dir)
    

    x <- 1+1
    print(x)

    setwd(cur_dir)
    
}

timeout(timeout_test(), seconds = 2)


## ** multi_lag_testing

test_mdl1 <- reg_spec_mdls[[1]]

test_mdl1$mdl_vars <- c(test_mdl1$mdl_vars, "sptinc992j_p99p100_lag2", "sptinc992j_p99p100_lag3", "sptinc992j_p99p100_lag4", "sptinc992j_p99p100_lag5", "smorc_dollar_fxm_lag1", "smorc_dollar_fxm_lag3", "smorc_dollar_fxm_lag4", "smorc_dollar_fxm_lag5")

run_vrbl_mdl_vars(test_mdl1, verbose = T)
## huh that works pretty well.... maybe all my work was for nothing?
## maybe should have been more 

test_mdl2 <- reg_spec_mdls[[3]]
test_mdl2$mdl_vars <- c(test_mdl2$mdl_vars, "sptinc992j_p99p100_lag2", "sptinc992j_p99p100_lag3", "sptinc992j_p99p100_lag4", "sptinc992j_p99p100_lag5", "hnwi_nbr_1B_lag2", "hnwi_nbr_1B_lag3", "hnwi_nbr_1B_lag4")
run_vrbl_mdl_vars(test_mdl2, verbose = T)

## full model doesn't run
## also not when removing clctr_cnt_cpaer_lags
## also not when also removing additional population lags
## also not when removing additional GDP lags
## also not when removing additional shweal lags
## runs after also removing additional hnwi_lags
## runs with 1 and 2 additional hnwi_lags, breaks down when adding third additional lag
## -> seems my hunch was completely correct

## ** clustering



## * scrap 
## ** first sloppy version 

REG_RES_DIR <- "/home/johannes/ownCloud/reg_res/v5/"
REG_RES_FILE <- "/home/johannes/ownCloud/reg_res/v5.csv"



## generate 100 specs
reg_specs <- lapply(seq(1,600), \(x) gen_reg_spec(non_thld_lngtd_vars)) %>% unique()

## for each of the 100, generate the spec variations -> 3.7k total 
all_spec_variations <- lapply(reg_specs, \(x) vary_spec(x))

## flatten the 3.7k 
all_specs_flat <- Reduce(\(x, y) c(x,y), all_spec_variations) %>% unique()


## t1 = Sys.time()
## lapply(all_specs_flat, \(x) run_spec(x, base_vars))
## t2 = Sys.time()


## gets stuck after ~70 models, which isn't even a complete spec per thread..
t1 = Sys.time()
mclapply(all_specs_flat, \(x) run_spec(x, base_vars), mc.cores = 6)
t2 = Sys.time()

print(t2-t1)



## * glmmTMB

fwrite(select(cbn_dfs$cbn_all, iso3c_num, year, nbr_opened, smorc_dollar_fxm_lag1, NY.GDP.PCAP.CDk_lag1,
                   Ind.tax.incentives, nbr_opened_cum_lag1),
            paste0(PROJECT_DIR, "data/processed/glmmTMB_test.csv"))


library(glmmTMB)

## compare glmmTMB and glmer.nb, which has to be RE (time-invariant models work)
## with somewhat similar results (within 10%)

t1 = Sys.time()
r_glmmtmb <- glmmTMB(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives +
                     (1 | iso3c),
                     ## (smorc_dollar_fxm_lag1 | iso3c) + (NY.GDP.PCAP.CDk_lag1 | iso3c)
                 data = cbn_dfs$cbn_all,
                 family = nbinom2)
t2 = Sys.time()
t2-t1

r_glmernb <- glmer.nb(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives + (1 | iso3c),
                      data = cbn_dfs$cbn_all, verbose = T)

r_glmer <- glmer(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives + (1 | iso3c),
                 data = cbn_dfs$cbn_all, verbose = T)

## ** fitting some random slope models lalala

r_glmmtmb_rs <- glmmTMB(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives +
                            (1 + smorc_dollar_fxm_lag1 | iso3c),
                 data = cbn_dfs$cbn_all,
                 family = nbinom2)

r_glmmtmb_rs2 <- glmmTMB(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives +
                            (1 + NY.GDP.PCAP.CDk_lag1 | iso3c),
                 data = cbn_dfs$cbn_all,
                 family = nbinom2)

## see if variation of random slope of smorc_dollar depends on tax incentives
## https://stats.stackexchange.com/questions/156237/interactions-between-levels-in-lme4: this is the format they suggest as well
r_glmmtmb_rs3 <- glmmTMB(nbr_opened ~ smorc_dollar_fxm_lag1*Ind.tax.incentives + NY.GDP.PCAP.CDk_lag1 + 
                            (1 + smorc_dollar_fxm_lag1 | iso3c),
                 data = cbn_dfs$cbn_all,
                 family = nbinom2)

## https://rpubs.com/corey_sparks/70812: try different specification of cross-level interaction -> 
r_glmmtmb_rs4 <- glmmTMB(nbr_opened ~ smorc_dollar_fxm_lag1*Ind.tax.incentives + NY.GDP.PCAP.CDk_lag1 + 
                            (1 | iso3c),
                 data = cbn_dfs$cbn_all,
                 family = nbinom2)


screenreg(list(r_glmmtmb, r_glmmtmb_rs, r_glmmtmb_rs3, r_glmmtmb_rs4))

## ** interpreation of variance-covariance, lrtests
summary(r_glmmtmb_rs)



library(lmtest)
x <- lrtest(r_glmmtmb, r_glmmtmb_rs)

screenreg(list(r_glmmtmb,r_glmernb, r_glmer, r_glmmtmb_rs, r_glmmtmb_rs2), digits = 5)

VarCorr(r_glmer) %>%
  as_data_frame() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  select(grp, icc)



## see if lmer has the same as:
## - xtreg: very similar values
## - lmer is maximum likelihood, and xtreg is OLS/GLS:
## https://stats.stackexchange.com/questions/143705/maximum-likelihood-method-vs-least-squares-method ML and OLS seem to be very similar 
r_lmer <- lmer(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives + (1 | iso3c),
               data = cbn_dfs$cbn_all)

screenreg(r_lmer, digits = 5)                                        
##   nbr_opened |      Coef.      
## -------------+-----------                                    
## smorc_doll~1 |   .4673727      smorc_dollar_fxm_lag1       0.45770 *** (0.03304)   
## nygdppcapc~1 |  -.0155816      NY.GDP.PCAP.CDk_lag1       -0.01618  (0.03238)
## indtaxince~s |   .0183852      Ind.tax.incentives          0.01957 (0.08819)   
##        _cons |   .2431858      (Intercept)                 0.23875 **  (0.08190)                              


## ** DHARMa

library(DHARMa)
sim_resids <- simulateResiduals(r_glmmtmb)
residuals(sim_resids) %>% hist()

residuals(sim_resids, quantileFunction = qnorm, outlierValues = c(-7,7)) %>% hist()

plot(sim_resids)

testZeroInflation(sim_resids)

plotResiduals(sim_resids, cbn_dfs$cbn_all$smorc_dollar_fxm_lag1, rank = F, quantreg = T)


plotResiduals(sim_resids, cbn_dfs$cbn_all$NY.GDP.PCAP.CDk_lag1, rank = F)



## ** dynamic panel models 
data("EmplUK", package = "plm")
     
## Arellano and Bond (1991), table 4 col. b 
z1 <- pgmm(log(emp) ~ plm::lag(log(emp), 1:2) + plm::lag(log(wage), 0:1)
           + log(capital) + plm::lag(log(output), 0:1) | plm::lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")

summary(z1, robust = FALSE)



## ** compare lmer and plm 


## compare plm and lmer: basically the same (r_lmer and plm_re), PLM seem to use OLS;
## R^2 between plm and stata is quite off tho: stata 0.38 total, plm has 0.127
## when specifying between model: R^2 of stata and pglm basically agrees: 0.73
## xtreg, be and plgm(model = "between") are basically the same: stata: regression on group means, pglm has n=88
r_plm_re <- plm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
          index = "iso3c",
         data = cbn_dfs$cbn_all, model = "random", effect = "individual")
## within model: drops 
r_plm_fe <- plm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
          index = "iso3c",
         data = cbn_dfs$cbn_all, model = "within", effect = "individual")
r_plm_be <- plm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
          index = "iso3c",
         data = cbn_dfs$cbn_all, model = "between", effect = "individual")


screenreg(list(r_lmer,r_plm_re,r_plm_fe, r_plm_be), digits = 5)

## r_plm_fe is same as xtreg, fe

## ** again testing pglm: complete garbage with random model

## random negbin is garbage, fe/be plausible (and neither possible in stata)

r_pglm_re <- pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
                  index = "iso3c",
                  data = cbn_dfs$cbn_all,
                  model = "random",
                  effect = "individual",
                  family = negbin,
                  reltol = 1e-4)
                  ## print.level = 5,
                  ## method = "nm"
screenreg(r_pglm_re)

## optim_methods <- c("bfgs", "bfgsr", "cg", "nm")
optim_methods <- c("nr", "bfgs", "bfgsr", "bhhh", "sann", "cg", "nm")

r_pglm_res <- lapply(optim_methods, \(x)
                     pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
                          index = "iso3c",
                          data = cbn_dfs$cbn_all,
                          model = "random",
                          effect = "individual",
                          family = negbin,
                          method = x))

screenreg(r_pglm_res[c(1,4,7)])

## *** test passing starting values to pglm


pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
     index = "iso3c",
     data = cbn_dfs$cbn_all,
     model = "random",
     effect = "individual",
     family = negbin,
     start = rep(1000000000000,5))


## *** inspecting source code 

untar(download.packages(pkgs = "Matrix",  destdir = ".",  type = "source")[,2])

## *** within/between


## try within model: 

r_pglm_fe <- pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1, # + Ind.tax.incentives,
          index = "iso3c",
          data = cbn_dfs$cbn_all, model = "within", effect = "individual", family = negbin)

## between 
r_pglm_be <- pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1, # + Ind.tax.incentives,
          index = "iso3c",
          data = cbn_dfs$cbn_all, model = "between", effect = "individual", family = negbin)

## run FE models with base packages
library(MASS)

r_glm <- glm.nb(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + factor(iso3c),
                data = cbn_dfs$cbn_all)


screenreg(list(r_pglm_re, r_pglm_fe, r_glm), custom.model.names = c("r_pglm_re", "r_pglm_fe", "r_glm"))

## ** test glmmadaptive
install.packages("GLMMadaptive")
library(GLMMadaptive)

t1 = Sys.time()
r_glmma <- mixed_model(fixed = nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
                       random = ~ 1 | iso3c,
                       data = cbn_dfs$cbn_all,
                       family = GLMMadaptive::negative.binomial())
t2 = Sys.time()
t2-t1


summary(r_glmma)

screenreg(r_glmernb)

## ** test poisson


r_glmer_poi <- glmer(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives + (1 | iso3c),
                     data = cbn_dfs$cbn_all, family = poisson)

r_glmer_poi_xx <- glmer(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives +
                            (1 + NY.GDP.PCAP.CDk_lag1 + smorc_dollar_fxm_lag1 + Ind.tax.incentives| iso3c) +
                            (1|iso3c),
                     data = cbn_dfs$cbn_all, family = poisson)

r_pglm_poi_re <- pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
          index = "iso3c",
          data = cbn_dfs$cbn_all, model = "random", effect = "individual", family = poisson)

poi_mdls_cpr <- list(r_pglm_poi_re, r_glmer_poi, r_glmernb, r_glmer_poi_xx, r_wbm)
screenreg(poi_mdls_cpr, digits = 5, single.row = F,
          custom.model.names = c("r_pglm_poi_re", "r_glmer_poi", "r_glmernb", "r_glmer_poi_xx", "r_wbm"))

## does pglm produces same results as xtpoisson? yup (previously had copied wrong model output)

##                   stata                   r_pglm_poi_re                 r_glmer_poi              
## smorc_doll~1 |    .521141   .1074379      0.52114 (0.10744) ***        0.58984 (0.11378) ***    
## nygdppcapc~1 |   .1938606   .1733529      0.19386 (0.17335)            0.15664 (0.15956)        
## indtaxince~s |   1.234667   .5965622      1.23467 (0.59656) *          1.01886 (0.59307)         
##        _cons |  -3.138592    .570038    -3.13859 (0.57004) ***        -3.49400 (0.58121) ***    
                                                                             
## glmber and pglm : also somewhat different, and r_glmer_poi_re has

library(parameters)
parameters(r_glmer_poi)
parameters(r_glmernb)
parameters(r_pglm_poi_re)
parameters(r_glmer_poi_xx)







## glmer poisson doesn't produce same as pglm: maybe 10-20% difference in parameter size 

## does glmer.nb produce same as xtnbreg? nope but how much different? 

## ** try panelR

install.packages("panelr")
library(panelr)

wbm_data <- panel_data(cbn_dfs$cbn_all, id = iso3c, wave = year)


r_wbm <- wbm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 | Ind.tax.incentives,
             data = wbm_data, family = MASS::negative.binomial(theta = 12.5281712))
screenreg(r_wbm)

## ** try bbmle
library(bbmle)


r_bbmle <- mle2(nbr_opened ~ dnbinom(mu = exp(logmu), size = exp(logtheta)),
     parameters = list(logmu ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives),
     data = cbn_dfs$cbn_all, 
     start = list(logmu = 0, logtheta = 0))
     



## ** margins tests: not working well 
devtools::install_github("benjaminguinaudeau/margins.pglm")

library(margins.pglm)
margins::margins(y2)

formula <- union ~ exper + wage 

anb <- pglm(formula, union_wage, family = binomial('probit'), 
            model = "pooling", method = "bfgs", print.level = 3, R = 5)

summary(margins::margins(model = anb, formula = formula))
m <- margins::margins(model = anb, at = list("wage" = c(-3:4)), formula = formula)



## ** debugging pglm

## running lnl.negbin with breakpoints:
## betas go completely bonkers in third iteration, are already when arriving 
## gradients and Hessian don't seem super weird after second run 

grad_r1 <<- gradi
H_r1 <<- H
l_r1 <<- l


grad_r2 <<- gradi
H_r2 <<- H
l_r2 <<- l

## compare gradients 

cpr_grads_dt <- rbind(
    adt(grad_r1) %>% melt() %>% .[, source := "r1"],
    adt(grad_r2) %>% melt() %>% .[, source := "r2"])

cpr_grads_smry <- cpr_grads_dt[, .(mean_value = mean(value), sd = sd(value)), by = .(source, variable)] 

ggplot(cpr_grads_smry, aes(x=variable, y=mean_value, group = source, fill = source)) +
    geom_bar(stat = "identity", position = "dodge")

## much smaller gradients in r2 as it should be

mean(H_r1)
mean(H_r2)
sd(H_r1)
sd(H_r2)

## mean and SD of Hession also smaller in r2

mean(l_r1)
mean(l_r2)
sd(l_r1)
sd(l_r2)

## mean/sd increase in r2 but this shouldn't really matter, this is estimation 

## let's see what happens between lnl.negbin calls
## effectively seems to be called by maxNRCompute, which seems standard MLE function




## see whether to find some MWE

data("PatentsRDUS", package="pglm")

fwrite(PatentsRDUS, paste0(PROJECT_DIR, "data/processed/PatentsRDUS.csv"))

r_nb_wi <- pglm(patents ~ rd  + sumpat, PatentsRDUS,
                 family = negbin,
                 model = "within", print.level = 4,
                 index = "cusip")

r_nb_ra <- pglm(patents ~ rd  + sumpat, PatentsRDUS,
                 family = negbin,
                 model = "random", print.level = 4,
                 index = "cusip")

r_poi_ra <- pglm(patents ~ rd + sumpat , PatentsRDUS,
                 family = poisson,
                 model = "random", print.level = 4,
                 index = "cusip")

screenreg(list(r_nb_wi, r_nb_ra, r_poi_ra), digits = 5)

x <- pglm(count ~ spp + mined, Salamanders, family = negbin,  model = "within", index = "site")
y <- pglm(count ~ spp + mined, Salamanders, family = negbin,  model = "random", index = "site")

screenreg(list(x,y))

data("cbpp", package = "lme4")

fwrite(cbpp, paste0(PROJECT_DIR, "data/processed/cpbb.csv"))

cbpp_dt <- adt(cbpp)[, N := .N, herd][N > 1]

r_within <- pglm(incidence ~ size, cbpp, family = negbin, model = "within", index = "herd", print.level = 4)
r_random <- pglm(incidence ~ size, cbpp, family = negbin, model = "random", index = "herd", print.level = 1, tol = 1e-30)

screenreg(list(r_within, r_within_stata,r_random), digits =5)

wage_data_num <- adt(WageData)[, .(wage_num = round(lwage), wks, blk, id)]

c <- pglm(wage_num ~ wks + blk, wage_data_num, family = negbin, model = "within", index = "id")
d <- pglm(wage_num ~ wks + blk, wage_data_num, family = negbin, model = "random", index = "id")



## ** writing SE post

library(pglm)
library(lme4)
library(texreg)

data("cbpp", package = "lme4")

r_within <- pglm(incidence ~ size, cbpp, family = negbin, model = "within", index = "herd") 
r_random <- pglm(incidence ~ size, cbpp, family = negbin, model = "random", index = "herd")

screenreg(r_random, digits=5)

r_random_poi <- pglm(incidence ~ size, cbpp, family = poisson, model = "random", index = "herd")
screenreg(r_random_poi, digits = 5)



r_random <- pglm(incidence ~ size, cbpp, family = negbin, model = "random", index = "herd", print.level = 4)


data("PatentsRDUS", package="pglm")
r_patents <- pglm(patents ~ rd  + sumpat, PatentsRDUS, family = negbin,  model = "random", index = "cusip")
screenreg(r_patents, digits = 5)

## ** test menbreg

library(lme4)

menbreg_res <- glmer.nb(incidence ~ size + (1 | herd), cbpp)
screenreg(menbreg_res, digits = 6)
adt(mtcars)

mtcars

iris


## ** updating squared terms

## calculate new squared values: based on 
cfgs <- names(cbn_dfs3$cbn_all) %>% keep(~grepl("_sqrd", .x)) %>% setNames(., gsub("_sqrd", "", .))
dtx <- adt(cbn_dfs3$cbn_all)
iwalk(as.list(cfgs), ~dtx[, get(.x) := get(.y)^2])
sd(dtx$smorc_dollar_fxm_sqrd_lag0)

vlu_vec <- list(iso3c_num = 10, iso3c = "asdf")

## lapply test: accessing external values doesn't really work well 
dtx[, lapply(.SD, \(x) vlu_vec[x]), .SDcols = c("iso3c_num", "iso3c")][1:3]

## lapply assignment by reference: doesn't work: .SD is locked
dtx[, lapply(cfgs, \(x) .SD[,unname(x)[[1]] := names(x)])]

## try without renaming in place 
dtx[, .(lapply(as.list(cfgs), \(x) unname(x)[[1]] = names(x)))]

## try with get
dtx[, .(lapply(as.list(cfgs), get(unname(x)[[1]]) = get(names(x))))]

## try with entire .SD
copy(dtx)[, lapply(.SD, \(x) x = x^2), .SDcols = names(cfgs)] %>%
    setnames(old = names(cfgs), new = unname(cfgs)) %>%
    cbind(dtx[, .SD, .SDcols = setdiff(names(dtx), names(.))], .) %>%
    melt(id.vars = c("iso3c", "year")) %>%
    .[, .(sd = sd(value), mean = mean(value)), variable] %>% print(n=200)


## combine into single block? dtx given 
x <- keep(names(dtx), ~grepl("_sqrd", .x)) %>% gsub("_sqrd", "", .)
## dtx[, .SD^2, .SDcols = .]
## .[, lapply(.SD, \(x) x = x^2), .SDcols = .]
x <- dtx[, .SD^2, .SDcols = gsub("_sqrd", "", keep(names(dtx), ~grepl("_sqrd", .x)))]



## seems to work 
    ## copy(dtx)[, .SD^2, .SDcols = gsub("_sqrd", "", keep(names(dtx), ~grepl("_sqrd", .x)))] %>%
    ##     setnames(old = gsub("_sqrd", "", keep(names(dtx), ~grepl("_sqrd", .x))),
    ##              new = keep(names(dtx), ~grepl("_sqrd", .x))) %>%
##     cbind(dtx[, .SD, .SDcols = setdiff(names(dtx), names(.))], .)

## list(linear = . ,sqrd = gsub("_sqrd", "", .)) needs to be named vector for setnames
 
        
    
        dtx[, .SD^2, .SDcols = names(.)]

    
    x1 <- dtx[, .SD^2, .SDcols = names(cfgs)]
    
    ## all(x==x1)
    ## dtx[, .SD := .SD^2]

    ## melt into superlong
    ## squared original: needed for names

    cfg_dt <- data.table(name_base = names(cfgs), name_sqrd = cfgs)
        
    dtx_sqrd_old <- dtx[, .SD, .SDcols = c("iso3c", "year", cfg_dt[, name_sqrd])]  %>%
        melt(id.vars = c("iso3c", "year")) %>%
        cfg_dt[., on = .(name_sqrd = variable)] # get the variable name of the unsquared variable 
        ## .[, lapply(.SD, \(x) sum(is.na(x)))]
        

    dt_usqrd <- dtx[, .SD, .SDcols = c("iso3c", "year", cfg_dt[, name_base])] %>%
        melt(id.vars = c("iso3c", "year"), value.name = "value_usqrd")

    dtx_sqrd_old[dt_usqrd, on = .(iso3c, year, name_base = variable)] %>%
        .[, value_sqrd_new := value^2] %>%
        .[, .(iso3c, year, name_sqrd, value_sqrd_new)] %>%
        dcast.data.table(iso3c + year ~ name_sqrd, value.var = "value_sqrd_new") %>% atb()
        ## .[, lapply(.SD, sd)] %>% atb() ## %>% melt() %>% print(n=30)
        
    
## ** country cherry picking
## shitty way of getting example countries
## get some country examples
dt_topchng_crys <- dtx_consvrbl_2k %>% copy() %>%
    .[, pred_orig := exp(predict(rx, dtx_consvrbl_2k_prep))] %>% 
    .[, .(diff1 = sum(nbr_opened) - sum(pred),
          diff2 = sum(pred_orig) - sum(pred)), iso3c] %>%
    ## .[order(-abs(diff1))] %>% head(6) %>% 
    .[order(-abs(diff2))] %>% head(3)

## get the data for those
dtiv_vis <- dtx_consvrbl_2k %>% copy() %>%
    .[, pred_orig := exp(predict(rx, dtx_consvrbl_2k_prep))] %>%
    .[, c(vvs$base_vars, paste0(vrblx, "_bu"), "pred", "pred_orig", "nbr_opened"), with = F] %>%
    .[dt_topchng_crys[, .(iso3c)], on = .(iso3c)]


## construct scaler 
sclr <- dtiv_vis %>% copy() %>% # .[, .N, variable] %>%
    melt(id.vars = c("iso3c", "year")) %>% 
                                        # set variable type: either in scale of IV or DV 
    .[, vrbltype := fifelse(variable == paste0(vrblx, "_bu"), "iv", "dv")] %>% 
    .[, .(max = max(value), min = min(value), rng = max(value) - min(value)), vrbltype] %>%
    melt() %>% dcast.data.table(...~vrbltype + variable) %$% # some awkward melting to get access as names 
    list(a = dv_min - ((dv_rng/iv_rng) * iv_min), b = dv_rng/iv_rng) # intercept and slope


## actual rescaling of IV
dtiv_vis %>% copy() %>%
    .[, shweal992j_p90p100_lag4_bu := (shweal992j_p90p100_lag4_bu * sclr$b) + sclr$a] %>%
    melt(id.vars = c("iso3c", "year")) %>%
    ggplot(aes(x=year, y=value, color = variable)) +
    ## geom_smooth(se=F, span = 0.4) +
    geom_line() + 
    facet_wrap(~iso3c) +
    scale_y_continuous("openings", sec.axis = sec_axis(~ (. * 1/sclr$b) - sclr$a, name = "closings"))


## ## plotting in two facets
## dtiv_vis %>% copy() %>%
##     melt(id.vars = c("iso3c", "year")) %>%
##     .[, vrbltype := fifelse(variable == paste0(vrblx, "_bu"), "iv", "dv")] %>% 
##     ggplot(aes(x=year, y=value, group = interaction(iso3c, variable), color = variable)) +
##     geom_smooth(se = F) +
##     facet_grid(vrbltype~iso3c, space = "free", scales = "free")

## plotting as scatterplot
## there is just no fucking point, looks ugly af: lines criss-crossing everywhere
## dtiv_vis %>% copy() %>%
##     melt(id.vars = c("iso3c", "year", "shweal992j_p90p100_lag4_bu")) %>%
##     ggplot(aes(x=shweal992j_p90p100_lag4_bu, y=value, color = variable)) +
##     geom_path() +
##     facet_grid(~iso3c, space = "free", scales = "free")


explr_decps <- function() {
    #' try to decompose regression into coefficient components
    #' but doesn't seem to work well: components can be fully negative (when all values are neg, and coef pos)
    #' but still have positive contribution

    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)
    idx <- mdl_id_dt$mdl_id[7]
    regspecx <- get_reg_spec_from_id(idx, fldr_info)

    dfx <- chuck(cbn_df_dict, "rates", chuck(regspecx, "cfg", "cbn_name"))
    iv_vars <- keep(setdiff(regspecx$mdl_vars, vvs$base_vars), ~!grepl("^SP\\.POP\\.TOTLm", .x))

    ## generate formula
    fx <- gen_r_f("rates", iv_vars)

    ## generate model 
    rx <- glmmTMB(fx, dfx, family = nbinom2)

    dt_decps <- adt(dfx)[, `:=`(pred_orig = exp(predict(rx, dfx)),
                                pred_nocons = exp(predict(rx, adt(dfx)[, iso3c := "lul"])))]

    dt_decps %>%
        melt(id.vars = c("iso3c", "year"), measure.vars = c("pred_orig", "pred_nocons")) %>%
        .[iso3c %in% c("DEU", "USA", "KOR", "IND", "CHN")] %>%
        ggplot(aes(x=year, y=value, color = iso3c, linetype = variable)) + geom_line()
    
    ## more decomposition: make into superlong

    ## get rx variables
    dt_vrbl <- fixef(rx) %>% chuck("cond") %>% data.table(vrbl = names(.)) %>% .[, .(vrbl, coef = .)] %>% 
        .[vrbl != "(Intercept)"] %>%
        rbind(list(vrbl = "SP_POP_TOTLm_lag0_uscld", coef = 1)) # add population exposure

    ## get country intercepts
    
    summary(rx) %>% coef()
    
    dt_ranef <- coef(rx) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>%
        .[, .(iso3c, coef = `(Intercept)`, value = 1, vrbl = "cons")] %>%
        .[dt_decps[, .(iso3c, year)], on = "iso3c"] # expand with actual data to get years
        
    ## dt_ranef[iso3c == "DEU"]

    ## melt into superlong
    dt_decps_splong <- dt_decps[, c("iso3c", "year", dt_vrbl$vrbl), with = F] %>%
        melt(id.vars = c("iso3c", "year"), variable.name = "vrbl") %>%
        dt_vrbl[., on = "vrbl"] %>% ## merge fixed effects
        .[vrbl == "SP_POP_TOTLm_lag0_uscld", value := log(value)] %>% # rescale population to exposure (log)
        rbind(dt_ranef) # rbind intercepts

    ## test that predictions work
    dt_decps_splong[, .(pred_mnl = sum(coef * value)), .(iso3c, year)] %>%
        .[, pred_orig := predict(rx, dfx)] %>%
        .[, diff := pred_mnl - pred_orig] %>% summary()
        .[iso3c == "DEU"] %>% print(n=200)
    ## looks good

    ## get coef components, some diagnostics 
    dt_decps_pred <- dt_decps_splong %>% copy() %>%
        .[ vrbl != "SP_POP_TOTLm_lag0_uscld"] %>% # filter out population -> per million rates
        .[, coef_cpnt := coef*value] %>% 
        .[, pred_mnl := sum(coef_cpnt), .(iso3c, year)] %>%
        ## .[, pred_prop := (coef*value)/pred_mpnl] %>% # proportion of overall prediction explained by coef
        .[, pred_diff := pred_mnl-coef_cpnt] # how different prediction would be without vrbl

    ### aggregate coef_cpnts of squared terms
    dt_decps_cbn <- copy(dt_decps_pred) %>%
        .[grepl("tmitr", vrbl) | vrbl == "Ind.tax.incentives", vrbl := "tax_incentives"] %>% 
        .[grepl("pm_density_global", vrbl), vrbl := "density_global"] %>% 
        .[grepl("pm_density", vrbl), vrbl := "density_country"] %>%
        ## .[grepl("smorc_dollar_fxm", vrbl), vrbl := "smorc_cbn"] %>%
        .[grepl("cnt_contemp_1990", vrbl), vrbl := "muem_cbn"] %>%
        .[, .(coef_cpnt = sum(coef_cpnt)), .(iso3c, year, vrbl)]

    dt_decps_vis <- dt_decps_cbn[iso3c == "DEU" & vrbl != "cons"] # focus one country, filter out cons
    
    ## test single country
    dt_decps_vis <- dt_decps_pred[iso3c == "DEU"]#   & year == 2010]
    dt_decps_vis[year == 2019]

    ggplot() +
        geom_line(dt_decps_vis, mapping = aes(x=year, y=coef_cpnt, color = vrbl)) + 
        geom_text(dt_decps_vis[, .SD[which.max(year)-2], vrbl][order(-abs(coef_cpnt))][1:5],
                  mapping= aes(x=max(year), y=coef_cpnt, label = vrbl))

    ## overall decomposition
    dt_decps_splong %>% copy() %>%
        ## .[vrbl != "shweal992j_p90p100_lag4"] %>%
        .[vrbl != "sptinc992j_p90p100_lag3"] %>% 
        .[, .(pred = exp(sum(coef*value))), .(iso3c, year)] %>%
        .[, sum(pred)]
    
    ## one-out
    dt_decps_splong[, unique(vrbl)] %>%
        data.table(vrbl = .,
                   predwo = map_dbl(.,
                                    ~copy(dt_decps_splong)[vrbl != .x, .(pred = exp(sum(coef*value))),
                                                           .(iso3c, year)] %>%
                                        .[, sum(pred)]),
                   predfull = dt_decps_splong[, .(pred = exp(sum(coef*value))), .(iso3c, year)][, sum(pred)]) %>%
        .[, diff := predfull - predwo]

    ## one-out with combined variable (already per million capita rate)
    dt_decps_cbn[, unique(vrbl)] %>%
        data.table(vrbl = .,
                   predwo = map_dbl(., ~dt_decps_cbn[vrbl != .x, .(pred = exp(sum(coef_cpnt))), .(iso3c,year)] %>%
                                           .[, mean(pred)]),
                   predfull = dt_decps_cbn[, .(pred = exp(sum(coef_cpnt))), .(iso3c, year)][, mean(pred)]) %>%
        .[, prop := predfull/predwo]

    
    dt_decps_cbn[vrbl == "shweal992j_p90p100_lag4"] %>%
        ## .[iso3c %in% sample(iso3c, 5)] %>% 
        ggplot(aes(x=year, y=coef_cpnt, group = iso3c)) + geom_line()


    
    ## look at density coef_cpnt in beginning: are negative
    ## but probably due to taking entire CY range into account: if no PM would be even more negative
    dt_decps_pred[iso3c == "DEU" & grepl("pm_density", vrbl) & !grepl("sity_global", vrbl)
                  & year %in% c(2000, 2002, 2005)]



    ## maybe average proportion of variable? 
    dt_decps_pred %>% copy() %>% # first get positive and negative parts
        ## .[, sign := sign(coef_cpnt)] %>%
        ## .[sign == 1, ctrbn_pos := sum(coef_cpnt), .(iso3c, year)] %>%
        ## .[sign == -1, ctrbn_neg := sum(coef_cpnt), .(iso3c, year)] %>%
        .[, .(iso3c, vrbl, coef_cpnt, sign = sign(coef_cpnt))] %>%
        .[, ctrbn := sum(coef_cpnt), .(iso3c, vrbl, sign)] %>%
        .[, ctrbn_prop := coef_cpnt/ctrbn] %>%
        .[, mean(ctrbn_prop), .(vrbl, sign)] %>% print(n=50)
        

    ## test relation between full prediction and without population
    dt_cpr_dvfmts <- rbind(
        copy(dt_decps_splong)[, src := "full"],
        copy(dt_decps_splong)[vrbl != "SP_POP_TOTLm_lag0_uscld"][, src := "wopop"]) %>%
        .[, .(pred_mnl = sum(coef*value)), .(iso3c, year, src)]

    ## plot some countries against different sources (full model, wopop)
    dt_cpr_dvfmts %>%
        ## dcast.data.table(iso3c + year ~ src, value.var = "pred_mnl") %>%
        .[iso3c %in% c("USA", "BEL", "NLD", "DEU")] %>%
        ggplot(aes(x=year, y=pred_mnl, color = iso3c, linetype = src)) + geom_line()
    
    ## see if wopop predictions make sense as per million rates
    copy(dt_cpr_dvfmts) %>% dcast.data.table(iso3c + year ~ src, value.var = "pred_mnl") %>%
        .[dt_decps[, .(iso3c, year, pop = SP_POP_TOTLm_lag0_uscld)], on = .(iso3c, year)] %>%
        .[, opng_rate_wopop := exp(wopop)] %>%
        .[, opng_cnt := opng_rate_wopop * pop] %>%
        .[, opng_cnt_log := log(opng_cnt)] %>%
        .[, diff := opng_cnt_log - full] %>% summary()
        
          

    ## test relation between constant and population: looks pretty linear
    dt_decps_pred %>% copy() %>%
        .[vrbl %in% c("cons", "SP_POP_TOTLm_lag0_uscld")] %>%
        .[, .(value = mean(value), coef_cpnt = mean(coef_cpnt)), .(iso3c, vrbl)] %>%
        dcast.data.table(iso3c ~ vrbl, value.var = c("value", "coef_cpnt")) %>% 
        ## dcast.data.table(year + iso3c ~ vrbl, value.var = c("value", "coef_cpnt")) %>% 
        ## lm(coef_cpnt_cons ~ value_SP_POP_TOTLm_lag0_uscld, .) %>% summary()
        ggplot(aes(x=value_SP_POP_TOTLm_lag0_uscld, y=coef_cpnt_cons)) +
        geom_point() + geom_smooth(method = "lm") +
        geom_text_repel(mapping = aes(label = iso3c))


    # difference between predicted full and predicted without intercepts
    # on CY level
    dt_decps %>% copy() %>% 
        .[, diff := pred_nocons - pred_orig] %$% sum(abs(diff))

    ## on country-level
    dt_decps %>% copy() %>% 
        .[, .(sum_orig = sum(pred_orig), sum_nocons = sum(pred_nocons)), iso3c] %>%
        .[, diff := sum_orig - sum_nocons] %$% # first aggregate expectations to country-level
        sum(abs(diff))

    ## for some strange reason they're the same 
    }


## ** dharma testing

test_dharma <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)
    idx <- mdl_id_dt$mdl_id[7]
    regspecx <- get_reg_spec_from_id(idx, fldr_info)

    dfx <- chuck(cbn_df_dict, "rates", chuck(regspecx, "cfg", "cbn_name"))
    iv_vars <- keep(setdiff(regspecx$mdl_vars, vvs$base_vars), ~!grepl("^SP\\.POP\\.TOTLm", .x))

    ## generate formula
    fx <- gen_r_f("rates", iv_vars)

    ## generate model 
    rx <- glmmTMB(fx, dfx, family = nbinom2)

    


    library(DHARMa)
    ## getSimulations
    ## getSimulations(rx)
    ## testDispersion(rx)
    simul_resid <- simulateResiduals(rx, n = 10000)
    ## simul_resid2 <- simulateResiduals(rx, n = 100, refit = T) # doesn't work 
    plot(simul_resid)
    ## simulate.glmmTMB
    ## simulate.merMod

    
    ## try to get residuals "per variable"
    plotResiduals(simul_resid, form = dfx$shweal992j_p90p100_lag4, quantreg = F, rank = F)
    plotResiduals(simul_resid, form = dfx$hnwi_nbr_1M_lag4, quantreg = T, rank = T)
    plotResiduals(simul_resid, form = dfx$smorc_dollar_fxm_lag3, quantreg = T, rank = T)
    plotResiduals(simul_resid, form = dfx$pm_density_lag1, quantreg = T, rank = F)
    plotResiduals(simul_resid, form = dfx$sptinc992j_p90p100_lag3, quantreg = T, rank = F)
    testQuantiles(simul_resid, predictor = dfx$shweal992j_p90p100_lag4)

    plotQQunif(simul_resid)

    ## compare variables
    dfx %>% adt() %>% .[, c(iv_vars, vvs$base_vars, "resid"), with = F] %>%
        melt(id.vars = c(vvs$base_vars, "resid")) %>%
        ggplot(aes(x=value, y = resid)) +
        geom_point(size = 0.2) +
        geom_smooth() + 
        facet_wrap(~variable, scales = "free")
    ## actually HNWI variables don't look great

    residuals(simul_resid)
    
    ## testZeroInflation(simul_resid)
    ## testOverdispersion(simul_resid)
    ## hist(simul_resid)
    
    
    dfx$resid <- residuals(simul_resid)
    ## dfx$resid_scaled <- simul_resid$scaledResiduals
    ## dfx$pred <- predict(rx, dfx)
    ## adt(dfx)[resid > 0.95, .(iso3c, year, resid)][,  %>%
    ##     adt(dfx)[, .(iso3c, year)

    res = recalculateResiduals(res, sel = testData$location == 1)
    testTemporalAutocorrelation(res, time = unique(testData$time))


    ## temporal autocorrelation test: kinda irrelevant
    split(dfx, dfx$iso3c) %>% keep(~nrow(.x) > 1) %>% map(~testTemporalAutocorrelation(
                                      recalculateResiduals(simul_resid, sel = dfx$iso3c ==  unique(.x$iso3c)),
                                      time = .x$year))

    ## recalculateResiduals(simul_resid, group = dfx$iso3c) %>%
    ##     adt(dfx)[, testTemporalAutocorrelation(., time = year), iso3c]

    dfx %>% adt() %>%
        .[, recalculateResiduals(simul_resid, group = iso3c) %>%
            testTemporalAutocorrelation(., time = year), iso3c]

    testTemporalAutocorrelation(simul_resid, time = dfx$year)

    ggplot() +
        geom_tile(dfx, mapping = aes(x=year, y=iso3c, fill = log(nbr_opened/SP_POP_TOTLm_lag0_uscld+0.00001))) +
        geom_point(adt(dfx)[resid > 0.9], mapping = aes(x=year, y=iso3c), color = "red") 

    ## aggregate residuals by country 
    adt(dfx)[, .(mean_resid = mean(resid_scaled)), iso3c] %>%
        .[, iso3c := factor(iso3c, levels = iso3c[order(mean_resid)])] %>% 
        ggplot(aes(x=mean_resid, y = iso3c)) + geom_col()

    dfx %>% adt() %>% .[iso3c == "SRB", .(pred, nbr_opened)]

    
    ## see what happens if variable is excluded
    fx2 <- gen_r_f("rates", setdiff(iv_vars, "shweal992j_p90p100_lag4"))
    rx2 <- glmmTMB(fx2, dfx, family = nbinom2)
    resids2 <- simulateResiduals(rx2, n = 10000)
    dfx$resid2 <- residuals(resids2)

    adt(dfx) %>% .[, sum_opnd := sum(nbr_opened), iso3c] %>%
        .[sum_opnd >= 3] %>%
        melt(id.vars = c("iso3c"), measure.vars = c("resid", "resid2")) %>%
        .[, .(mean_resid = abs(mean(0.5 - value))), .(iso3c, variable)] %>% # distance to 0.5
        dcast.data.table(iso3c ~ variable, value.var = "mean_resid") %>%
        .[, diff := resid2 - resid] %>% # how much smaller (better) distance to 0.5 is in rx2 vs rx1
        .[order(diff)] %>% 
        adt(dfx)[, sum_opened := sum(nbr_opened), iso3c][sum_opened >= 3][ # merge with correlation data: 
          , .(corx = cor.test(nbr_opened, shweal992j_p90p100_lag4) %>% # cor data: visual match
                  chuck("estimate")), iso3c][., on = "iso3c"] %>%
        .[, maxmz := (-scale(diff))*scale(corx)] %>%
        .[order(-maxmz)]
        
        
        
    adt(dfx)[iso3c %in% c("RUS", "USA", "ITA")] %>%
        melt(id.vars = c("iso3c", "year"), measure.vars = c("nbr_opened", "shweal992j_p90p100_lag4")) %>%
        ggplot(aes(x=year, y=value, group = iso3c)) +
        facet_grid(variable ~ iso3c, scales = "free") +
        geom_line(se = F) +
        geom_smooth(se = F)


    ## contribution of intercepts
    exp(predict(rx, adt(dfx)[, iso3c := "lul"])) %>% sum()
    exp(predict(rx, adt(dfx))) %>% sum()
    ## idk: in some cases there's more, in some less:
    ## sum(abs(difference))

    
    
    

    

}


model_weights <- function() {

## fit with population weights
    rx3 <- glmmTMB(fx2, dfx, family = nbinom2, weights = dfx$SP_POP_TOTLm_lag0_uscld)
    rx32 <- glmmTMB(fx2, dfx, family = nbinom2, weights = dfx$SP_POP_TOTLm_lag0_uscld/10)
    rx4 <- glmmTMB(fx2, dfx, family = nbinom2, weights = asinh(dfx$SP_POP_TOTLm_lag0_uscld)) # scaled weights
    compare_models(rx2, rx3, rx32, rx4)
    ## oooof quite  different
    ## affects N -> could check how to fit properly

    
    ranef(rx)
    r2(rx)
}

added_squared_term_to_hnwi <- function() {
    ## add some squared term to model to improve fit
    ## LUL nobody cares
    
    rx2 <- update.formula(fx, ~ . + I(hnwi_nbr_1M_lag4^2)) %>%
        glmmTMB(dfx, family = nbinom2)
    dharma_res2 <- simulateResiduals(rx2, n = 1000)
    plotResiduals(dharma_res2, form = dfx$hnwi_nbr_1M_lag4, quantreg = T, rank = T) # no complains now, 
    dfx$resid2 <- residuals(dharma_res2)

    compare_models(rx, rx2) # no yuge difference, hwni still not significant

    dfx %>% adt() %>% .[, c(iv_vars, vvs$base_vars, "resid2"), with = F] %>%
        melt(id.vars = c(vvs$base_vars, "resid2")) %>%
        ggplot(aes(x=value, y = resid2)) +
        geom_point(size = 0.2) +
        geom_smooth() + 
        facet_wrap(~variable, scales = "free")

}
