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

screenreg(list(reg3, reg4, reg5)

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
