## ** new test

df_reg1 <- na.omit(df_reg[,c("iso3c", "year", "nbr_opened", "PC1_caf", "cnt_contemp_1985", "pct_cutoff_10M", "gptinc992j")])

df_reg1 <- filter(df_reg1, pct_cutoff_10M < 1)



chart.Correlation(df_reg1[,c("nbr_opened", "PC1_caf", "cnt_contemp_1985", "pct_cutoff_10M", "gptinc992j")])

ols_lm <-      lm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, df_reg1)
ols_glm <-   lmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1)
poi_glm <-    glm(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j, df_reg1, family = poisson)
poi_glmm <- glmer(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1, family = poisson)
nb <-    glmer.nb(nbr_opened ~ PC1_caf + cnt_contemp_1985 + pct_cutoff_10M + gptinc992j + (1|iso3c), df_reg1)



screenreg(list(ols_lm, ols_glm, poi_glm, poi_glmm, nb),
          custom.coef.map = list("(Intercept)" = "(Intercept)", "PC1_caf" = "PC1_caf",
                                 "cnt_contemp_1985" = "cnt_contemp_1985",
                                 "pct_cutoff_10M" = "pct_cutoff_10M", "gptinc992j" = "gptinc992j"))




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


screenreg(list(pglm_within, pglm_pooling, pglm_random, pglm_between))

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

