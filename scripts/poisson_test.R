library(tidyverse)
library(texreg)
library(MASS)

df <- as_tibble(
    read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/poisson_test_data/Coxe et al. Poisson Dataset in Text Format.txt", header=FALSE, sep= " "))
names(df) <- c("id", "sensation", "gender", "drinks")

df$drinks_mod <- df$drinks + 0.01

ols <- lm(drinks ~ sensation, df)
poi1 <- glm(drinks ~ sensation, df, family = "poisson")
poi2 <- glm(drinks ~ sensation + gender, df, family = "poisson")
poi_interact <- glm(drinks ~ sensation*gender, df, family = "poisson")
nb1 <- glm.nb(drinks ~ sensation, df)


screenreg(list(ols, poi1, poi2, nb1, poi_interact))

resids <- residuals(poi2, "response")
resids <- residuals(poi2, "deviance")

plot(poi2$fitted.values, resids)





pseudo.r2 <- function(glm) {
    #' pseudo r-square following Coxe_West_Aiken_2009_count
    return(1-(glm$deviance/glm$null.deviance))
}

## ** overdispersion test

poisson.phi <- function(glm) {
    #' calculate overdispersion parameter phi
    dp = sum(residuals(glm,type ="pearson")^2)/glm$df.residual
    return(dp)
}
library(AER)
overdisp <- dispersiontest(poi2)
poisson.phi(poi2)
## hmm i get different results, but poisson.phi produces also what Coxe_etal report (2.30 for poi2, 2.84 for poi1)
## dispersiontest is very similar tho: 2.27 for poi2, 2.83 for poi1

## https://towardsdatascience.com/adjust-for-overdispersion-in-poisson-regression-4b1f52baa2f1
## calling summary doesn't work somehow
## i fucking hate these overloaded methods
summary(poi2, dispersion = overdisp)

## seems like family=quasipoisson implements overdispersed poisson
qpoi2 <- glm(drinks ~ sensation + gender, df, family = "quasipoisson")
qpoi1 <- glm(drinks ~ sensation,df, family = "quasipoisson")
screenreg(list(poi2, qpoi2))

summary(qpoi1)

## this is literally just larger SEs...



## ** descriptives 

aggregate(drinks ~ gender, df, mean)
plot(df$sensation, df$drinks)

ggplot(df, aes(x=sensation, y=drinks, color=factor(gender))) +
    geom_jitter(width=0.5, height=0.5)



## **  test how R deals with non-integer things
poi_mod <- glm(drinks_mod ~ sensation + gender, df, family = "poisson")
## coefs still get estimated, fit stats (AIC, BIC, LL) are gonesies
