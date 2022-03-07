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


## ** prediction

predict(poi2)
predict_df <- as_tibble(expand.grid(gender=c(0,1), sensation=seq(0,8,0.1)))

predict_df$prediction <- predict.glm(poi_interact, newdata=predict_df, type="response")

predict_se <- predict.glm(poi_interact, newdata=predict_df, type="response", se.fit=T)
predict_df$prediction <- predict_se$fit
predict_df$se <- predict_se$se.fit
## options for type:
## - "response": seems to work
## - "link": just linear?
## - "terms": something weird, but seems like linear

predict_df$ci_hi <- predict_df$prediction + 1.96*predict_df$se
predict_df$ci_lo <- predict_df$prediction - 1.96*predict_df$se

ggplot(predict_df, aes(x=sensation, y=prediction, color=factor(gender))) +
    geom_line() +
    geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi, fill=factor(gender), alpha=0.5), show.legend=FALSE)

## makes sense that men start higher: exp(0) for gender evaluates to 1, so if men have something more there (0.83) they have more because there are is still non-zero baseline to multiplicate with 
exp(-0.78) ## women at sensation 0
exp(-0.78) * exp(0.83) ## men at sensation 0

## huh the interaction is strong
##
## (Intercept)       0.05182
## sensation         0.10318: slope of sensation when gender ==0
## gender           -0.30196: slope of gender when senation==0
## sensation:gender  0.21434

## men at 0 sensation:
filter(predict_df, gender==1 & sensation == 0)

exp(0.05181 + 0.103*0 + -0.3 + 0.21) ## nope 
exp(0.05181 + -0.30196) ## yup: only gender coef now matters because sensation is 0

## men at sensation 5
filter(predict_df, gender==1 & sensation == 5)
exp(0.05181 + 0.21434*5) ## nope
exp(0.05181 + 0.10318 * 5 + -0.30196 + 0.21434*5) ## brainlet
exp(0.05181 + (0.10318 + 0.21434)*5 + (-0.30196 + 0.21434)*1)
exp(0.05181 + (0.10318 + 0.21434)*5 + (-0.30196 + 0.21434)*1)

as.data.frame(filter(predict_df, gender==1 & sensation == 7.4))

exp(0.05181 + 0.10318 *7.4  + (-0.30196) + 0.21434*7.4) ## this makes no sense
## why is interaction coef added overall and not to each? 
exp(0.05181 + (0.103181 + 0.21434) * 7.4 + (-0.30196))
## now interaction coef only added to sensation coef
## hm that's cause gender coef is 1
## -> so I can read it as normal linear adding up coefs for prediction purposes


## women at sensation 5
filter(predict_df, gender==0 & sensation == 5)
exp(0.05181 + 0.103*5)



## *** AME
library(margins)
m <- margins(poi_interact, type="link")
summary(m)
plot(m) ## buggy 

## hope it also works for all the glmer.nb models...




## not using poisson

## still kinda unclear what values I should use for all the other variables that I'm not plotting
## maybe there's something like Average marginal effects? 



## ** pseudo R2


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
