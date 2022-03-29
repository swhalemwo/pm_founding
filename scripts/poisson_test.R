library(tidyverse)
library(texreg)
library(MASS)
library(lme4)

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
## seems to be the case 



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



## ** test how R deals with non-integer things
poi_mod <- glm(drinks_mod ~ sensation + gender, df, family = "poisson")
## coefs still get estimated, fit stats (AIC, BIC, LL) are gonesies

## ** testing FE

poi_fe_df <- as_tibble(cbind(c(1,1,1,2,2,2,3,3,3),
                             c(1,2,3,4,5,6,7,8,9)))
names(poi_fe_df) <- c("id", "y")
## poi_fe_df$x_within_across <- poi_fe_df$y + rnorm(poi_fe_df$y, sd = 0.5)
poi_fe_df$x_within_across <- poi_fe_df$y
poi_fe_df$x_within <- rep(c(1,2,3), 3)
poi_fe_df$x_across <- sort(poi_fe_df$x_within)
poi_fe_df$size <- poi_fe_df$id*1000
poi_fe_df$x_within2 <- c(-2, -1, 0, 8,9,10,4,5,6)


ols_base <- lm(y ~ x_within_across, poi_fe_df)
poi_base <- glm(y ~ x_within_across, poi_fe_df, family = "poisson")
summary(poi_base)
screenreg(list(ols_base, poi_base))
pseudo.r2(poi_base)

poi_fe_within_across <- glmer(y ~ x_within_across + (1 | id), poi_fe_df, family = "poisson")
poi_fe_within <- glmer(y ~ x_within + (1 | id), poi_fe_df, family = "poisson")
poi_fe_across <- glmer(y ~ x_across + (1 | id), poi_fe_df, family = "poisson")

screenreg(list(poi_base, poi_fe_within_across, poi_fe_within, poi_fe_across))
## kinda worrying that poi_fe_within doesn't find the within relationship
## maybe it's only demeaning the predictors? 

## testing more ways of getting fe to work

poi_fe_within2 <- glmer(y ~ x_within2 + (1 | id), poi_fe_df, family = "poisson")
poi_fe_within3 <- glmer(x_within ~ x_within2 + (1 | id), poi_fe_df, family = "poisson")
poi_fe_within4 <- glm(x_within ~ x_within2 + factor(id), poi_fe_df, family = "poisson")

poi_fe_df <- poi_fe_df %>%
    group_by(id) %>%
    mutate(x_within3 = x_within2 - mean(x_within2))
poi_fe_within5 <- glm(x_within ~ x_within3, poi_fe_df, family = "poisson")

poi_fe_df$x_within4 <- poi_fe_df$x_within3 + 2
poi_fe_within6 <- glm(x_within ~ x_within4, poi_fe_df, family = "poisson")

screenreg(list(poi_fe_within, poi_fe_within2, poi_fe_within3, poi_fe_within4, poi_fe_within5, poi_fe_within6))
## hmm not good, x_within 2 should predit x_within after being demeaned
## still not good: controlling for factor(id) should make x_within2 significant

## hmm even predicting itself is not significant?? at least with4/5/6 always give the same result of 0.52 for demeaning, factor and identical
## -> challenge is now to get the same from glmer

## screenreg(glm(y ~ x_within_across, poi_fe_df, family = "poisson"))
## screenreg(glm(x_within ~ x_within4, poi_fe_df, family = "poisson"))




poi_fe_within21 <- glmer(y ~ x_within3 + (1 | id), poi_fe_df, family = "poisson")
poi_fe_within22 <- glmer(y ~ x_within4 + (1 | id), poi_fe_df, family = "poisson")


screenreg(list(poi_fe_within2, poi_fe_within21, poi_fe_within22))

## ** tutorial (Atkins_etal_2013_count)

## first dataset
## can't find second one for now, whatever
library(saemix)

library(dplyr)

df_rapi <- as_tibble(rapi.saemix)

df_rapi$gender <- dplyr::recode(as.character(df_rapi$gender),
                         "Men"=1,
                         "Women"=0)

## actually working, not sure why tho -> still need to understand the different specifications more -> don't delete the wrong ones yet

library(parallel)

poi_specs <- list(
    "right (time-re)" = rapi ~ time + gender + (time | id),
    "expec (fe)" =      rapi ~ time + gender + (1 | id),
    "zero gender-re"=   rapi ~ time + gender + (0 + gender |id),
    "gender-re" =       rapi ~ time + gender + (gender | id)
    ## "multi-fe" =        rapi ~ time + gender + (1 | gender)  + (1 | id),
    ## "gender-fe" =       rapi ~ time  + (1 | gender)  + (1 | id),
    )
poi_res <- mclapply(poi_specs, function(x) glmer(x, df_rapi, family = "poisson"), mc.cores = 8)
screenreg(poi_res)

## ** model comparison
## *** right (time-re)
coef(poi_res$"right (time-re)")$id[1:5,]
## different time slopes, different intercepts
mean(coef(poi_res$"right (time-re)")$id$time)
mean(coef(poi_res$"right (time-re)")$id$"(Intercept)")
## mean of slopes is kinda close to fixef value, but not completely
## same for mean of Intercepts and fixef Intercept
fixef(poi_res$"right (time-re)")

## *** expec (fe)
coef(poi_res$"expec (fe)")$id[1:5,]
coef(poi_res$"expec (fe)")$id$time
## different intercepts, all same slope 
mean(coef(poi_res$"expec (fe)")$id$"(Intercept)")
## mean of intercepts is (again) close to fixef value 
fixef(poi_res$"expec (fe)")

## *** multi-fe
coef(poi_res$"multi-fe")$id[1:5,]
coef(poi_res$"multi-fe")$id$time
## same time slope
mean(coef(poi_res$"multi-fe")$id$"(Intercept)")
fixef(poi_res$"multi-fe")
## mean of individual intercepts again close to fixef mean 

## ok this seems to be kinda pointless: doing FE for gender means I think there might be clustering on gender
## but gender is dummy, and i control for it in regression anyways -> kinda like FE dummy 
## -> should be identical to "gender-fe" =       rapi ~ time  + (1 | gender)  + (1 | id), isn't tho for some reason??, but (1|gender) seems so pointless that I think I can drop it

## *** others
## actually have idea what (varx | id) means 
## means that the effect of varx differs between id-s
## if (gender |id): for some people the effect of gender (being a man) is stronger than for others?
coef(poi_res$"zero gender-re")$id[1:5,]
hist(coef(poi_res$"zero gender-re")$id$gender, breaks = 100)
## i think (gender | id) is kinda pointless





## ** prediction 


## predict df with individual ids, then matching gender to it (since id is needed if not using re.form=NA), probably not gonna use this much
predict_df_re <- as_tibble(expand.grid(time=seq(0,25,1), id=seq(50)))
predict_df_re <- as_tibble(merge(predict_df_re, unique(df_rapi[,c("id", "gender")])))
predict_df_re$fitted <- predict(poi_res$"right (time-re)", newdata=predict_df, type = "response")


## just the fixed effects
predict_df <- as_tibble(expand.grid(time=seq(0,25,1), gender=c(0,1)))

## just one model 
predict_df$fitted <- predict(poi_res$"right (time-re)", newdata=predict_df, type = "response", re.form = NA)
predict_df$label <- round(predict_df$fitted,2)
predict_df$label[which(predict_df$time %!in% c(0,6,12,18,24))] <- NA

ggplot(predict_df, aes(x=time, y=fitted, color=factor(gender))) +
    geom_line() +
    geom_label_repel(aes(label = label))


## fitting to all models 
fit_stuff <- function(mod, predict_df, label) {
    #' don't use the random effects 
    predict_df$fitted <- predict(mod, newdata = predict_df, type = "response", re.form = NA)
    predict_df$labelx <- label
    return(predict_df)
}
##    
pred_res_all <- lapply(seq_along(poi_res), function(x)
    fit_stuff(mod=poi_res[[x]], predict_df = predict_df, label=names(poi_res)[[x]]))
    ## have to makme sure to pass arguments properly 
## 
pred_res_df <- as_tibble(Reduce(function(x,y,..) rbind(x,y), pred_res_all))
pred_res_df$gender <- factor(pred_res_df$gender)
##
ggplot(pred_res_df, aes(x=time, y=fitted, color=gender)) +
    facet_wrap(~labelx) + 
    geom_line(size=1)

## zero-gender re: has same for men, as other models, but higher for women??
## right (re?) has much steeper slope
## gender-re has same as other fe models huh



## *** bbolker: for CIs: not needed yet 
library(lme4)
library(ggplot2)
data("Orthodont",package="MEMSS")
fm1 <- lmer(
    formula = distance ~ age*Sex + (age|Subject)
    , data = Orthodont
)
newdat <- expand.grid(
    age=c(8,10,12,14)
    , Sex=c("Female","Male")
    , distance = 0
)
newdat$distance <- predict(fm1,newdat,re.form=NA)
mm <- model.matrix(terms(fm1),newdat)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
tvar1 <- pvar1+VarCorr(fm1)$Subject[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
newdat <- data.frame(
    newdat
    , plo = newdat$distance-cmult*sqrt(pvar1)
    , phi = newdat$distance+cmult*sqrt(pvar1)
    , tlo = newdat$distance-cmult*sqrt(tvar1)
    , thi = newdat$distance+cmult*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat, aes(x=age, y=distance, colour=Sex))+geom_point()
g0 + geom_pointrange(aes(ymin = plo, ymax = phi))+
    labs(title="CI based on fixed-effects uncertainty ONLY")



## ** bootMer: for CIs, but also skip for now 
if (interactive()) {
fm01ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE)
screenreg(fm01ML)

## see ?"profile-methods"
mySumm <- function(.) { s <- sigma(.)
    c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
(t0 <- mySumm(fm01ML)) # just three parameters
## alternatively:
mySumm2 <- function(.) {
    c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}

sigma(fm01ML)


set.seed(101)
## 3.8s (on a 5600 MIPS 64bit fast(year 2009) desktop "AMD Phenom(tm) II X4 925"):
boo01 <- bootMer(fm01ML, mySumm, nsim = 100) 

## ** overdispersion glmer
