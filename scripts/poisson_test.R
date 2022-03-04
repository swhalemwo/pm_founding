library(tidyverse)
library(screenreg)

df <- as_tibble(
    read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/poisson_test_data/Coxe et al. Poisson Dataset in Text Format.txt", header=FALSE, sep= " "))
names(df) <- c("id", "sensation", "gender", "drinks")

ols <- lm(drinks ~ sensation, df)
poi1 <- glm(drinks ~ sensation, df, family = "poisson")
poi2 <- glm(drinks ~ sensation + gender, df, family = "poisson")

screenreg(list(ols, poi1, poi2, ))

