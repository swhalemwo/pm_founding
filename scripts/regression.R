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
