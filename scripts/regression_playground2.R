## *** more optimization testing by grid.expanding optimization parameters

startx = 8
byx = 4
topx <- 17
tols <- expand.grid(tol = seq(startx,topx, by=byx),
            reltol = seq(startx,topx, by=byx),
            gradtol = seq(startx,topx, by=byx),
            steptol = seq(startx,topx, by=byx),
            lambdatol = seq(startx,topx, by=byx),
            qrtol = seq(startx,topx, by=byx)) %>% adt()

tols2 <- 1/10^tols


proc_tol_cfg <- function(tol_vec) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' the the grid.expand-ed tolerance configurations
    
    
    base_str_pglm <- 'pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
     index = "iso3c", data = cbn_dfs$cbn_all,  model = "random",  effect = "individual",
     family = negbin,
     tol = %s, reltol = %s, gradtol = %s, steptol = %s, lambdatol = %s, qrtol = %s)'

    str_procd <- sprintf(base_str_pglm, tol_vec$tol, tol_vec$reltol, tol_vec$gradtol, tol_vec$steptol,
                         tol_vec$lambdatol, tol_vec$qrtol)

    eval(parse(text = str_procd))
    
}

tols2.list <- setNames(split(tols2, seq(nrow(tols2))), rownames(tols2))

tol_res <- mclapply(tols2.list, proc_tol_cfg, mc.cores = 6)

tol_res[1:10]

tol_res_procd <- lapply(tol_res, \(x) as.list(coef(x))) %>% rbindlist()
names(tol_res_procd)[1] <- "intercept"

tol_res_procd[order(-intercept)]




## ** offset testing

nonmel <- read.table(header = TRUE,
                     text = "
   cases city u1 u2 u3 u4 u5 u6 u7      n
1      1    0  1  0  0  0  0  0  0 172675
2     16    0  0  1  0  0  0  0  0 123065
3     30    0  0  0  1  0  0  0  0  96216
4     71    0  0  0  0  1  0  0  0  92051
5    102    0  0  0  0  0  1  0  0  72159
6    130    0  0  0  0  0  0  1  0  54722
7    133    0  0  0  0  0  0  0  1  32185
8     40    0  0  0  0  0  0  0  0   8328
9      4    1  1  0  0  0  0  0  0 181343
10    38    1  0  1  0  0  0  0  0 146207
11   119    1  0  0  1  0  0  0  0 121374
12   221    1  0  0  0  1  0  0  0 111353
13   259    1  0  0  0  0  1  0  0  83004
14   310    1  0  0  0  0  0  1  0  55932
15   226    1  0  0  0  0  0  0  1  29007
16    65    1  0  0  0  0  0  0  0   7583
")

## Create age.range variable and city variable
nonmel <- within(nonmel, {
    age.range <- rep(c("15_24","25_34","35_44","45_54","55_64","65_74","75_84","85+"), 2)
    age.range <- factor(age.range)
    age.range <- relevel(age.range, ref = "85+")

    city <- factor(city, 0:1, c("Minneapolis", "Dallas"))
})

## rop unnecessary columns
nonmel <- nonmel[c("cases","n","city","age.range")]

## Check data
nonmel

model_offset <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)
model_nooffset <- glm(cases ~ city + age.range + n, family = poisson(link = "log"), data = nonmel)

screenreg(list(model_offset, model_nooffset))
library(lmtest)
library(sandwich)
coeftest(model_offset, vcov = sandwich)

r_nooffset <- glmmTMB(nbr_opened ~ hnwi_nbr_1M_lag1 + sptinc992j_p90p100_lag1 + shweal992j_p90p100_lag1 + tmitr_approx_linear20step_lag1 + smorc_dollar_fxm_lag1 + smorc_dollar_fxm_sqrd_lag1+ NY.GDP.PCAP.CDk_lag1 + SP.POP.TOTLm_lag1 + clctr_cnt_cpaer_lag1 + nbr_opened_cum_lag1+ nbr_opened_cum_sqrd_lag1 + Ind.tax.incentives+ NPO.tax.exemption + cnt_contemp_1995 + cnt_contemp_1995_squared + ti_tmitr_interact_lag1 + (1 | iso3c),
                      data=cbn_dfs$cbn_all,
                      family = nbinom2)

r_nooffset_log <- glmmTMB(nbr_opened ~ hnwi_nbr_1M_lag1 + sptinc992j_p90p100_lag1 + shweal992j_p90p100_lag1 + tmitr_approx_linear20step_lag1 + smorc_dollar_fxm_lag1 + smorc_dollar_fxm_sqrd_lag1+ NY.GDP.PCAP.CDk_lag1 + log(SP.POP.TOTLm_org) + clctr_cnt_cpaer_lag1 + nbr_opened_cum_lag1+ nbr_opened_cum_sqrd_lag1 + Ind.tax.incentives+ NPO.tax.exemption + cnt_contemp_1995 + cnt_contemp_1995_squared + ti_tmitr_interact_lag1 + (1 | iso3c),
                      data=cbn_all_offset,
                      family = nbinom2)

screenreg(list(r_nooffset))


## get the original population data to use as properly offset
cbn_all_offset <- select(df_reg, iso3c, year, SP.POP.TOTLm) %>%
    select(everything(), SP.POP.TOTLm_org = SP.POP.TOTLm) %>%
    inner_join(cbn_dfs$cbn_all, by = c("iso3c", "year"))




r_offset <- glmmTMB(nbr_opened ~ hnwi_nbr_1M_lag1 + sptinc992j_p90p100_lag1 + shweal992j_p90p100_lag1 + tmitr_approx_linear20step_lag1 + smorc_dollar_fxm_lag1 + smorc_dollar_fxm_sqrd_lag1+ NY.GDP.PCAP.CDk_lag1 + clctr_cnt_cpaer_lag1 + nbr_opened_cum_lag1+ nbr_opened_cum_sqrd_lag1 + Ind.tax.incentives+ NPO.tax.exemption + cnt_contemp_1995 + cnt_contemp_1995_squared + ti_tmitr_interact_lag1 + SP.POP.TOTLm_org + offset(log(SP.POP.TOTLm_org)) + (1|iso3c),
                    data=cbn_all_offset,
                    family = nbinom2)



screenreg(list(r_nooffset, r_nooffset_log, r_offset))
stargazer(r_offset)

library(sjPlot)

coef(summary(r_offset))


tdt <- data.table(id = c("a", "b"), pm = c(5,5), pop = c(50,500), hnwi = c(10,100))

tdt[, hnwi_pcap := hnwi/pop]

r_nfst <- glmmTMB(pm ~ pop + hnwi, tdt, family = nbinom2)
r_nfst2 <- glmmTMB(pm ~ hnwi + log(pop), tdt, family = nbinom2)
r_ofst <- glmmTMB(pm ~ hnwi + offset(log(pop)), tdt, family = nbinom2)

glmmTMB(pm ~ hnwi_pcap + log(pop), tdt, family = nbinom2)

screenreg(list(r_nfst, r_ofst))

## look at real data: PM europe

dtt <- filter(df_reg, countrycode(iso3c, "iso3c", "un.region.name") == "Europe", year == 2012) %>%
    select(iso3c, nbr_opened, hnwi_nbr_30M, SP.POP.TOTLm, gptinc992j) %>% na.omit() %>%
    mutate(nbr_opened_pcapm = nbr_opened/SP.POP.TOTLm,
           hnwi_nbr_30M_pcapm = hnwi_nbr_30M/SP.POP.TOTLm) %>% adt()


r_nofst <- glm.nb(nbr_opened ~ hnwi_nbr_30M + SP.POP.TOTLm + gptinc992j, dtt)
r_nofst2 <- glm.nb(nbr_opened ~ hnwi_nbr_30M + log(SP.POP.TOTLm) + gptinc992j, dtt)
r_ofst <- glm.nb(nbr_opened ~ hnwi_nbr_30M + offset(log(SP.POP.TOTLm)) + gptinc992j, dtt)

## using other rates 
r_nofst3 <- glm.nb(nbr_opened ~ hnwi_nbr_30M + log(SP.POP.TOTLm) + gptinc992j, dtt)

mdl_names <- c("r_nofst", "r_nofst2", "r_ofst")
mdl_list <- lapply(mdl_names, get)
     
screenreg(mdl_list, digits = 5, custom.model.names = mdl_names)


