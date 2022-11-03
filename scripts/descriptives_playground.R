## * pooled descriptives based on df_reg



## df_reg$smorc_dollar_fx <- df_reg$smorc_dollar_fx/1e6

rel_vars <- c("nbr_opened" = "Number of Private Museums opened",
              "sum_core" = "Tax incentives",
              "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax Incentives",
              "tmitr_approx_linear20step" = "Marginal Income Tax Rate (%)",
              "hnwi_nbr_1M" = "# HNWIs with net worth >= 1M",
              "hnwi_nbr_5M" = "# HNWIs with net worth >= 5M",
              "hnwi_nbr_30M" = "# HNWIs with net worth >= 30M",
              "hnwi_nbr_200M" = "# HNWIs with net worth >= 200M",
              "hnwi_nbr_1B" = "# HNWIs with net worth >= 1B",
              "sptinc992j_p90p100" = "Income share of top 10%",
              "sptinc992j_p99p100" = "Income share of top 1%",
              "gptinc992j" = "Gini of pre-tax income",
              "ghweal992j"= "Gini of net wealth",
              "shweal992j_p90p100" = "Wealth share of top 10%",
              "shweal992j_p99p100" = "Wealth share of top 1%",
              "smorc_dollar_fxm" = "Gvt cultural spending (millions)",
              "NY.GDP.PCAP.CDk" = "GDP per capita (thousands)",
              "SP.POP.TOTLm" = "Population (millions)",
              "cnt_contemp_1985" = "# Museums of contemporary art in 1985",
              "clctr_cnt_cpaer" = "# Collectors in Artnews collector list",
              "ln_s" = "ln(s)",
              "cons" = "cons",
              "ln_r" = "ln(r)",
              "cnt_contemp_1995" = "# of modern/contemp. art museums in 1995",
              "nbr_opened_cum" = "cumulative openings (legitimacy)",
              "nbr_opened_cum_sqrd" = "cumulative openings squared (competition)"
              )





get_more_vrbl_info <- function(x) {

    df_reg %>% select(iso3c, year, x) %>% na.omit() %>%
        mutate(vrbl = x) %>% 
        summarize(nbr_crys = n_distinct(iso3c),
                  min_time = min(year), max_time = max(year),
                  time_range = paste0(min_time, "-", max_time),
                  vrbl = unique(vrbl)) %>%
        select(vrbl, nbr_crys, time_range) %>% as.list()
}
## get_more_vrbl_info("smorc_dollar_fx")

more_vrbl_info <- rbindlist(lapply(
    intersect(names(vvs$vrbl_lbls), names(df_reg)),
    get_more_vrbl_info))


 
var_table <- describe(df_reg[,names(rel_vars)]) %>% select("Country-years" = n, mean, sd, median, min, max)
rownames(var_table) <- recode(rownames(var_table), !!!rel_vars)
var_table$`# Countries` <- more_vrbl_info$nbr_crys

var_xtbl <- xtable(var_table, caption = "main variables (all monetary variables are in or based on 2021 constant US dollars)", label = "var_desc", digits = c(0, 0, rep(2,6)))

print(var_xtbl, file = paste0(TABLE_DIR, "var_desc.tex"), include.rownames = T, hline.after =c(-1,0,7,11))

## ** some SO inspired testing of rates vs counts

## https://stats.stackexchange.com/questions/522275/controlling-for-population-size-using-per-capita-or-including-a-variable-for-po

test_df <- data.table(name = c("nebraska", "kansas", "idaho"), mort = c(5, 1, 3), sales = c(10,10, 5), pop = c(10, 1, 5)) %>%
    .[, `:=`(mort_pcap = mort/pop,
             sales_pcap = sales/pop)]


lm(mort ~ sales + pop, test_df[1:2])
lm(mort_pcap ~ sales_pcap, test_df[1:2])
lm(mort ~ sales_pcap, test_df[1:2])


ggplot(test_df, aes(x=sales_pcap, y=mort_pcap)) + geom_point()
ggplot(test_df, aes(x=sales, y=mort, size = pop)) + geom_point()

set.seed(5)
# create the variance covariance matrix
sigma<-rbind(c(1,-0.8,-0.7), c(-0.8,1, 0.4), c(-0.7,0.4,1))
# create the mean vector
mu<-c(10, 5, 2) 
# generate the multivariate normal distribution
dfx <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma)) %>% adt()
names(dfx) <- c("whatever", "dvx", "ivx")
## generate population 
dfx$pop <- rnorm(n=1000, mean = 50, sd = 10)

cor(dfx$ivx, dfx$dvx)
cor(dfx$pop, dfx$dvx)
cor(dfx$pop, dfx$ivx)

dfx[, `:=`(iv_cnts = ivx * pop, dv_cnts = dvx * pop)]

cor(dfx$iv_cnts, dfx$dv_cnts)

r_rts <- lm(dvx ~ ivx, dfx)
r_cnt <- lm(dv_cnts ~ iv_cnts + pop, dfx)

r_rts_scld <- lm(scale(dvx) ~ scale(ivx), dfx)
r_cnt_scld <- lm(scale(dv_cnts) ~ scale(iv_cnts) + scale(pop), dfx)

r_rts_pop <- lm(dvx ~ ivx + pop, dfx)

r_rts_scld_pop <- lm(scale(dvx) ~ scale(ivx) + scale(pop), dfx)

screenreg(list(r_rts, r_cnt, r_rts_scld, r_cnt_scld, r_rts_scld_pop, r_rts_pop), digits = 4)

predict(r_rts)

## ** try some poisson data
library(texreg)
library(glmmTMB)
library(ggplot2)
cor(rpois(1000, lambda = 1), rpois(1000, lambda = 1))
library(RNGforGPD)

cor_mat <- rbind(c(1,0.7, 0.3),c(0.7,1, 0), c(0.3, 0, 1))
x <- GenMVGpois(sample.size = 1000, theta.vec = c(1,1,1), no.gpois = 3, lambda.vec = c(0.5, 0.5, 0.5), cmat.star = cor_mat)
poi_dt <- adt(x$data)
setnames(poi_dt, c("dv_rt", "iv1_rt", "iv2_rt"))
cor(poi_dt$dv_rt, poi_dt$iv1_rt)

ggplot(poi_dt, aes(x=iv1_rt, y=dv_rt)) +
    geom_jitter()

poi_dt2 <- poi_dt %>% copy() %>%
    .[, pop := sample(1:10, .N, replace = T)] %>%
    .[, `:=`(dv_cnt = dv_rt*pop, iv1_cnt = iv1_rt*pop, iv2_cnt = iv2_rt*pop)]
    
poi_dt2_scld <- poi_dt2[, c("dv_cnt", lapply(.SD, scale_wo_attr)),
                        .SDcols = c("iv1_cnt", "iv2_cnt", "pop", "iv1_rt", "iv2_rt")]
poi_dt2_scld$dv_cnt <- poi_dt2$dv_cnt
poi_dt2_scld$pop_uscld <- poi_dt2$pop


r_cnts <- glm(dv_cnt ~ iv1_cnt + iv2_cnt + pop, poi_dt2, family = poisson)
r_rts <- glm(dv_cnt ~ iv1_rt + iv2_rt + offset(pop), poi_dt2, family = poisson)


r_cnts_scld <- glm(dv_cnt ~ iv1_cnt + iv2_cnt + pop, adf(poi_dt2_scld), family = poisson)
r_rts_scld <- glm(dv_cnt ~ iv1_rt + iv2_rt + offset(pop_uscld), poi_dt2_scld, family = poisson)

mdl_names <- c("r_cnts", "r_rts", "r_cnts_scld", "r_rts_scld")
mdl_list <- lapply(mdl_names, get)

screenreg(mdl_list, custom.model.names = mdl_names)



chart.Correlation(poi_dt2)

library(texreg)
screenreg(r_rts)
summary(r_cnts)
summary(r_rts)

## ** see what happens with rates
poi_dt3 <- poi_dt2

## poi_dt4 <- 

poi_dt4 <- poi_dt3 %>% copy() %>% .[, iv1_rt_sml := iv1_rt/5] %>% # add small rate
    .[, `:=`(iv1_rt_sml_sqrd = iv1_rt_sml^2, iv1_rt_sqrd = iv1_rt^2)] # add small and normal rate squared
    

poi_dt4_scld <- poi_dt4[, lapply(.SD, scale_wo_attr),
                        .SDcols = c("iv1_rt", "iv1_rt_sqrd", "iv1_rt_sml", "iv1_rt_sml_sqrd")] %>%
    cbind(poi_dt4[, .(pop, dv_cnt)])



r_rt_nol <- glm(dv_cnt ~ iv1_rt + iv1_rt_sqrd + offset(pop), poi_dt4, family = poisson) # normal rate
r_rt_sml <- glm(dv_cnt ~ iv1_rt_sml + iv1_rt_sml_sqrd + offset(pop), poi_dt4, family = poisson) # small rate

r_rt_nol_scld <- glm(dv_cnt ~ iv1_rt + iv1_rt_sqrd + offset(pop), poi_dt4_scld, family = poisson) # normal rate
r_rt_sml_scld <- glm(dv_cnt ~ iv1_rt_sml + iv1_rt_sml_sqrd + offset(pop), poi_dt4_scld, family = poisson) # small rate

mdl_names <- c("r_rt_normal", "r_rt_sml")
lapply(mdl_names, get)

mdl_list <- c(get("r_rt_nol"), get("r_rt_sml"))


screenreg(list(r_rt_nol, r_rt_sml, r_rt_nol_scld, r_rt_sml_scld))





## ** debugging lack of private museum openings even in cluster 4 (developed countries)
## kinda fixed by first summing by cluster, then dividing by population
df_reg_clstrd %>% filter(cluster == 4) %>% select(iso3c, year, nbr_opened, nbr_opened_pcap) %>%
    ## pull(nbr_opened) %>% table()
    summarize(mean_nbr_opened = mean(nbr_opened),
              mean_nbr_opened_pcap = mean(nbr_opened_pcap),
              median_nbr_opened = median(nbr_opened),
              median_nbr_opened_pcap = median(nbr_opened_pcap))

## see which countries have many contemporary art collectors per capita

## arrange(df_reg_clstrd, -clctr_cnt_cpaer_pcap) %>% select(iso3c, year, clctr_cnt_cpaer_pcap, cluster) %>%
##     filter(cluster != 4) %>% head(30) %>% distinct(iso3c)

## countrycode(c("BRB", "BHS", "CRI"), "iso3c", "country.name")
