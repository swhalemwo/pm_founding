## * regression playground for essential stuff, so that buffer can be C-c C-b-ed


## ** running old

cluster_sumry_addgns <- c(
    "hnwi_nbr_30M_pure" = "HNWI with net worth 30M USD (count)",
    "nbr_opened_pure" = "private museums openings (count)",
    "NY.GDP.PCAP.CD" = "country-average GDP per capita (2021 constant USD)",
    "NY.GDP.TTL" = "individual-average GDP per capita (2021 constant USD)",
    "SP.POP.TOTL_pure" = "population")

    

#' overall regression wrapping 

## *** end of functions


reg_settings <- list(
    nbr_specs = 1,
    batch_nbr = "v20",
    vary_vrbl_lag = T,
    cbns_to_include = names(cbn_dfs),
    mdls_to_include = c("full")
)

fldr_info <- setup_regression_folders_and_files(reg_settings$batch_nbr)

## generating 20k models costs around 5 secs


reg_spec_mdls <- gen_batch_reg_specs(reg_settings, vvs, vrbl_thld_choices)
names(reg_spec_mdls) <- lapply(reg_spec_mdls, \(x) x$mdl_id)

## check how unique the model cfgs are 
table(table(names(reg_spec_mdls)))

## run_vrbl_mdl_vars(reg_spec_mdls[[2]])
## gen_mdl_id(reg_spec_mdls[[2]])

cvrgns <- mclapply(reg_spec_mdls[1:12], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info, c("log_likelihood")), mc.cores = 6) %>% unlist()

lapply(reg_spec_mdls[1:30], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info, c("converged"))) %>% unlist()

## run_vrbl_mdl_vars(reg_spec_mdls[[1]], vvs, fldr_info)
NULL






## **** convergence testing

vrbl_thld_choices_cvrg <- slice_sample(vrbl_thld_choices, n=1)


reg_settings_cvrg <- list(
    nbr_specs_per_thld = 2,
    batch_nbr = "v40",
    vary_vrbl_lag = F,
    cbns_to_include = "cbn_all",
    mdls_to_include = c("full"),
    technique_strs = c("nr", "dfp", "bfgs", "nr 5 dfp 5 bfgs 5"),
    difficulty_switches = c(T,F)
)
    
reg_spec_mdls_cvrg <- gen_batch_reg_specs(reg_settings_cvrg, vvs, vrbl_thld_choices_cvrg)

fldr_info_cvrg <- setup_regression_folders_and_files(reg_settings_cvrg$batch_nbr)

              
mclapply(reg_spec_mdls_cvrg, \(x) optmz_reg_spec(x, fldr_info_cvrg, reg_settings_cvrg),
         mc.cores = 6)

optmz_reg_spec(reg_spec_mdls_cvrg[[1]], fldr_info = fldr_info_cvrg, reg_settings = reg_settings_cvrg)

## ** scrap: debugging, re-running

## mdl_ids_tbl <- tibble(mdl_id = unlist(mdl_ids)) 
## mdl_ids_tbl$x <- 1

## mdls_to_check_ids <- merge(mdl_ids_tbl,
##       df_reg_anls_cfgs_wide %>% select(mdl_id) %>% mutate(y=2),
##       all.x = T) %>% atb() %>%
##     filter(is.na(y)) %>% pull(mdl_id)

## mdls_to_check_locs <- which(mdl_ids %in% mdls_to_check_ids)

## ## reg_spec_mdls[mdls_to_check_specs]
## mclapply(reg_spec_mdls[mdls_to_check_locs], run_vrbl_mdl_vars, mc.cores = 6) 





## ** standardization of cbn_dfs

cbn_dfs$cbn_all

adt(cbn_dfs$cbn_all)[, lapply(.SD, sd)] %>% melt() %>% adf()
adt(cbn_dfs$cbn_no_cult_spending)[, lapply(.SD, sd)] %>% melt() %>% adf()
adt(cbn_dfs$cbn_no_cult_spending_and_mitr)[, lapply(.SD, sd)] %>% melt() %>% adf()


## ** debug time

dirx <- "/home/johannes/reg_res/v49/reg_res/"

v49_files <- paste0(dirx, list.files(dirx))
v49_files[[1]]

v49_file_infos <- lapply(v49_files, file.info)

v49_time_infos <- lapply(v49_file_infos, \(x) format(as.POSIXct(x$ctime), format = "%Y-%m-%d %H:%M"))
v49_time_infos[[1000]]
as.POSIXct(v49_time_infos[[1000]])

format(v49_time_infos[[1000]], format = "%Y-%m-%d %H:%M")

v49_time_infos[[1000]]

t_dtx <- data.table(x=1, timex = unlist(v49_time_infos)) %>%
    .[, timex2 := as.POSIXct(timex)]

t_dtx %>% copy() %>% .[, ten_min := ceiling_date(timex2, '10 min')] %>%
    .[, .N, ten_min] %>%
    ggplot(aes(x=ten_min, y=N)) +
    geom_line()


ggplot(t_dtx, aes(x=timex2)) +
    geom_histogram(bins = 180)
    


## ** debug error of rates not being properly lagged
cbn_df_dict$rates$cbn_all %>%
    viz_lines(y="clctr_cnt_cpaer_lag0")


cbn_dfs_rates$cbn_all %>%
    mutate(nbr_opened_prop = nbr_opened/SP_POP_TOTLm_lag0_uscld) %>%
    viz_lines(y="nbr_opened_prop", duration = 0)

cbn_dfs_rates$cbn_all$nbr_opened



viz_lines(df_reg_rts, y="clctr_cnt_cpaer")
viz_lines(cbn_dfs_rates$cbn_controls, y="hnwi_nbr_5M_lag0")
viz_lines(cbn_dfs_counts$cbn_all, y="hnwi_nbr_5M_lag0")

filter(cbn_dfs_counts$cbn_all, iso3c == "CHE")$hnwi_nbr_5M_lag0
filter(cbn_dfs_rates$cbn_all, iso3c == "CHE")$hnwi_nbr_5M_lag0


## , facets = "region")


## ** ti tmitr interaction

sd(cbn_dfs_counts$cbn_all$ti_tmitr_interact_lag0)
sd(cbn_dfs_counts$cbn_all$Ind.tax.incentives)
sd(cbn_dfs_counts$cbn_all$tmitr_approx_linear20step_lag3)

## ** checking scaling, looks good

par(mfrow = c(2,1))
hist(df_reg_rts$nbr_closed_cum_global)
hist(cbn_dfs_rates$cbn_controls$nbr_closed_cum_global_lag0)

library(magrittr)


right_join(
    select(df_reg_rts, iso3c, year, pm_density_global),
    select(cbn_dfs_rates$cbn_controls, iso3c, year, pm_density_global_lag0)) %$%
    cor(pm_density_global, pm_density_global_lag0)


    ggplot(aes(x=nbr_closed_cum_global, y=nbr_closed_cum_global_lag0)) +
    geom_jitter(width = 3, height = 0.5)

## ** within/between simulation

## first generate one country

gen_x_cons <- function(dtx, vrbls) {
    #' hold x constant at first yhear within group
    ## copy(dtx)[year != 0, x := NA][, x := nafill(x, type = "locf"), id]
    copy(dtx)[year != 0, (vrbls) := NA] %>%
        .[, (vrbls) := map(vrbls, ~nafill(get(.x), type = "locf")), id]
}

gen_wb_vrbls <- function(dtx, vrbls) {
    #' generate within/between variables for vrblx in dtx
    copy(dtx)[, paste0(vrbls, "_between") := map(.SD, mean), id, .SDcols = vrbls] %>%
        .[, paste0(vrbls, "_within") := map(.SD, ~.x - mean(.x)), id, .SDcols = vrbls]
}

gen_re_res <- function(dtx) {
    vrbls <- setdiff(names(dtx), c("id", "year", "y"))

    fx <- sprintf("y ~ %s + (1 | id)",
                  paste0(vrbls, collapse = " + ")) %>%
        as.formula()

    r_cbn <- glmmTMB(fx, dtx)
    print(summary(r_cbn))

    ## dt_wb$pred <- predict(r_cbn, dt_wb)

    ## plot predicted 
    ## ggplot(dt_wb, aes(x=year, y=pred, color = id)) + geom_line()

    ## hold x constant at 0
    dt_wb_cons <- gen_x_cons(dtx, "x")
    ## ggplot(dt_wb_cons, aes(x=x, y=y, color = id)) + geom_line()

    dt_wb_cons$pred <- predict(r_cbn, dt_wb_cons)

    return(dt_wb_cons)

}


gen_wb_res <- function(dtx) {
    #' generate within/between predictions under constant x
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    vrbls <- setdiff(names(dtx), c("id", "year", "y"))
    vrbls_wb <- CJ(vrbls, c("_between", "_within"))[, paste0(vrbls,V2)]
## vrbls <- c("x")

    dtx_wb_dcps <- copy(dtx) %>% gen_wb_vrbls(vrbls)

    fx <- sprintf("y ~ %s + (1 | id)",
                  paste0(vrbls_wb, collapse = " + ")) %>%
        as.formula()

    r_dcps <- glmmTMB(fx, dtx_wb_dcps)
    print(summary(r_dcps))
    ## constant x: generate new data
    dtx_wb_dcps_cons <- copy(dtx_wb_dcps) %>% gen_x_cons(vrbls = c("x_within", "x_between"))

    dtx_wb_dcps$pred <- predict(r_dcps, dtx_wb_dcps_cons)

    return(dtx_wb_dcps)
}


dt_wba <- data.table(year = 0:20, id = "a", x = jitter(0:20), y = jitter((0:20))*0.5)

## positive within, positive between 
dt_wbb <- copy(dt_wba)[, `:=`(id = "b", x = (x*0.5) + 2, y = (y*0.3) + 4)]

## create overall 
dt_wb <- rbind(dt_wba, dt_wbb)


## descriptive plots
ggplot(dt_wb, aes(x=year, y=x, color = id)) + geom_line()
## ggplot(dt_wb, aes(x=year, y=y, color = id)) + geom_line()
## ggplot(dt_wb, aes(x=x, y=y, color = id)) + geom_line()

gen_re_res(dt_wb) %>% ggplot(aes(x=year, y=pred, color = id)) + geom_line()
    
## plot predictions -> flat line
## ggplot(dt_wb_cons, aes(x=year, y=pred, color = id)) + geom_line()

## seems plausible

## look at within/between
## base model

gen_wb_res(dt_wb) %>% ggplot(aes(x=year, y=pred, color = id)) + geom_line()

## ggplot(dt_wb_cons_dcsp, aes(x=year, y=pred, color = id)) + geom_line()
## ggplot(dt_wb_dcsp_cons, aes(x=year, y=pred, color = id)) + geom_line()


## weak negative within effect, strong positive between effect
dt_wbc <- copy(dt_wba)[, `:=`(id = "c", x = x + 15, y = (-y*.1) + 4)]
dt_wbd <- copy(dt_wba)[, `:=`(id = "d", x = x, y = (-y*.1) + 1)]
dt_wb2 <- rbind(dt_wbc, dt_wbd)
ggplot(dt_wb2, aes(x=x, y=y, group = id)) + geom_line()

gen_re_res(dt_wb2) %>%  ggplot(aes(x=year, y=pred, color = id)) + geom_line()
gen_wb_res(dt_wb2) %>%  ggplot(aes(x=year, y=pred, color = id)) + geom_line()

dt_res <- rbind(
    copy(dt_wb2)[, .(year, id, y = y, src = "data")],
    gen_re_res(dt_wb2)[, .(year, id, y = pred, src = "re")],
    gen_wb_res(dt_wb2)[, .(year, id, y = pred, src = "wb")])
    
ggplot(dt_res, aes(x=year, y=y, color = id, linetype = src)) + geom_line() +
    facet_wrap(~src)

## not really a difference between RE and WB models



## add more variables
dt_wbe <- copy(dt_wba)[, `:=`(id = "e", x = x + 15, y = (y*.1) + 4 , z = x * 0.2)]
dt_wbf <- copy(dt_wba)[, `:=`(id = "f", x = (x * -1) + 35, y = (y*.1) + 1  , z = x * 0.2)]
dt_wb3 <- rbind(dt_wbe, dt_wbf)
ggplot(dt_wb3, aes(x=x, y=y, color = id)) + geom_line()

ggplot(dt_wb3, aes(x=year, y=y, color = id)) + geom_line()
ggplot(dt_wb3, aes(x=year, y=x, color = id)) + geom_line()

dt_res3 <- rbind(
    ## copy(dt_wb3)[, .(year = x, y=y, id, src = "scatter")],
    copy(dt_wb3)[, .(year, id, y = y, src = "data")],
    gen_re_res(dt_wb3)[, .(year, id, y = pred, src = "re")],
    gen_wb_res(dt_wb3)[, .(year, id, y = pred, src = "wb")])

ggplot(dt_res3, aes(x=year, y=y, color = id, linetype = src)) + geom_line() +
    facet_wrap(~src, nrow = 1, scales = "free")


## ** counterfactual random slope testing, debugging of SEs -> move to point estimates

## fx_rs <- sprintf("nbr_opened ~ %s + (1 + %s | iso3c) + %s",
##                  paste0(iv_vars, collapse = " + "),
##                  vrblx,
##                  "offset(log(SP_POP_TOTLm_lag0_uscld))") %>% as.formula()

## rx_rs <- glmmTMB(fx_rs, dfx, family = nbinom2)
## just scale by population (millions)


## ## compare values 
## dt_slpcprn <- rbind(copy(res_re$dtx_vlus)[, src := "re"],
##       copy(res_rs$dtx_vlus)[, src := "rs"]) %>%
##     .[, vrbl := NULL] %>% 
##     melt(id.vars = c("dt_id", "src")) %>%
##     dcast.data.table(dt_id + variable ~ src) %>%
##     .[, diff := re-rs]

## ## compare rs/re densities
## rbind(
##     copy(res_re$dtx_wse)[, src := "re"],
##     copy(res_rs$dtx_wse)[, src := "rs"]) %>%
##     .[, rid := 1:.N] %>% 
##     ## .[, paste0("rnorm", 1:10) := map(as.list(rnorm(n = 10, mean = .SD$pred, sd = .SD$se)), ~.x), rid]
##     .[, paste0("rnorm", 1:50) := as.list(exp(rnorm(n = 50, mean = pred, sd = se))), rid] %>%
##     .[, map(.SD, sum), by = .(vrbl, src), .SDcols = keep(names(.), ~grepl("rnorm", .x))] %>%
##     melt(id.vars = c("vrbl", "src"), variable.name = "rnorm_id", value.name = "pred") %>%
##     .[, nbr_opened := dt_slpcprn[dt_id == "2k4" & variable == "nbr_opened", re]] %>%
##     .[, diff := nbr_opened - pred] %>%
##     ggplot(aes(x=diff, y=src)) +
##     geom_density_ridges(scale = 0.9, bandwidth = 1.5) +
##     scale_y_discrete(expand = expansion(add = c(0.1, 1))) +
##     geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)

## ## debugging SE predictions
## exp(res_re$dtx_wse$pred) %>% sum()

## ## ## generate at set ranges
## res_re$dtx_wse %>% copy() %>% .[, rid := 1:.N] %>% 
##     .[, paste0("rnorm", 1:13) := as.list(exp(seq(-3,3,0.5)*se + pred)), rid] %>%
##     .[, map(.SD, sum), .SDcols = keep(names(.), ~grepl("rnorm", .x))] %>%
##     melt() %$% plot(value, type = 'l')

## ## generate at random again
## res_re$dtx_wse %>% copy() %>% .[, rid := 1:.N] %>%
##     .[, paste0("rnorm", 1:500) := as.list(exp(
##             ## Winsorize(rnorm(n=500, mean = pred, sd = se), maxval = pred, minval = -100000))), rid] %>%
##             rnorm(n=500, mean = pred, sd= se))), rid] %>% 
##     .[, map(.SD, sum), .SDcols = keep(names(.), ~grepl("rnorm", .x))] %>%
##     melt() %>%
##     ggplot(aes(x=value)) + geom_density(trim = T) + 
##     geom_vline(xintercept = sum(exp(res_re$dtx_wse$pred)))


## first look at nonmodified data: same issue? 
## dtx <- adt(dfx) %>% .[, c("pred", "se") := predict(rx, dfx, se.fit = T)]

## dtx[, pred_exp := predict(rx, dfx, type = "response")]
## dtx[, .(pred, se, pred_exp)]

## map_dbl(1:5000, ~sum(dtx$pred_exp[sample(nrow(dtx), replace = T)])) %>% hist()



## dispersion <- sumry(rx) %>% chuck("sigma")


## dtx[, .(pred, se)] %>% .[, rid := 1:.N] %>%
##     ## .[, paste0("rnorm", 1:100) := as.list(exp(rnorm(n=500, mean = pred, sd= se))), rid] %>%
##     .[, paste0("rgamma", 1:100) := as.list(exp(rgamma(n = 500, shape = 1, rate = 0.10))), rid] %>% 
##     .[, map(.SD, sum), .SDcols = keep(names(.), ~grepl("rnorm", .x))]

## rgamma(n=100, shape = 0.1) %>% hist()


## ## 
## res_re$dtx_wse %>% copy() %>% .[, rid := 1:.N] %>%
##     .[, paste0("rnorm", 1:100) := as.list(rnorm(n=100, mean = pred, sd = se)), rid]



## dt_predres_sdrange <- pred_sdrange(vrblx, rx, dfx, iv_vars)
## ggplot(dt_predres_sdrange, aes(x=updown, y=pred)) + geom_line()



## ** counterfactual prediction weight testing


rxw <- glmmTMB(fx, dfx, family = nbinom1,
               weights = SP_POP_TOTLm_lag0_uscld)


## scale them total weights to 1
rxw2 <- glmmTMB(fx, dfx, family = nbinom1,
                weights = SP_POP_TOTLm_lag0_uscld/sum(SP_POP_TOTLm_lag0_uscld))

## multiply population by 0.1
rxw3 <- glmmTMB(fx, dfx, family = nbinom1,
                weights = SP_POP_TOTLm_lag0_uscld * 0.1)

rxw4 <- glmmTMB(fx, dfx, family = nbinom1,
                weights = SP_POP_TOTLm_lag0_uscld * 0.5)

rxw5 <- glmmTMB(fx, dfx, family = nbinom1,
                weights = SP_POP_TOTLm_lag0_uscld * 0.25)

rxw6 <- glmmTMB(fx, dfx, family = nbinom1,
                weights = SP_POP_TOTLm_lag0_uscld * 0.01)

## compare_models(rx, rxw, rxw2, rxw3, select = "se_p")
compare_models(rxw, rxw3, rxw4, rxw5, select = "se_p")

compare_models(rxw3, rxw4, rxw5, rxw6, select = "se_p")


## adt(dfx)[, SP_POP_TOTLm_lag0_uscld/sum(SP_POP_TOTLm_lag0_uscld)*.N, year][, V1])

## compare_models(rx, rxw)

## see whether coef changes based on weight scale also occurs in OLS
r_olsw1 <- glmmTMB(fx, dfx, weights = SP_POP_TOTLm_lag0_uscld)
r_olsw2 <- glmmTMB(fx, dfx, weights = SP_POP_TOTLm_lag0_uscld * 0.01)

compare_models(r_olsw1, r_olsw2, r_olsw3)


## ## calculate with random intercept and random slopes
## compare with population weights
res_re <- pred_given_const_vrbl(vrblx, rx, dfx, iv_vars)
res_wt <- pred_given_const_vrbl(vrblx, rxw, dfx, iv_vars)
res_wt2 <- pred_given_const_vrbl(vrblx, rxw2, dfx, iv_vars)
res_wt3 <- pred_given_const_vrbl(vrblx, rxw3, dfx, iv_vars)
## res_rs <- pred_given_const_vrbl(vrblx, rx_rs, dfx, iv_vars)




## comparison to glmer.nb
res_glm <- glmer.nb(fx, dfx)
res_glmw <- glmer.nb(fx, dfx, weights = SP_POP_TOTLm_lag0_uscld)
res_glmw2 <- glmer.nb(fx, dfx, weights = SP_POP_TOTLm_lag0_uscld*0.1)
## screenreg(res_glm, res_glmw)

compare_models(rx, res_glm, rxw, res_glmw, select = "se_p")

## compare glmer.nb models of different weights -> also different
compare_models(res_glm, res_glmw, res_glmw2)

## check how weights work
mtcars %>% ggplot(aes(x=wt, y=carb)) + geom_point()
## test expansion of dt
rt_base <- glmmTMB(carb ~ wt + qsec + gear, mtcars, family = nbinom1)
rt_w1 <- glmmTMB(carb ~ wt + qsec +gear , mtcars, family = nbinom1, weights = gear)
dt_mtcars <- adt(mtcars, keep.rownames = "name")[, .(nbr = 1:gear), .(name, carb, wt, qsec, gear)]
rt_w2 <- glmmTMB(carb ~ wt + qsec + gear, dt_mtcars, family = nbinom1)
rt_w3 <- glmmTMB(carb ~ wt + qsec +gear , mtcars, family = nbinom1, weights = gear*0.1)
rt_w4 <- glmmTMB(carb ~ wt + qsec +gear , mtcars, family = nbinom1, weights = gear*100)

compare_models(rt_base, rt_w1, rt_w2, rt_w3, rt_w4, select = "se_p")




## ** chatgpt: garbage
# Load required libraries
library(lme4)

# Set seed for reproducibility
set.seed(123)

# Simulate data for demonstration
N <- 100  # Number of individuals
T <- 5    # Number of time points

# Simulated predictor variable X
X <- rnorm(N*T)

# Simulated outcome variable Y
Y <- 2*X + rnorm(N*T)

# Create a data frame
data <- data.frame(ID = rep(1:N, each = T),
                   Time = rep(1:T, times = N),
                   X,
                   Y)

## *** try 1

# Random intercepts model
model_intercepts <- lmer(Y ~ X + (1 | ID), data = data)

# Random effects within-between model
data$X_between <- ave(data$X, data$ID)  # Calculate subject means of X
data$X_within <- data$X - data$X_between  # Calculate within-person component of X
model_within_between <- lmer(Y ~ X_between + X_within + (1 | ID), data = data)

# Generate predictions from both models
new_data <- data.frame(X = seq(from = -3, to = 3, by = 0.1))
new_data$X_between <- mean(data$X_between)  # Use average subject mean for between-person component
new_data$X_within <- 0  # Set within-person component to 0 for predictions
new_data$ID <- 1

# Predict using random intercepts model
new_data$predicted_intercepts <- predict(model_intercepts, newdata = new_data)

# Predict using random effects within-between model
new_data$predicted_within_between <- predict(model_within_between, newdata = adt(new_data))

# Plot the predicted values
plot(data$X, data$Y, xlab = "X", ylab = "Y", main = "Predicted Values")
lines(new_data$X, new_data$predicted_intercepts, col = "blue", lwd = 2, lty = 1, 
      main = "Predicted Values - Random Intercept Model")
lines(new_data$X, new_data$predicted_within_between, col = "red", lwd = 2, lty = 2,
      main = "Predicted Values - Random Effects Within-Between Model")
legend("topleft", legend = c("Random Intercept Model", "Random Effects Within-Between Model"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))





## *** dt version
# Load required libraries
library(lme4)
library(data.table)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate data for demonstration
N <- 100  # Number of individuals
T <- 5    # Number of time points

# Simulated predictor variable X
X <- rnorm(N * T)

# Simulated outcome variable Y
Y <- 2 * X + rnorm(N * T)

# Create a data table
data <- data.table(ID = rep(1:N, each = T),
                   Time = rep(1:T, times = N),
                   X,
                   Y)

# Random intercepts model
model_intercepts <- lmer(Y ~ X + (1 | ID), data = data)

# Random effects within-between model
data[, X_between := mean(X), by = ID]  # Calculate subject means of X
data[, X_within := X - X_between]  # Calculate within-person component of X
model_within_between <- lmer(Y ~ X_between + X_within + (1 | ID), data = data)

# Generate predictions from both models
new_data <- data.table(X = seq(from = -3, to = 3, by = 0.1), id = 2) %>%
    gen_wb_vrbls(vrbls = c("X"))

## new_data[, X_between := mean(data$X_between)]  # Use average subject mean for between-person component
## new_data[, X_within := 0]  # Set within-person component to 0 for predictions
## new_data[, ID := 1]

# Predict using random intercepts model
new_data[, predicted_intercepts := predict(model_intercepts, newdata = new_data, re.form = NA)]
# Predict using random effects within-between model
new_data[, predicted_within_between := predict(model_within_between, newdata = new_data, re.form = NA)]

# Plot the predicted values
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  geom_line(data = new_data, aes(x = X, y = predicted_intercepts), color = "blue", linetype = "solid") +
  geom_line(data = new_data, aes(x = X, y = predicted_within_between), color = "red", linetype = "dashed") +
  labs(x = "X", y = "Y", title = "Predicted Values") +
  scale_color_manual(values = c("blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal()
