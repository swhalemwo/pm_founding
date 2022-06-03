## * header

read_reg_res <- function(idx) {
    #' read back model results with some id    

    reg_res <- readRDS(paste0(REG_RES_DIR, idx))

    coef_df <- reg_res$coef_df
    coef_df$mdl_id <- idx
    
    gof_df <- reg_res$gof_df
    gof_df$mdl_id <- idx


    return(list(coef_df = coef_df,
                gof_df = gof_df))
    
}


df_reg_anls_lags <- read.csv(paste0(REG_RES_FILE_LAGS), sep = " ", header = F,
                             col.names = c("variable", "value", "lag_spec", "cfg_id", "mdl_id", "cvrgd")) %>%
    atb()

df_reg_anls_cfgs <- read.csv(paste0(REG_RES_FILE_CFGS), sep = " ", header = F,
                             col.names = c("variable", "value", "cfg_id", "lag_spec", "mdl_id", "cvrgd")) %>%
    atb()

df_reg_anls_cfgs_wide <- df_reg_anls_cfgs %>% select(variable, value, mdl_id, lag_spec) %>%
    pivot_wider(id_cols = c(mdl_id, lag_spec), names_from = variable, values_from = value)

    



## read_reg_res(df_reg_anls_cfgs$mdl_id[[1]])



all_mdl_res <- lapply(unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id), read_reg_res)

coef_df <- lapply(all_mdl_res, \(x) atb(x[["coef_df"]])) %>% bind_rows()
gof_df <- lapply(all_mdl_res, \(x) x[["gof_df"]]) %>% bind_rows() %>% atb()

## add the model details as variables 
gof_df_cbn <- merge(gof_df, df_reg_anls_cfgs_wide) %>% atb()

gof_df_cbn$cbn_name <- factor(gof_df_cbn$cbn_name, levels = names(vrbl_cbns))



pdf(paste0(FIG_DIR, "cbn_log_likelihoods.pdf"), width = 6, height = 3)

filter(gof_df_cbn, gof_names == "log_likelihood") %>%
    ggplot(aes(x = gof_value, fill = cbn_name)) +
    geom_histogram(binwidth = 2)
## facet_wrap(~cbn_name, ncol = 1, scales = "fixed")

dev.off()

## ok makes sense: cbn_all models have best fit: most variables, least observations
## fit gets worse the more variables are removed and the more cases are added


library(stringr)

## construct the within-change df 
df_anls_base <- coef_df %>%
    mutate(vrbl_name_unlag = gsub("_lag[1-5]", "", vrbl_name)) %>%
    merge(df_reg_anls_cfgs_wide) %>% atb() %>%
    mutate(t_value = coef/se, 
           sig = ifelse(pvalues < 0.05, 1, 0))


## ** within base-spec changes

df_anls_within_prep <- df_anls_base %>%
    filter(vrbl_name_unlag != vrbl_name) %>% ## only use the lag variables
    mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5))) %>%
    filter(vrbl_varied == vrbl_name_unlag | (vrbl_varied == "tmitr_approx_linear20step" & vrbl_name_unlag == "ti_tmitr_interact") , cbn_name != "cbn_controls") %>% ## only use within-base_spec changes, add special case for tmitr
    group_by(vrbl_name_unlag, cbn_name) %>%
    mutate(base_lag_spec_id = as.numeric(factor(base_lag_spec))) %>%
    group_by(vrbl_name_unlag, cbn_name, base_lag_spec_id) %>%
    mutate(nbr_mdls_cvrgd = len(base_lag_spec_id)) %>% 
    filter(nbr_mdls_cvrgd == 5)


df_anls_within <- df_anls_within_prep %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    filter(base_lag_spec_id %in% sample(unique(base_lag_spec_id), 15))

## unique(df_anls_within$vrbl_name_unlag)

## order the factors
df_anls_within$vrbl_name_unlag <- factor(df_anls_within$vrbl_name_unlag, levels = c(ti_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars, cult_spending_vars, ctrl_vars_lngtd))

## shouldn't group by base_lag_spec when selecting
## see if some aux vars can be constructed to select on 
library(ggbeeswarm)

pdf(paste0(FIG_DIR, "reg_within_tmitr_fixed.pdf"), width = 8, height = 12)
ggplot(df_anls_within, aes(x=lag, y=coef, group = base_lag_spec)) +
    geom_line(show.legend = F, alpha = 0.15) +
    geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) + 
    facet_grid(cols = vars(cbn_name), rows = vars(vrbl_name_unlag), scales = "free", switch = "y") +
    theme(strip.text.y.left = element_text(angle = 0)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()
        

## ** coefs from all models

df_anls_all <- df_anls_base %>%
    filter(vrbl_name_unlag != vrbl_name) %>% ## only use lagged variables
    mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5))) %>%
    filter(cbn_name != "cbn_controls") %>%
    group_by(vrbl_name_unlag, cbn_name, lag) %>%
    slice_sample(n=10)

table(df_anls_all$vrbl_name_unlag)

df_anls_all$vrbl_name_unlag <- factor(df_anls_all$vrbl_name_unlag, levels = c(ti_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars, cult_spending_vars, ctrl_vars_lngtd))

pdf(paste0(FIG_DIR, "reg_res_all_tmitr_fixed.pdf"), width = 8, height = 12)
ggplot(df_anls_all, aes(x=lag, y=coef)) +
    geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) +
    facet_grid(cols = vars(cbn_name), rows = vars(vrbl_name_unlag), scales = "free", switch = "y") +
    theme(strip.text.y.left = element_text(angle = 0)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()


## ** best fitting models




best_mdls <- gof_df_cbn %>%
    filter(gof_names == "log_likelihood", cbn_name != "cbn_controls") %>%
    group_by(cbn_name) %>% 
    arrange(gof_value) %>%
    slice_tail(n=8)

best_mdl_coefs <- merge(df_anls_base, best_mdls) %>% atb()
best_mdl_coefs$lag <- as.numeric(substring(str_extract(best_mdl_coefs$vrbl_name, "_lag(\\d+)"), 5))
best_mdl_coefs$lag[is.na(best_mdl_coefs$lag)] <- 0

vrbl_levels <- c(ti_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars, cult_spending_vars, ctrl_vars_lngtd)
other_var_names <- unique(best_mdl_coefs$vrbl_name_unlag)[unique(best_mdl_coefs$vrbl_name_unlag) %!in% vrbl_levels]

best_mdl_coefs$vrbl_name_unlag <- factor(best_mdl_coefs$vrbl_name_unlag, levels = c(vrbl_levels, other_var_names))

pdf(paste0(FIG_DIR, "best_models_tmirtr_fixed.pdf"), width = 8, height = 12)
ggplot(best_mdl_coefs, aes(x=lag, y=coef, color = t_value)) +
    geom_quasirandom(aes(shape = factor(sig)), height = 0, width = 0.33, show.legend=T, size = 3) +
    facet_grid(cols = vars(cbn_name), rows = vars(vrbl_name_unlag), scales="free", switch = "y") +
    theme(strip.text.y.left = element_text(angle = 0)) + 
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()
