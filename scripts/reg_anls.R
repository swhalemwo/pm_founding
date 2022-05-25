

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

gof_df_cbn <- merge(gof_df, df_reg_anls_cfgs_wide) %>% atb()

filter(gof_df_cbn, gof_names == "log_likelihood") %>%
    ggplot(aes(x = gof_value)) +
    geom_histogram(binwidth = 0.1) +
    facet_wrap(~cbn_name, ncol = 1, scales = "free")

## ok makes sense: cbn_all models have best fit: most variables, least observations
## fit gets worse the more variables are removed and the more cases are added



vrbl <- "hnwi_nbr_30M"
vrbl <- "NY.GDP.PCAP.CDk"
vrbl_lag <- paste0(vrbl, "_lag")

library(stringr)

## construct the within-change df 
df_anls <- coef_df %>%
    mutate(vrbl_name_unlag = gsub("_lag[1-5]", "", vrbl_name)) %>%
    filter(vrbl_name_unlag != vrbl_name) %>% ## only use the lag variables
    mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5))) %>%
    merge(df_reg_anls_cfgs_wide) %>% atb() %>%
    filter(vrbl_varied == vrbl_name_unlag, cbn_name != "cbn_controls") %>%
    mutate(t_value = coef/se, 
           sig = ifelse(pvalues < 0.05, 1, 0))
    
unique(df_anls$vrbl_name_unlag)


## order the factors
df_anls$vrbl_name_unlag <- factor(df_anls$vrbl_name_unlag, levels = c(ti_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars, cult_spending_vars, ctrl_vars_lngtd))


df_anls2 <- df_anls %>% group_by(vrbl_name_unlag, cbn_name) %>%
    mutate(base_lag_spec_id = as.numeric(factor(base_lag_spec))) %>%
    filter(base_lag_spec_id <= 20)








## shouldn't group by base_lag_spec when selecting
## see if some aux vars can be constructed to select on 
library(ggbeeswarm)

pdf(paste0(FIG_DIR, "first_reg_res3.pdf"), width = 8, height = 12)
ggplot(df_anls2, aes(x=lag, y=coef, group = base_lag_spec)) +
    geom_line(show.legend = F, alpha = 0.15) +
    geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) + 
    facet_grid(cols = vars(cbn_name), rows = vars(vrbl_name_unlag), scales = "free", switch = "y") +
    theme(strip.text.y.left = element_text(angle = 0)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()
        


        


df_anls <- filter(coef_df, scramblematch(vrbl_lag, vrbl_name)) %>% 
    mutate(lag = as.numeric(substring(vrbl_name, nchar(vrbl_lag)+1))) %>%
    merge(df_reg_anls_cfgs_wide) %>% atb() %>%
    filter(vrbl_varied == vrbl, mdl_name == "full")


hist(df_anls$coef, breaks = 800)



ggplot(df_anls, aes(x=lag, y=coef, group=interaction(mdl_name, base_lag_spec))) +
    facet_wrap(~cbn_name) +
    geom_line(show.legend = F)

unique(df_anls$base_lag_spec)[1]

df_anls %>% filter(base_lag_spec == "14XXXX3X4XX211X22") %>%
    select(mdl_id, coef, lag, cbn_name) %>%
    arrange(cbn_name)

## can't believe coefs are really exactly the same...





