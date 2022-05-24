

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

all_mdl_res <- lapply(unique(df_reg_anls_cfgs$mdl_id), read_reg_res)

coef_df <- lapply(all_mdl_res, \(x) atb(x[["coef_df"]])) %>% bind_rows()
gof_df <- lapply(all_mdl_res, \(x) x[["gof_df"]]) %>% bind_rows() %>% atb()



vrbl <- "hnwi_nbr_30M"
vrbl_lag <- paste0(vrbl, "_lag")

df_anls <- filter(coef_df, scramblematch(vrbl_lag, vrbl_name)) %>% 
    mutate(lag = as.numeric(substring(vrbl_name, nchar(vrbl_lag)+1))) %>%
    merge(df_reg_anls_cfgs_wide) %>% atb() %>%
    filter(vrbl_varied == vrbl, mdl_name == "full")


hist(df_anls$coef, breaks = 800)

table(df_anls$mdl_name)
len(unique(df_anls$lag_spec))


## 

ggplot(df_anls, aes(x=lag, y=coef, group=interaction(mdl_name, base_lag_spec))) +
    facet_wrap(~cbn_name) +
    geom_line(show.legend = F)

unique(df_anls$base_lag_spec)[1]

df_anls %>% filter(base_lag_spec == "14XXXX3X4XX211X22") %>%
    select(mdl_id, coef, lag, cbn_name) %>%
    arrange(cbn_name)

## can't believe coefs are really exactly the same...





