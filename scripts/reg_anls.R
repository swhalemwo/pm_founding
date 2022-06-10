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


## list of all the model results 
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
    filter(vrbl_varied == vrbl_name_unlag |
           (vrbl_varied == "tmitr_approx_linear20step" & vrbl_name_unlag == "ti_tmitr_interact") 
           ,cbn_name != "cbn_controls") %>% ## only use within-base_spec changes, add special case for tmitr
    group_by(vrbl_name_unlag, cbn_name) %>%
    mutate(base_lag_spec_id = as.numeric(factor(base_lag_spec))) %>%
    group_by(vrbl_name_unlag, cbn_name, base_lag_spec_id) %>%
    mutate(nbr_mdls_cvrgd = len(base_lag_spec_id)) %>% 
    filter(nbr_mdls_cvrgd == 5)


df_anls_within_prep2 <- df_anls_within_prep %>%
    group_by(cbn_name, vrbl_name_unlag) %>%
    filter(base_lag_spec_id %in% sample(unique(base_lag_spec_id), 15))

## unique(df_anls_within$vrbl_name_unlag)

df_anls_time_invariant <- df_anls_base %>%
    filter(vrbl_name %in% crscn_vars) %>%
    group_by(cbn_name, vrbl_name) %>%
    slice_sample(n=15) %>%
    mutate(base_lag_spec_id = 1,
           lag = 1,
           nbr_mdls_cvrgd = 1)


df_anls_within <- rbind(df_anls_within_prep2, df_anls_time_invariant)

## order the factors
df_anls_within$vrbl_name_unlag <- factor(df_anls_within$vrbl_name_unlag,
                                         levels = c(ti_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars,
                                                    cult_spending_vars, ctrl_vars_lngtd, crscn_vars))

## shouldn't group by base_lag_spec when selecting
## see if some aux vars can be constructed to select on 
library(ggbeeswarm)


pdf(paste0(FIG_DIR, "reg_within_tmitr_fixed.pdf"), width = 10, height = 12)
ggplot(df_anls_within, aes(x=lag, y=coef, group = base_lag_spec)) +
    geom_line(show.legend = F, alpha = 0.15) +
    geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) + 
    facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y", 
               labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0)) +
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()
        
## df_anls_within_ribbon

## df_anls_within %>% group_by(cbn_name, vrbl_name_unlag, lag) %>%
##     summarize(coef_mean = mean(coef), sd = sd(coef),
##               t_value_mean = mean(t_value)) %>%
##     mutate(coef_min = coef_mean - 1.96*sd, coef_max = coef_mean + 1.96*sd) %>%
##     ggplot(aes(x = lag, y=coef_mean)) +
##     geom_line(aes(color = t_value_mean)) +
##     geom_ribbon(aes(ymin = coef_min, ymax = coef_max), alpha = 0.3) + 
##     facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y") +
##     scale_color_gradient2(low = "blue", mid = "grey", high = "red") 
        


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

## *** models themselves 

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

best_mdl_coefs <- filter(best_mdl_coefs, vrbl_name_unlag %!in% c("ln_s", "cons", "ln_r"))

pdf(paste0(FIG_DIR, "best_models_tmirtr_fixed.pdf"), width = 8, height = 12)
ggplot(best_mdl_coefs, aes(x=lag, y=coef, color = t_value)) +
    geom_quasirandom(aes(shape = factor(sig)), height = 0, width = 0.33, show.legend=T, size = 3) +
    facet_grid(vrbl_name_unlag~cbn_name, scales="free", switch = "y", labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0)) + 
    scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    scale_shape_manual(values = c(1,4))
dev.off()

## *** LL lines

## can just merge
## maybe don't even need: can use best_mdl_coefs?
## nah doesn't have all the lag information anymore 


mdl_fit_df <- merge(df_anls_within,
                   filter(gof_df_cbn, gof_names == "log_likelihood")) %>% 
    group_by(cbn_name, vrbl_name_unlag, base_lag_spec) %>%
    mutate(gof_value = gof_value - min(gof_value)) %>%
    group_by(cbn_name, vrbl_name_unlag, lag) %>%
    summarize(gof_value = mean(gof_value), base_lag_spec = 1)


pdf(paste0(FIG_DIR, "mdl_fit_plot.pdf"), height=10, width = 8)
mdl_fit_df %>% 
    ggplot(aes(x=lag, y=gof_value, group = base_lag_spec)) +
    geom_line(show.legend = F) + 
    geom_point() + 
    facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y", 
               labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0))
dev.off()

## *** two-axes plot

    
    



## combine data to be plotted 
two_axis_df <- rbind(
    best_mdl_coefs %>% select(cbn_name, vrbl_name_unlag, lag, vlu = coef, base_lag_spec) %>%
    mutate(source = "best_coefs"),
    mdl_fit_df %>% select(cbn_name, vrbl_name_unlag, lag, vlu = gof_value, base_lag_spec) %>%
    mutate(source = "ll_lines")) %>% atb()


## get the scales for the best_coef plots
## actually get them from within_anls plot
ll_scale <- df_anls_within %>%
    filter(cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>%
    summarize(min_vlu = min(coef), max_vlu = max(coef), range = max_vlu - min_vlu,
              source = "best_coefs") %>%
    select(cbn_name, vrbl_name_unlag, source, min_vlu, max_vlu, range)

coef_scale <- filter(two_axis_df, cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>%
    group_by(source, cbn_name, vrbl_name_unlag) %>%
    summarize(min_vlu = min(vlu), max_vlu = max(vlu), range = max_vlu - min_vlu) %>%
    filter(source == "ll_lines")


test_scale <- rbind(ll_scale, coef_scale)

## need to scale the same variables consistently -> calculate them outside of dplyr
scaler <- filter(test_scale, source == "ll_lines")$range/filter(test_scale, source == "best_coefs")$range

## now scaling best_coefs to ll_lines 
## the range of best_coefs should now be the range of ll_lines

test_scale2 <- test_scale %>%
    mutate(min_vlu = ifelse(source == "best_coefs", min_vlu * scaler, min_vlu),
           max_vlu = ifelse(source == "best_coefs", max_vlu * scaler, max_vlu))
## ranges (not the value, but the actual range between min and max values) are the same now


offset_vlu <- filter(test_scale2, source == "ll_lines")$max_vlu - filter(test_scale2, source == "best_coefs")$max_vlu 

## don't need to readjust values in test_scale, now I have scaler and offset value -> can adjust actual values
## should tho to check errors
test_scale3 <- test_scale2 %>%
    mutate(min_vlu = ifelse(source == "best_coefs", min_vlu + offset_vlu, min_vlu),
           max_vlu = ifelse(source == "best_coefs", max_vlu + offset_vlu, max_vlu))
           
## hmm seems to work, could ofc be that largest value is not the best fitting one?
## could also be due to negative numbers?
## maybe complete line is more helpful? 


filter(two_axis_df, cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>% 
    mutate(vlu = ifelse(source == "best_coefs", offset_vlu + (vlu * scaler), vlu)) %>% 
    ggplot(aes(x=lag, y=vlu, color = source)) +
    geom_point() +
    scale_y_continuous(name = "ll_lines", sec.axis = sec_axis(~ (.- offset_vlu) /scaler , name = "best_coefs"))
    
## doesn't align completely, but could be correct:
## my overall min from ll_scale is -0.525, but my lowest best_coef is only -0.38
## -0.38 * scaler + offset_vlu = 0.43, which looks correct
## -0.525 * scaler + offset_vlu = 0 -> also correct
## -> so points get scaled correctly, but second axis still wrong
## maybe the sign change?
## doesn't seem like it, but y=ax+b -> x=(y-b)/a

## can make line out of LL if i really want: just pass differently filtered data to geom_point and geom_line

gen_ll_best_coef_plot <- function(vrbl_name_unlag, cbn_name) {
    #' generate individual plot with information on fit (LL) and coef values at best fits
    #' needs as globals: df_anls_within, two_axis_df
    
    

    ll_scale <- df_anls_within %>%
        filter(cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>%
        summarize(min_vlu = min(coef), max_vlu = max(coef), range = max_vlu - min_vlu,
                  source = "best_coefs") %>%
        select(cbn_name, vrbl_name_unlag, source, min_vlu, max_vlu, range)

    ## get the scale information of the best_coefs, actually use full coefs for illustration/maybe adding coef line
    coef_scale <- filter(two_axis_df, cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>%
        group_by(source, cbn_name, vrbl_name_unlag) %>%
        summarize(min_vlu = min(vlu), max_vlu = max(vlu), range = max_vlu - min_vlu) %>%
        filter(source == "ll_lines")

    ## combine scaling information 
    scale_df1 <- rbind(ll_scale, coef_scale)

    ## generate scaler to stretch best_coef range to ll_lines range
    scaler <- filter(scale_df1, source == "ll_lines")$range/filter(scale_df1, source == "best_coefs")$range

    ## adjust ranges, needed to generate offset
    scale_df2 <- scale_df1 %>%
        mutate(min_vlu = ifelse(source == "best_coefs", min_vlu * scaler, min_vlu),
               max_vlu = ifelse(source == "best_coefs", max_vlu * scaler, max_vlu))

    ## generate offset
    offset_vlu <- filter(scale_df2, source == "ll_lines")$max_vlu - filter(scale_df2, source == "best_coefs")$max_vlu 

    filter(two_axis_df, cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>% 
        mutate(vlu = ifelse(source == "best_coefs", offset_vlu + (vlu * scaler), vlu)) %>% 
        ggplot(aes(x=lag, y=vlu, color = source)) +
        geom_point(show.legend = F) +
        scale_y_continuous(name = "", sec.axis = sec_axis(~ (.- offset_vlu) /scaler , name = "")) +
        labs(x="", y="")

}


facets_to_plot <- df_anls_within %>% select(cbn_name, vrbl_name_unlag) %>% unique() %>% arrange(cbn_name, vrbl_name_unlag)

ll_coef_plots <- apply(facets_to_plot[1:6,], 1, \(x) gen_ll_best_coef_plot(x[["vrbl_name_unlag"]], x[["cbn_name"]]))

plot_grid(plotlist = ll_coef_plots[1:6], ncol = 1)

grid.arrange(ll_coef_plots)

grid.arrange(grobs = ll_coef_plots)



library(cowplot)
library(patchwork)

(ll_coef_plots[[1]] + ll_coef_plots[[2]]) / (ll_coef_plots[[3]] + ll_coef_plots[[4]])

wrap_plots(ll_coef_plots) +
    plot_layout(ncol = 2, widths = c(0,2))

lapply(ll_coef_plots, ggplotGrob) %>% rbind() %>% grid.draw()


gen_ll_best_coef_plot("shweal992j_p90p100", "cbn_all")

## ** between-within coef variation

get_between_within_sds <- function(vlu_vec, id_vec) {
    #' generate the between and within variation for value vector given id vector 
    
    dfx = data_frame(vlu = vlu_vec,  id = id_vec)
    xtsum_res <- xtsum(dfx, vlu, id)

    sd_within <- xtsum_res$sd[3]
    sd_between <- xtsum_res$sd[2]

    return(data_frame(sd_within = sd_within, sd_between = sd_between))
}


variation_anls_prep <- df_anls_within %>% group_by(cbn_name, vrbl_name_unlag) %>%
    do(get_between_within_sds(.$coef, .$base_lag_spec))

    
variation_anls <- variation_anls_prep %>% pivot_longer(cols = c(sd_within, sd_between)) %>%
    mutate(cbn_name = factor(cbn_name, levels = rev(names(cbn_dfs))))


pdf(paste0(FIG_DIR, "coef_variation_anls.pdf"), width = 8, height = 9)
variation_anls %>% 
    ggplot(aes(x=value, y=cbn_name , group = name, color = name)) +
    geom_path() +
    geom_point() + 
    facet_wrap(~vrbl_name_unlag, switch = "y", ncol = 1, scales = "free_y",
               labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0))
dev.off()    


    



    



