## * header
## ** functions 

library(stringr)
library(ggbeeswarm)
library(patchwork)

read_reg_res <- function(idx, fldr_info) {
    #' read back model results with some id    

    reg_res <- readRDS(paste0(fldr_info$REG_RES_DIR, idx))

    coef_df <- reg_res$coef_df
    coef_df$mdl_id <- idx
    
    gof_df <- reg_res$gof_df
    gof_df$mdl_id <- idx


    return(list(coef_df = coef_df,
                gof_df = gof_df))
    
}


read_reg_res_files <- function(fldr_info) {
    #' reads the basic regression result files in
    

    ## df_reg_anls_lags <- read.csv(paste0(fldr_info$REG_RES_FILE_LAGS), sep = " ", header = F,
    ##                              col.names = c("variable", "value", "lag_spec", "cfg_id", "mdl_id", "cvrgd")) %>%
    ##     atb()

    df_reg_anls_cfgs <- read.csv(paste0(fldr_info$REG_RES_FILE_CFGS), sep = " ", header = F,
                                 col.names = c("variable", "value", "cfg_id", "lag_spec", "mdl_id", "cvrgd")) %>%
        atb()

    df_reg_anls_cfgs_wide <- df_reg_anls_cfgs %>% select(variable, value, mdl_id, lag_spec, cvrgd) %>% unique() %>% 
        pivot_wider(id_cols = c(mdl_id, lag_spec, cvrgd), names_from = variable, values_from = value)


    ## read_reg_res(df_reg_anls_cfgs$mdl_id[[1]])


    ## list of all the model results 
    all_mdl_res <- mclapply(unique(filter(df_reg_anls_cfgs, cvrgd == 1)$mdl_id), \(x)
                            read_reg_res(x, fldr_info), mc.cores = 6)

    coef_df <- mclapply(all_mdl_res, \(x) atb(x[["coef_df"]]), mc.cores = 6) %>% bind_rows()
    gof_df <- mclapply(all_mdl_res, \(x) x[["gof_df"]], mc.cores = 6) %>% bind_rows() %>% atb()

    ## add the model details as variables 
    gof_df_cbn <- merge(gof_df, df_reg_anls_cfgs_wide) %>% atb()

    gof_df_cbn$cbn_name <- factor(gof_df_cbn$cbn_name, levels = names(vrbl_cbns))


    ## construct the within-change df


    return(list(
        df_reg_anls_cfgs_wide = df_reg_anls_cfgs_wide,
        coef_df = coef_df,
        gof_df_cbn = gof_df_cbn
        ))

}

add_coef_sig <- function(coef_df,  df_reg_anls_cfgs_wide) {
    #' add significance to coefs

    df_anls_base <- coef_df %>%
        mutate(vrbl_name_unlag = gsub("_lag[1-5]", "", vrbl_name),
               lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5)),
               lag = ifelse(is.na(lag), 0, lag)) %>%
        merge(df_reg_anls_cfgs_wide) %>% atb() %>%
        mutate(t_value = coef/se, 
               sig = ifelse(pvalues < 0.05, 1, 0))

    return(df_anls_base)

}


construct_df_anls_within_prep <- function(df_anls_base, optmzd) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' construct the within-base-spec changes:
    #' only get the coefs of variables where the variable is varied (within base-spec changes)
    #' only get coefs where all 5 variations converged
    
    df_anls_within_prep1 <- df_anls_base %>%
        filter(vrbl_name_unlag != vrbl_name) %>% ## only use the lag variables
        mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5)))


    ## if reg_res is created by step-wise optimization, vrbl_varied is not available 
    ## here it's created from vrbl_optmzd, which is the best within-lag variation I think I have for optmzed runs
    ## if optmzd: also group by loop_nbr: otherwise super large
    
    if (optmzd) {
        df_anls_within_prep2 <- df_anls_within_prep1 %>%
            mutate(vrbl_varied = vrbl_optmzd)
        
        group_vrbls <- c("vrbl_name_unlag", "cbn_name", "base_lag_spec_id", "regcmd", "loop_nbr")

    } else {
        df_anls_within_prep2 <- df_anls_within_prep1
        group_vrbls <- c("vrbl_name_unlag", "cbn_name", "base_lag_spec_id", "regcmd")
    }
    
    ## only use within-base_spec changes, add special case for tmitr
    df_anls_within_prep3 <- df_anls_within_prep2 %>% 
        filter(vrbl_varied == vrbl_name_unlag |
               (vrbl_varied == "tmitr_approx_linear20step" & vrbl_name_unlag == "ti_tmitr_interact") 
              ,cbn_name != "cbn_controls")

    ## names(df_anls_within_prep3)
    ## dt_anls_within_prep3 <- adt(df_anls_within_prep3)
    ## dt_anls_within_prep3[, lapply(.SD, uniqueN)] %>% melt() ## check nbr of unique values, maybe lagspec?
    ## dt_anls_within_prep3[, .N, lag_spec][, .N, N] ## doesn't seem like it: so much
    

    df_anls_within_prep4 <- df_anls_within_prep3 %>%
        group_by(vrbl_name_unlag, cbn_name) %>%
        mutate(base_lag_spec_id = as.numeric(factor(base_lag_spec)))

    ## add some lag_variation ID (lag_variatn): convert to dt because dt is awesome
    ## lag_variatn: the grouping of models by varying the lag of one variable while keeping all others constant
    dt_anls_within_prep4 <- adt(df_anls_within_prep4)
    dt_anls_within_prep4[, lag_variatn := .GRP, by = group_vrbls]
    ## dt_anls_within_prep4[, .N, group_vrbls][, .N, N] ## check the grouping counts
    ## dt_anls_within_prep4[, .N, lag_variatn][, .N, N]


    df_anls_within_prep5 <- atb(dt_anls_within_prep4) %>%
        group_by(across(all_of(group_vrbls))) %>% # haha plain english verbs make it so EZ AMIRITE
        mutate(nbr_mdls_cvrgd = len(base_lag_spec_id)) %>% 
        filter(nbr_mdls_cvrgd == 5)


    return(df_anls_within_prep5)

}




construct_time_invariant_coefs <- function(df_anls_base, vvs, df_anls_within_prep2, NBR_MDLS) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' grab some coefs of cross-sectional variables, to be rbinded with the longitudinal coefs
    
    df_anls_time_invariant_prep1 <- df_anls_base %>%
        filter(vrbl_name %in% vvs$crscn_vars)

    ## inner_join(df_anls_time_invariant_prep1, df_anls_within_prep2, on = "mdl_id")
    ## intersect(df_anls_time_invariant_prep1$mdl_id, df_anls_within_prep2$mdl_id) %>% len()

    ## can't get ALL the coefs of invariant variables from the models used in df_anls_within_prep2:
    ## each model has its own set of coefs for the time-invariant variables -> way too many
    ## just pick some top ones -> need gof

    ## join with data table because for some reason inner_join doesn't work?
    dt_anls_time_invariant_prep2 <- adt(df_anls_time_invariant_prep1)[
        ## include all kinds of variables in df_anls_within_prep2 that are later need in rbind 
        adt(df_anls_within_prep2)[, .(mdl_id,gof_value, lag_variatn, base_lag_spec_id, nbr_mdls_cvrgd)],
        on = "mdl_id"]
      
    ## need to get the mdls from which I want to take coefs
    ## then use the mdl_ids to get the coefs of time-invariant vrbls
    ## and also use the mdl_ids to get the lag_variatn/gof_value from df_anls_within_prep2 to rbind stuff together
    ## LUL actually don't need to because .SD is just so amazing in tucking all the vrbls I need in there

    grp_vrbls <- c("regcmd", "cbn_name", "vrbl_name_unlag")

    ## order the data.table, select top models
    time_invrnt_mdl_ids <- dt_anls_time_invariant_prep2[order(-gof_value), .SD[1:NBR_MDLS], by= grp_vrbls]

    ## just assign lag = 3 to time invariant variables
    time_invrnt_mdl_ids$lag <- 3

    ## if optzmd: make sure that time invariant-variables are also properly labeled 
    if ("vrbl_optmzd" %in% names(time_invrnt_mdl_ids)) {
        time_invrnt_mdl_ids$vrbl_varied = time_invrnt_mdl_ids$vrbl_optmzd
    }
            
    ## plot(time_invrnt_mdl_ids$gof_value)
    ## data.table seems robust enough to allow different group order? 
    ## len(intersect(time_invrnt_mdl_ids$mdl_id, time_invrnt_mdl_ids2$mdl_id))
    ## grp_vrbls2 <- c("vrbl_name_unlag", "regcmd", "cbn_name")
    ## time_invrnt_mdl_ids2 <- dt_anls_time_invariant_prep2[order(-gof_value), .SD[1:NBR_MDLS], by= grp_vrbls2]
    ## plot(dt_anls_time_invariant_prep2[order(-gof_value), .SD, by= grp_vrbls2]$gof_value)
    ## plot(dt_anls_time_invariant_prep2[order(-gof_value), .SD, by = grp_vrbls]$gof_value)

    ##     group_by(cbn_name, vrbl_name) %>%
    ##     slice_sample(n=15) %>%
    ##     mutate(base_lag_spec_id = 1,
    ##            lag = 1,
    ##            nbr_mdls_cvrgd = 1)

    ## return(df_anls_time_invariant)

    return(atb(time_invrnt_mdl_ids))
}

construct_df_anls_within <- function(df_anls_base, vvs, NBR_MDLS, optmzd, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' construct the dataset of the within lag-spec changes

    df_anls_within_prep <- construct_df_anls_within_prep(df_anls_base, optmzd = optmzd)

    gof_df_cbn_fltrd <- gof_df_cbn %>%
        filter(gof_names == "log_likelihood") %>%
        select(mdl_id, gof_value)

    ## intersect(names(df_anls_within_prep), names(gof_df_cbn_fltrd))

    df_anls_within_prep1 <- inner_join(df_anls_within_prep, gof_df_cbn_fltrd, by="mdl_id")

    ## filter out a number of models per lag (and cbn)
    ## really seems like I need to stick to this awkward filtering: 
    ## slicing would only select rows, not groups of rows:
    ## -> either drop coefs or get all (when also grouping by base_lag_spec_id)
    ## df_anls_within_prep2 <- df_anls_within_prep %>%
    ##     group_by(cbn_name, vrbl_name_unlag, regcmd) %>%
    ##     filter(base_lag_spec_id %in% sample(unique(base_lag_spec_id),
    ##                                         min(NBR_MDLS, n_distinct(base_lag_spec_id))))

    ## ## only select coefs from the NBR_MDLS best fitting ones
    ## ## probably have to re-number the base_lag_spec_ids
    ## df_anls_within_prep2 <- df_anls_within_prep1 %>%
    ##     group_by(cbn_name, vrbl_name_unlag, regcmd) %>%
    ##     arrange(-gof_value) %>%
    ##     mutate(base_lag_spec_id2 = as.numeric(factor(base_lag_spec))) %>%
    ##     select(mdl_id, gof_value, base_lag_spec_id2) %>% 
    ##     print(n=200)
        

    ##     filter(base_lag_spec_id %in% sample(unique(base_lag_spec_id),
    ##                                         min(NBR_MDLS, n_distinct(base_lag_spec_id))))

    

    df_lag_variatns <- df_anls_within_prep1 %>%
        group_by(cbn_name, vrbl_name_unlag, regcmd, lag_variatn) %>%
        summarize(max_gof = max(gof_value)) %>%
        group_by(regcmd, cbn_name, vrbl_name_unlag) %>%
        slice_max(max_gof, n=NBR_MDLS, with_ties = F) %>%
        ungroup() %>% 
        select(lag_variatn)
    
    df_anls_within_prep2 <- inner_join(df_lag_variatns, df_anls_within_prep1, by="lag_variatn") 
    ## select(-lag_variatn, -gof_value) ## yeet variables that are not in df_anls_time_invariant
    ## ehhh should add them there as well
    
    ## should get the time-invariant coefs of the mdls that I'm using in df_anls_within_prep2
    
    df_anls_time_invariant <- construct_time_invariant_coefs(df_anls_base, vvs, df_anls_within_prep2, NBR_MDLS)

    ## setdiff(names(df_anls_within_prep2), names(df_anls_time_invariant))

    df_anls_within <- rbind(df_anls_within_prep2, df_anls_time_invariant)

    ## order the factors: use the vvs$vrbl_lbls order
    df_anls_within$vrbl_name_unlag <- factor(df_anls_within$vrbl_name_unlag,
                                             levels = names(vvs$vrbl_lbls)[names(vvs$vrbl_lbls) %in%
                                                                           unique(df_anls_within$vrbl_name_unlag)])
    ## c(vvs$ti_vars, vvs$density_vars, vvs$hnwi_vars,
    ##   vvs$inc_ineq_vars, vvs$weal_ineq_vars,
    ##   vvs$cult_spending_vars, vvs$ctrl_vars_lngtd,
    ##   vvs$crscn_vars))


    return(df_anls_within)
}



construct_df_anls_all <- function(df_anls_base, vvs, NBR_MDLS) {
    #' pick coefs from all models 

    df_anls_all <- df_anls_base %>%
        filter(vrbl_name_unlag != vrbl_name) %>% ## only use lagged variables
        mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5))) %>%
        filter(cbn_name != "cbn_controls") %>%
        group_by(vrbl_name_unlag, cbn_name, lag) %>%
        slice_sample(n=NBR_MDLS)

## table(df_anls_all$vrbl_name_unlag)

    df_anls_all$vrbl_name_unlag <- factor(df_anls_all$vrbl_name_unlag,
                                          levels = c(names(vvs$vrbl_lbls)[names(vvs$vrbl_lbls) %in%
                                                                          df_anls_all$vrbl_name_unlag]))

    return(df_anls_all)
}

construct_df_best_mdls <- function(df_anls_base, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' construct the data of the best fitting models per combination
    

    ## filter out HNWI 1B for now 
    df_anls_base_no1B <- df_anls_base %>% group_by(mdl_id) %>%
        filter("hnwi_nbr_1B" %!in% vrbl_name_unlag)

    ## add some check to make sure there are no NAs in gof_value
    if (any(is.na(gof_df_cbn$gof_value))) {
        stop("some NAs in gof_df_cbn$gof_value (construct_df_best_mdls)")}

    ## select best model per combination
    best_mdls <- gof_df_cbn %>%
        filter(gof_names == "log_likelihood", cbn_name != "cbn_controls",
               mdl_id %in% unique(df_anls_base_no1B$mdl_id)) %>%
        filter(!is.na(gof_value)) %>% 
        group_by(cbn_name, regcmd) %>% 
        arrange(gof_value) %>%
        slice_tail(n=1)
    
    ## gof_df_cbn %>% filter(is.na(gof_value)) %>% pull(mdl_id)


    best_mdl_coefs <- merge(df_anls_base, best_mdls) %>% atb()
    
    ## setdiff(unique(best_mdl_coefs$vrbl_name_unlag), names(vvs$vrbl_lbls))

    ## best_mdl_coefs$lag <- as.numeric(substring(str_extract(best_mdl_coefs$vrbl_name, "_lag(\\d+)"), 5))
    ## best_mdl_coefs$lag[is.na(best_mdl_coefs$lag)] <- 0
    
    ## vrbl_levels <- c("sum_core", vvs$ti_vars, vvs$density_vars, vvs$hnwi_vars, vvs$inc_ineq_vars,
    ##                  vvs$weal_ineq_vars, vvs$cult_spending_vars, vvs$ctrl_vars_lngtd)
    
    ## other_var_names <- c(unique(best_mdl_coefs$vrbl_name_unlag)[unique(best_mdl_coefs$vrbl_name_unlag) %!in% vrbl_levels])
    ## levels = c(vrbl_levels, other_var_names))
    

    ## filter out stuff I don't need
    best_mdl_coefs2 <- filter(best_mdl_coefs, vrbl_name_unlag %!in%
                                              c("ln_s", "cons", "ln_r", "alpha", "intcpt_var"))

    ## reordering the variables: TI, hnwi, inequ, cult spending, controls (defined in vvs$vrbl_labels)
   
    vrbl_lbls <- names(vvs$vrbl_lbls)[names(vvs$vrbl_lbls) %in% unique(best_mdl_coefs2$vrbl_name_unlag)]

    best_mdl_coefs3 <- best_mdl_coefs2 %>%
        mutate(vrbl_name_unlag = factor(vrbl_name_unlag, levels = vrbl_lbls))


    return(best_mdl_coefs3)
}


construct_best_mdls_summary <- function(df_best_mdls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' summary variables for best models 
    
    mdl_summary <- df_best_mdls %>% group_by(vrbl_name_unlag, cbn_name, regcmd) %>%
        summarize(coef = mean(coef), lag_mean = mean(lag), lag_sd = sd(lag), p_value = mean(pvalues),
                  t_value = mean(t_value), se = mean(se), min = coef - 1.96*se, max = coef + 1.96*se,
                  sig = ifelse(abs(t_value) > 1.96, 1,0))

    ## have to reverse order to make points look good 
    mdl_summary$vrbl_name_unlag <- factor(mdl_summary$vrbl_name_unlag,
                                          levels = rev(levels(mdl_summary$vrbl_name_unlag)))

    return(mdl_summary)
}


proc_reg_res_objs <- function(reg_anls_base, vvs, NBR_MDLS) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' further processing of the regression res objects, no reading-in here

    
    coef_df <- reg_anls_base$coef_df
    df_reg_anls_cfgs_wide <- reg_anls_base$df_reg_anls_cfgs_wide
    gof_df_cbn <- reg_anls_base$gof_df_cbn

    ## merging significance to all coefs (maybe can be 
    df_anls_base <- add_coef_sig(coef_df, df_reg_anls_cfgs_wide)

    ## number of models to pick for the analyses
    

    optmzd = "loop_nbr" %in% names(gof_df_cbn)
        
    df_anls_within <- construct_df_anls_within(df_anls_base, vvs, NBR_MDLS, optmzd, gof_df_cbn)
    df_anls_all <- construct_df_anls_all(df_anls_base, vvs, NBR_MDLS)

    df_best_mdls <- construct_df_best_mdls(df_anls_base, gof_df_cbn)
    mdl_summary <- construct_best_mdls_summary(df_best_mdls)


    return(
        list(
            gof_df_cbn = gof_df_cbn,
            df_anls_base = df_anls_base,
            df_anls_within = df_anls_within,
            df_anls_all = df_anls_all,
            df_best_mdls = df_best_mdls,
            mdl_summary = mdl_summary
        )
    )
    
}

## gen_plt_mdl_summary(mdl_summary, vvs)

## proc_reg_res_objs(reg_anls_base, vvs)




gen_plt_cbn_log_likelihoods <- function(gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plot of likelihoods

    ## ok makes sense: cbn_all models have best fit: most variables, least observations
    ## fit gets worse the more variables are removed and the more cases are added

   
    gof_df_cbn_prep <- gof_df_cbn %>% 
        filter(gof_names == "log_likelihood")

    vlines <- gof_df_cbn_prep %>%
        group_by(cbn_name, regcmd) %>%
        summarize(vlines = max(gof_value))


    gof_df_cbn_prep %>% 
        ggplot(aes(x = gof_value, fill = cbn_name, group = cbn_name)) +
        geom_histogram(aes(y= ..density..), binwidth = 1) +
        ## geom_density(geom = "line", position = "identity", n=8000) +
        ## geom_vline(aes(xintercept =max (gof_value))) + 
        xlim(c(min(gof_df_cbn_prep$gof_value)-1,
               max(gof_df_cbn_prep$gof_value)+1)) + 
        geom_vline(vlines, mapping = aes(xintercept = vlines)) + 
        facet_wrap(~regcmd, ncol = 1) 




}

gen_plt_reg_res_within <- function(df_anls_within, vvs, NBR_MDLS) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' plot the within coef change (all other coefs constant)

    ## debugging some weird squiggly lines
    ## ## general filtering
    ## filter(df_anls_within, cbn_name == "cbn_all", 
    ##        vrbl_name_unlag %in% c("smorc_dollar_fxm", "sptinc992j_p99p100")) %>%
    ##     ## narrow filtering
    ##     filter(vrbl_name_unlag == "sptinc992j_p99p100", regcmd == "menbreg") %>%
    ##     select(lag_variatn, mdl_id, base_lag_spec) %>% adt() %>% .[, .N, base_lag_spec]
        

    ## filter(df_anls_within, cbn_name == "cbn_all", 
    ##        vrbl_name_unlag %in% c("smorc_dollar_fxm", "sptinc992j_p99p100")) %>%
    df_anls_within %>%
        ## group = interaction(base_lag_spec, regcmd)
        ggplot(aes(x=lag, y=coef, group = lag_variatn)) +
        geom_line(aes(linetype = regcmd), show.legend = T, alpha = 1/NBR_MDLS) +
        geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2,  width = 0.3, stroke = 1) + 
        facet_grid(vrbl_name_unlag ~ cbn_name + regcmd, scales = "free", switch = "y", 
                   ## labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
                   labeller = as_labeller(c(vvs$vrbl_lbls, vvs$cbn_lbls, vvs$regcmd_lbls),
                                          default = label_wrap_gen(35))) +
                   ## labeller = label_wrap_gen(width = 2)) + 
        theme(strip.text.y.left = element_text(angle = 0),
              panel.spacing.y = unit(0.1, "lines"),
              panel.background = element_rect(fill = NA, color = "black")) +
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))
    
}

gen_plt_reg_res_all <- function(df_anls_all, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' plot coefs from all models 
    

    ggplot(df_anls_all, aes(x=lag, y=coef)) +
        geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, width = 0.3, stroke = 1) +
        ## facet_grid(cols = c(vars(cbn_name), vars(regcmd)), rows = vars(vrbl_name_unlag),
        facet_grid(vrbl_name_unlag ~ cbn_name + regcmd,
                            scales = "free", switch = "y",
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0)) +
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))
    
}

gen_plt_lag_cprn <- function(df_best_mdls, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plot that allows easy comparison of lags chosen across combinations and models 
    
    

    ## filter down to necessary variables (lag info)
    lag_prep <- filter(df_best_mdls, vrbl_name_unlag %!in% vvs$crscn_vars) %>%
        select(vrbl_name_unlag, lag, regcmd, cbn_name) %>%
        mutate(source = "lag") %>% adt()

    ## calculate diff in lags, put in into separate (small; space="free") facet
    lag_diff <- lag_prep %>%  adt() %>% 
        dcast.data.table(vrbl_name_unlag + cbn_name ~ regcmd, value.var = "lag") %>%
        .[,lag_diff := abs(menbreg - xtnbreg)] %>%
        .[, .(vrbl_name_unlag, cbn_name, lag = lag_diff)] %>% .[, source := "lagdiff"]

    lag_cbn <- rbind(lag_prep, lag_diff, fill = T) %>% adf()


    lag_cbn %>% 
        ggplot(aes(x = lag, y= as.character(vrbl_name_unlag), group = regcmd, fill = regcmd)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
        facet_grid(~cbn_name + source, scales = "free", space = "free") +
        scale_y_discrete(labels = vvs$vrbl_lbls) +
        labs(y="variable") +
        theme(legend.position = "bottom") +
        scale_x_continuous(breaks = seq(0,5))

}




gen_plt_best_mdls_wlag <- function(df_best_mdls, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plot of coefs of beste models with lag

    ggplot(df_best_mdls, aes(x=lag, y=exp(coef), color = t_value)) +
        geom_quasirandom(aes(shape = factor(sig), stroke = as.numeric(factor(regcmd))-0.5),
                         width = 0.33, show.legend=T, size = 3) +
        facet_grid(vrbl_name_unlag~ cbn_name + regcmd , scales="free", switch = "y",
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0)) + 
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))

}

gen_plt_mdl_summary <- function(mdl_summary, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the summary plot of the best models 

    mdl_summary2 <- mdl_summary %>% 
        mutate(time_invariant = ifelse(vrbl_name_unlag %in% vvs$crscn_vars, T, F))

    
    ## ggplot(mdl_summary2, aes(color = factor(sig), y = as.character(vrbl_name_unlag), group = regcmd)) +
    ##     geom_point(aes(x = coef, shape = factor(regcmd)), size = 2.5, alpha = 0.95, show.legend = T,
    ##                position = position_dodge(width = 0.5)) +
    ##     geom_errorbarh(aes(xmin = min, xmax = max, , height= 0.1), alpha = 0.6, show.legend = F,
    ##                    position = position_dodge(width = 0.5)) +
    ##     facet_wrap(~cbn_name) +
    ##     ## facet_wrap(time_invariant + vrbl_name_unlag  ~ cbn_name, scales = "free_x", ncol = 3,
    ##     ##            switch = "y", drop = T) +
    ##     geom_vline(xintercept =0, linetype = "dashed") +
    ##     scale_shape_manual(values = c(15,16)) +
    ##     scale_color_manual(values = c("#1C5BA6", "#BD0017")) +
    ##     scale_y_discrete(labels = vvs$vrbl_lbls) +
    ##     ## coord_cartesian(xlim = c(min(mdl_summary$coef)*0.9, max(mdl_summary$coef)*0.9)) + 
    ##     ## coord_cartesian(xlim=c(-1, 1)) +
    ##     labs(x="coefficient size, 95% CI", y="coefficient")

    ## fx <- c(`>` , `<`)
    ## filter(mdl_summary2, fx[[1]](se, 0.5))
    ## plts_scale <- lapply(c(`>=`, `<`), \(x) filter(mdl_summary2, 


    se_large_thld <- quantile(mdl_summary2$se, probs = 0.7)
    coef_thld_lo <- quantile(mdl_summary2$coef, probs = 0.15)
    coef_thld_hi <- quantile(mdl_summary2$coef, probs = 0.85)

    mdl_summary_split <- mdl_summary2 %>%
        group_by(vrbl_name_unlag) %>% 
        ## mutate(se_large = ifelse(any(se > se_large_thld), T, F)) %>%
        ## split(.$se_large)
        mutate(coef_large = ifelse(any(coef > coef_thld_hi | coef < coef_thld_lo), T, F)) %>%
        split(.$coef_large)
    plts_scale <- mdl_summary_split %>% lapply(\(x) x %>% 
                    ggplot(aes(color = factor(sig), y = vrbl_name_unlag, group = regcmd)) +
                    geom_point(aes(x = coef, shape = factor(regcmd)), size = 2.5, alpha = 0.95, show.legend = T,
                               position = position_dodge(width = 0.5)) +
                    geom_errorbarh(aes(xmin = min, xmax = max, , height= 0.1), alpha = 0.6, show.legend = F,
                                   position = position_dodge(width = 0.5)) +
                    facet_wrap(~cbn_name) +
                    geom_vline(xintercept =0, linetype = "dashed") +
                    scale_shape_manual(values = c(15,16)) +
                    scale_color_manual(values = c("#1C5BA6", "#BD0017")) +
                    scale_y_discrete(labels = vvs$vrbl_lbls) +
                    labs(x="", y="") +
                    theme(legend.position = "bottom"))

    plts_scale[[1]] <- plts_scale[[1]] + coord_cartesian(xlim = c(coef_thld_lo, coef_thld_hi))
    plts_scale[[2]] <- plts_scale[[2]] + coord_cartesian(xlim = c(min(mdl_summary2$coef), max(mdl_summary2$coef)))
   
    ## combine plots with patchwork,
    ## add some arbitrary scaling for now to make difference between variables more equal
    plt_cbn <- plts_scale[[1]] / plts_scale[[2]] +
        plot_layout(heights = c(len(unique(mdl_summary_split[[1]]$vrbl_name_unlag)),
                                len(unique(mdl_summary_split[[2]]$vrbl_name_unlag))),
                    guides = "collect") &
        theme(legend.position = 'bottom')



    return(plt_cbn)           

}

gen_plt_cvrgnc <- function(gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plot of convergence: run it only if loop_nbr in gof_df_cbn

    ## formalized test of convergence
    ## question: do the base lag specs converge to the same gof_value
    ## group by: vrbl_choice (based on thld_choice), cbn, 
    ## first: get the top values (values only go up)

    cvrgnc_df_prep <- filter(gof_df_cbn, gof_names == "log_likelihood") %>%
        select(gof_value, base_lag_spec, loop_nbr, vrbl_optmzd, cbn_name, regcmd) %>%
        mutate(step_base = 1, loop_nbr = as.numeric(loop_nbr),
               vrbl_choice = gsub("[1-5]", "0", base_lag_spec))

    ## developing analysis of value-analyzing
    ## dtx <- data.table(vlu = c(1,1,2, 1,2,2, 2, 2, 1), id = c(rep("a", 3), rep("b", 3), rep("c", 3)))
    ## dtx[id == "a", paste0(table(vlu), collapse = "")]
    ## dtx[, paste0(table(vlu), collapse = ""), by="id"]

    cvrgnc_df_test <- cvrgnc_df_prep %>% 
        group_by(vrbl_choice, base_lag_spec, regcmd, cbn_name) %>% # get top values of each run 
        ## group_by(vrbl_choice, base_lag_spec, regcmd, cbn_name) %>% # get top values 
        slice_max(gof_value, n=1, with_ties = F) %>%
        group_by(cbn_name, vrbl_choice, regcmd) %>% ## group by nbr_specs per thld
        summarize(n_gof = len(gof_value), n_distinct_gof = n_distinct(gof_value), var_gof = sd(gof_value),
                  vlu_proc = paste0(table(gof_value), collapse = "")) %>%
        adt()

    ## generate summary table of how many times the different starting values reached the same/different results
    print("convergence summary")
    print(cvrgnc_df_test[, .(nbr = .N, mean_var_gof = mean(var_gof)), by = n_distinct_gof])

    ## generate summary of gof value distribution, focus on non-identical convergence
    print("convergence summary 2")
    cvrgnc_df_test[, .(.N, mean_var_gof = mean(var_gof)), vlu_proc] %>%
        .[, `:=`(ttl = sum(N), prop = 100*N/sum(N))] %>% print()
    

    ## progress after each variable
    ## variables are randomly chosen, so step is different for each base_lag_spec
    cvrgnc_df_prep2 <- cvrgnc_df_prep %>% 
        group_by(vrbl_choice, cbn_name, base_lag_spec, loop_nbr, vrbl_optmzd, regcmd) %>%
        slice_max(gof_value, with_ties = F)

    ## group_vrbls <- c("vrbl_choice", "cbn_name", "base_lag_spec", "loop_nbr", "vrbl_optmzd", "regcmd")

    ## cvrgnc_df_prep2 <- cvrgnc_df_prep %>% adt() %>%
    ##     .[, .(gof_value = max(gof_value), step_base = 1), by = group_vrbls] %>% atb()

    cvrgnc_df_prep3 <- cvrgnc_df_prep2 %>% 
        group_by(cbn_name,base_lag_spec,regcmd) %>% 
        arrange(gof_value) %>%
        mutate(step = ave(step_base, FUN = cumsum)) 


    plt_cvrgnc <- cvrgnc_df_prep3 %>% 
        ggplot(aes(x=step, y=gof_value, group = interaction(base_lag_spec, regcmd), color = vrbl_choice,
                   linetype =regcmd)) +
        geom_line() +
        facet_wrap(~cbn_name, ncol = 1, scales = "free_y")

    return(plt_cvrgnc)
}




gen_reg_res_plts <- function(reg_res_objs, vvs, NBR_MDLS) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate all the plots
    

    gof_df_cbn <- reg_res_objs$gof_df_cbn
    df_anls_within <- reg_res_objs$df_anls_within
    df_best_mdls <- reg_res_objs$df_best_mdls
    mdl_summary <- reg_res_objs$mdl_summary

    plt_cbn_log_likelihoods = gen_plt_cbn_log_likelihoods(gof_df_cbn)
    plt_reg_res_within = gen_plt_reg_res_within(df_anls_within, vvs, NBR_MDLS)
    plt_reg_res_all = gen_plt_reg_res_all(df_anls_within, vvs)
    plt_best_models_wlag = gen_plt_best_mdls_wlag(df_best_mdls, vvs)
    plt_best_models_condensed = gen_plt_mdl_summary(mdl_summary, vvs)

    l_plts <- list(plt_cbn_log_likelihoods= plt_cbn_log_likelihoods,
                   plt_reg_res_within = plt_reg_res_within,
                   plt_reg_res_all = plt_reg_res_all,
                   plt_best_models_wlag = plt_best_models_wlag,
                   plt_best_models_condensed = plt_best_models_condensed)

    
    ## only generate lag cprn plot if multiple regcmds are used
    if (all(c("menbreg", "xtnbreg") %in% df_best_mdls$regcmd)) {
        plt_lag_cprn <- gen_plt_lag_cprn(df_best_mdls, vvs)
        l_plts <- c(l_plts, list(plt_lag_cprn = plt_lag_cprn))
    }

    
    ## only generate convergence plot when using optimization
    if ("loop_nbr" %in% names(gof_df_cbn)) {
        plt_cvrgnc = gen_plt_cvrgnc(gof_df_cbn)
        l_plts <- c(l_plts, list(plt_cvrgnc = plt_cvrgnc))
    }


    return(l_plts)

}


## reg_res$plts$cbn_log_likelihoods <- gen_plt_cbn_log_likelihoods(reg_anls_base$gof_df_cbn)


gen_plt_cfgs <- function() {
    #' generate the plot configs (manually specified)
    
    return(
        list(
            plt_cbn_log_likelihoods = list(filename = "cbn_log_likelihoods.pdf", width = 6, height = 3),
            plt_reg_res_within = list(filename = "reg_res_within.pdf", width = 12, height = 12),
            plt_reg_res_all = list(filename = "reg_res_all.pdf", width = 12, height = 12),
            plt_best_models_wlag = list(filename = "best_models_wlag.pdf", width = 8, height = 12),
            plt_best_models_condensed = list(filename = "best_models_condensed.pdf", width = 9, height = 8),
            plt_lag_cprn = list(filename = "lag_cprn.pdf", width = 7, height = 5),
            plt_cvrgnc = list(filename = "crvgnc.pdf", width = 5, height = 7)
        )
    )

}

render_all_reg_res_plts <- function(reg_res, batch_version) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' wrapper to comfily render all the plots of a regression results version

    
    lapply(names(reg_res$plts), \(x)
           render_reg_res(x, reg_res, reg_res$plt_cfgs, batch_version))

    
}
    


render_reg_res <- function(plt_name, reg_res, plt_cfgs, batch_version) {
    #' general way to plot regression result to file, also adding batch number
    
    plt <- reg_res$plts[[plt_name]]

    
    ## plt_name <- deparse(substitute(plt)) %>% strsplit("$", fixed = T) %>% unlist() %>% tail(1)

    plt_cfg <- reg_res$plt_cfgs[[plt_name]]

    plt_filename <- paste0(FIG_DIR, "plt_", batch_version, "_", plt_cfg$filename)
    
    pdf(plt_filename,  width = plt_cfg$width, height = plt_cfg$height)
    plot(plt)
    dev.off()
    
}

cpr_vrsns <- function(reg_res_vsns) {
    #' compare two versions of regression results
    #' take best model per cbn, regcmd, vrbl_choice

    #' not fully functionalized: want to add this layer of abstraction (versions) as little as possible
    #' atm still relies on v48 and v49, and has hard-coded plot call
    
    ## get best models
    dt_cpr_vsns <- imap_dfr(reg_res_vsns, ~ .x$reg_res_objs$gof_df_cbn %>%
                                              filter(gof_names == "log_likelihood") %>%
                                              mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>% 
                                              group_by(cbn_name, regcmd, vrbl_choice) %>% 
                                              slice_max(gof_value, with_ties = F) %>%
                                              select(cbn_name, regcmd, gof_value) %>%
                                              mutate(source = .y))
    ## some shitty plot
    dt_cpr_vsns %>% 
        ggplot(aes(x = gof_value, y=source)) +
        geom_point() + 
        facet_wrap(~ regcmd + cbn_name, scales = "free")
        
    ## cast best gofs wide -> calculate goff diff
    dt_cpr_vsns_wide <- adt(dt_cpr_vsns) %>%
        dcast.data.table(vrbl_choice + cbn_name + regcmd ~ source , value.var = "gof_value") %>%
        .[, gof_diff := v49- v48]

    ## dt_cpr_vsns_wide$gof_diff %>% hist(breaks = 40)


    plt_gof_cpr_vsn <- ggplot(dt_cpr_vsns_wide, aes(x=gof_diff)) +
        geom_histogram(aes(y=..density..), binwidth = 0.2, color = "black", fill = "grey") +
        xlim(c(0, 7)) + 
        scale_x_continuous(breaks = seq(-1,7, 0.5)) +
        geom_vline(xintercept = 0, linetype = "dashed")

    pdf(paste0(FIG_DIR, "plt_vsns_cprsn.pdf"), height = 4, width = 7)
    plot(plt_gof_cpr_vsn)
    dev.off()

}


gen_reg_res <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    NBR_MDLS <- 3
    ## fldr_info <- fldr_info_optmz
    reg_anls_base <- read_reg_res_files(fldr_info)
    reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs, NBR_MDLS)

    reg_res <- list()

    ## generate plots, construct configs
    reg_res$plts <- gen_reg_res_plts(reg_res_objs, vvs, NBR_MDLS)
    reg_res$plt_cfgs <- gen_plt_cfgs()

    reg_res$reg_res_objs <- reg_res_objs

    return(reg_res)
}



stop("functions done")

## ** main analysis
NBR_MDLS <- 3
## fldr_info <- fldr_info_optmz
reg_anls_base <- read_reg_res_files(setup_regression_folders_and_files("v60"))
reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs, NBR_MDLS)

reg_res <- list()

## generate plots, construct configs
reg_res$plts <- gen_reg_res_plts(reg_res_objs, vvs, NBR_MDLS)
reg_res$plt_cfgs <- gen_plt_cfgs()

## render all plots to file
## lapply(names(reg_res$plts), \(x) render_reg_res(x, fldr_info))


## ** version comparison 
## read in stuff, construct objects

reg_res_v48 <- gen_reg_res(setup_regression_folders_and_files("v48"))
reg_res_v49 <- gen_reg_res(setup_regression_folders_and_files("v49"))

setdiff(names(reg_res_v48$plt_cfgs), names(reg_res_v48$plts))

## reg_res_v48$plt_cfgs <- gen_plt_cfgs()
## reg_res_v48$plts <- gen_reg_res_plts(reg_res_v48$reg_res_objs, vvs, NBR_MDLS)
## render_reg_
render_all_reg_res_plts(reg_res_v48, "v48")
render_all_reg_res_plts(reg_res_v49, "v49")


reg_res_vsns <- list(v48 = reg_res_v48, v49=reg_res_v49)
cpr_vrsns(reg_res_vsns)



## filter(reg_res_objs$gof_df_cbn, gof_names == "log_likelihood") %>% 




## reg_anls_base$df_reg_anls_cfgs_wide$loop_nbr %>% table()


## render_reg_res(reg_res$plts$cbn_log_likelihoods, fldr_info)
## render_reg_res(reg_res$plts$best_models_condensed, fldr_info)
## ** evaluate model convergence consistency

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    group_by(regcmd, cbn_name, lag_spec) %>%
    summarize(nbr_mdls = len(mdl_id), nbr_unq_gof = n_distinct(gof_value)) %>%
    pull(nbr_unq_gof) %>% table()

## ** evaluate possible savings of better model caching 

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>% 
    group_by(regcmd, cbn_name, vrbl_choice) %>%
    summarize(nbr_mdls = len(vrbl_choice), nbr_unq_mdls = n_distinct(lag_spec)) %>%
    ungroup() %>%
    summarize(sum(nbr_mdls), sum(nbr_unq_mdls))





   
## ** step-wise optimization starts here 

plot_stacker <- function(dfx, ystack, xstack, shape_clm = NULL, color_clm="lag") {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' stack coef results vertically
    #' assumes: lag (points get colored by), coef, min, max, sig, vrbl_name_unlag

    ## base_aes <- aes(y=!!sym(ystack))

    ## programmatically edit the aes: 
    ## modify color and shape based on function input
    
    point_aes <- aes(x=coef)

    if (!is.null(shape_clm)) {
        point_aes <- c(point_aes, aes(shape = !!sym(shape_clm)))
        class(point_aes) <- "uneval"
    }

    if (!is.null(color_clm)) {
        point_aes <- c(point_aes, aes(color = !!sym(color_clm)))
        class(point_aes) <- "uneval"
    }
    
    ## setdiff(unique(dfx$vrbl_name_unlag), names(vvs$vrbl_lbls))

    dfx$vrbl_name_unlag <- factor(dfx$vrbl_name_unlag, levels = names(vvs$vrbl_lbls))

    ## actual plotting 
    ggplot(dfx, aes(y=get(ystack))) + 
        geom_errorbarh(aes(xmin = min, xmax = max, height= 0.2, linetype = factor(sig), size = factor(sig)),
                       alpha = 0.8, show.legend = T)  +
        geom_point(point_aes, size = 2.5,  show.legend = T) +
        facet_grid(vrbl_name_unlag ~ get(xstack), switch = "y", 
                   labeller = labeller(vrbl_name_unlag = rev(vvs$vrbl_lbls))) +
        theme(strip.text.y.left = element_text(angle = 0),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_linetype_manual(values = c(2, 1)) + # setting errorbar linetype
        scale_size_manual(values=c(0.4, 0.7)) + ## setting errorbar size
        theme(panel.spacing.y = unit(0.1, "lines"),
              legend.position = "bottom",
              legend.justification = "left",
              panel.background = element_rect(fill = NA, color = "black")) +
        guides(shape = guide_legend(order=1, label.position = "right", direction = "vertical"),
               color = guide_colorbar(order=2),
               size = "none", ## remove the supid sig() legend
               linetype = "none") +
        labs(y=ystack)
    
}

## *** optimized functions end here 

reg_anls_base_optmz <- read_reg_res_files(fldr_info_cvrg)
reg_anls_base_optmz <- read_reg_res_files(fldr_info_optmz)

## best result per base_spec after each loop of optimization
## uglyl af -> uncomment
## filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>%
##     group_by(loop_nbr, base_lag_spec, cbn_name) %>%
##     slice_max(gof_value) %>% 
##     ggplot(aes(x=loop_nbr, y=gof_value, group = interaction(base_lag_spec, cbn_name))) +
##     geom_line() +
##     facet_wrap(~cbn_name, ncol = 1, scales = "free_y")





## comparison 
## filter(reg_anls_base$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()
## filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()










## see if different starting coefs of same vrbl_choice lead to same results
best_mdls_optmzd <- filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% 
    ## select(mdl_id, gof_value, base_lag_spec, loop_nbr, vrbl_optmzd, cbn_name) %>%
    select(mdl_id, gof_value, base_lag_spec, loop_nbr, vrbl_optmzd, cbn_name, technique_str, difficulty, regcmd) %>%
    mutate(step_base = 1, loop_nbr = as.numeric(loop_nbr),
           vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>%
    ## group_by(cbn_name, vrbl_choice, base_lag_spec) %>%
    group_by(cbn_name, vrbl_choice, base_lag_spec, technique_str, difficulty, regcmd) %>%
    slice_max(gof_value, n=1) %>% 
    slice_sample(n=1)





## reg_anls_base_optmz$gof_df_cbn$base_lag_spec %>% unique()



df_anls_base_optmzd <- add_coef_sig(reg_anls_base_optmz$coef_df, reg_anls_base_optmz$df_reg_anls_cfgs_wide)
## construct_df_best_mdls(reg_anls_base_optmz, reg_anls_base_optmz$gof_df_cbn)

best_mdls_optmzd_coefs <- merge(df_anls_base_optmzd, best_mdls_optmzd) %>% atb() %>%
    mutate(min = coef - 1.96*se, max = coef + 1.96*se) %>% 
    filter(vrbl_name_unlag %!in% c("ln_s", "cons", "ln_r", "alpha", "intcpt_var", "(Intercept)"))
    

## condense the ystack to be able to save vertical space when expanding the xstack
best_mdls_optmzd_coefs <- best_mdls_optmzd_coefs %>%
    group_by(cbn_name, vrbl_name_unlag, vrbl_choice) %>%
    mutate(vrbl_choice_factor = row_number(), ## when using xstack=vrbl_choice
           just_one = factor(1), ## when using xstac=base_lag_spec
           tec_base_interact = interaction(technique_str, base_lag_spec), ## spread out the multiple versions 
           tec_diffl_interact = interaction(technique_str, difficulty))

## most straightforward way to see if different variable choices lead to different coefs/lags
## coefs/lags are pretty much the same, but handful of marginally significant coefs :/
plot_stacker(best_mdls_optmzd_coefs, ystack = "base_lag_spec", xstack = "cbn_name",
             shape_clm = "vrbl_choice", color_clm = "lag")

## want those from different combinations on top of each other: compare 
plot_stacker(best_mdls_optmzd_coefs, xstack = "vrbl_choice", ystack = "cbn_name",
             shape_clm = "cbn_name", color_clm = "lag")


## even more effective way to show that reg_specs with same variable choice converge to same results
## these 2 only make sense for having only one combination: would need more detailed ystacking
plot_stacker(best_mdls_optmzd_coefs, ystack = "vrbl_choice_factor", xstack = "vrbl_choice",
             shape_clm = "cbn_name", color_clm = "lag")


## similar to first coef visualization (one model per column)
plot_stacker(best_mdls_optmzd_coefs, ystack = "just_one", xstack = "base_lag_spec",
             shape_clm = "vrbl_choice", color_clm = "lag")


## **** convergence tests
## compare difficulty within technique strs 
plot_stacker(best_mdls_optmzd_coefs, xstack = "technique_str", ystack = "cbn_name",
             shape_clm = "cbn_name", color_clm = "lag")

## compare techniques within difficulty 
plot_stacker(best_mdls_optmzd_coefs, ystack = "technique_str", xstack = "difficulty",
             shape_clm = "technique_str", color_clm = "lag")

## compare technique within difficulty, with different runs unstacked
plot_stacker(best_mdls_optmzd_coefs, ystack = "tec_base_interact", xstack = "difficulty",
             shape_clm = "technique_str", color_clm = "lag")

## compare difficulty within tec_base_interact
plot_stacker(best_mdls_optmzd_coefs, xstack = "tec_base_interact", ystack = "cbn_name",
             shape_clm = "technique_str", color_clm = "lag")

plot_stacker(best_mdls_optmzd_coefs, xstack = "cbn_name", ystack = "regcmd",
             shape_clm = "vrbl_choice", color_clm = "lag")




## "formal" test: all gof_values of best-fitting models are the same 
best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str, regcmd) %>%
    slice_max(gof_value) %>%
    select(vrbl_name_unlag, difficulty, technique_str, gof_value) %>%
    ## pull(technique_strs) %>%
    pull(gof_value) %>%
    n_distinct()



## **** non-identical convergence



## see how coefs/lags differ between different base_lag_specs
base_lag_spec_cprn_df <- best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str) %>%
    select(vrbl_name_unlag, difficulty, technique_str, coef, gof_value, base_lag_spec, lag) %>%
    mutate(base_lag_spec_fctr = as.numeric(as.factor(base_lag_spec))) %>% 
    pivot_wider(id_cols = c(vrbl_name_unlag, difficulty, technique_str), names_from = base_lag_spec_fctr,
                values_from = c(coef, gof_value, lag)) %>% 
    mutate(diff_coef = coef_1 - coef_2,
           diff_lag = lag_1 - lag_2) 

base_lag_spec_cprn_df %>% 
    ggplot(aes(x=diff_coef)) +
    ## ggplot(aes(x=diff_lag)) +
    geom_histogram(bins = 100)

base_lag_spec_cprn_df %>%
    filter(diff_coef !=0) %>%
    ## pull(vrbl_name_unlag) %>%
    ## pull(technique_str) %>%
    pull(difficulty) %>%
    table()


## pull(diff) %>% table()
## pull(diff) %>% hist(breaks = 50)
## huh this looks like quite some different values
## well they don't look so different when you plot them, also in partly due to small differences being dwarved by the few huge coefs 

## compare base_lag_spec within tec_diffl_interact
plot_stacker(best_mdls_optmzd_coefs, xstack = "tec_diffl_interact", ystack = "base_lag_spec",
             shape_clm = "base_lag_spec", color_clm = "lag")


## should also do lag test comparison, there seems to be some difference
filter(base_lag_spec_cprn_df, diff_lag != 0) %>%
    select(vrbl_name_unlag, difficulty, technique_str, lag_1, lag_2) %>% 
    ## pull(vrbl_name_unlag) %>%
    ## pull(technique_str) %>%
    pull(difficulty) %>%
    table()


## difference between techniques: which is closest to nr

base_lag_spec_cprn_df2 <- best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str) %>%
    select(vrbl_name_unlag, difficulty, technique_str, coef, gof_value, base_lag_spec, lag)


merge(filter(base_lag_spec_cprn_df2, technique_str == "nr") %>%
      select(vrbl_name_unlag, difficulty, base_lag_spec, technique_str_nr = technique_str, coef_nr = coef),
      filter(base_lag_spec_cprn_df2, technique_str != "nr")) %>% atb() %>%
    mutate(coef_diff = abs(coef_nr - coef)) %>%
    ggplot(aes(x=coef_diff)) +
    geom_histogram() +
    facet_wrap(~technique_str)
    ## group_by(technique_str) %>% 
    ## summarize(coef_diff_mean = mean(coef_diff))

 

## **** lag test

filter(df_anls_base_optmzd, vrbl_name_unlag %in% c("ti_tmitr_interact", "tmitr_approx_linear20step")) %>%
    select(mdl_id, vrbl_name_unlag, lag, base_lag_spec) %>%
    pivot_wider(names_from = vrbl_name_unlag, values_from = lag) %>%
    mutate(lag_same = ti_tmitr_interact == tmitr_approx_linear20step) %>%
    ## head(100) %>% adf()
    pull(lag_same) %>% table()
    ## arrange(base_lag_spec) %>% 
    ## filter(!lag_same) %>% adf()


filter(df_anls_base_optmzd, vrbl_name_unlag %in% c("ti_tmitr_interact", "tmitr_approx_linear20step")) %>%
    select(mdl_id, vrbl_name_unlag, lag, base_lag_spec) %>%
    ## filter(vrbl_name_unlag == "tmitr_approx_linear20step") %>%
    ## filter(vrbl_name_unlag == "ti_tmitr_interact") %>%
    group_by(base_lag_spec) %>%
    summarize(distinct_lags = n_distinct(lag))

## squared test
## dfx <- tibble(a = rnorm(1000), b=rnorm(1000))
## t1 <- lm(a ~ b, dfx)
## t2 <- lm(a ~ b + I(b^2), dfx)
## screenreg(list(t1, t2))




## doesn't run: vrbl_varied: is not provided, means that within-changes don't make sense
## hopefully can stitch together other funcs tho 
## reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs)




## ** within base-spec changes




## LAZILY just copying 
## df_anls_within_prep2 <- df_anls_within_prep

## unique(df_anls_within$vrbl_name_unlag)

## just rbind the time-invariant values there 





## shouldn't group by base_lag_spec when selecting
## see if some aux vars can be constructed to select on 



        
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


    ## c(ti_vars, density_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars,
    ##   cult_spending_vars, ctrl_vars_lngtd))



## ** best fitting models




## *** condensed

generate_plot_models <- function(cbn_namex) {
    #' generate texreg models of best models?

    mdl_summary <- best_mdl_coefs %>% group_by(vrbl_name_unlag) %>%
        filter(cbn_name == cbn_namex) %>% 
        summarize(coef = mean(coef), lag_mean = mean(lag), lag_sd = sd(lag), p_value = mean(pvalues),
                  t_value = mean(t_value), se = mean(se))
    ## reg_res <- lm(mpg ~ cyl + disp, mtcars)
    ## plotreg(reg_res)

    texreg_mdl <- createTexreg(coef.names = as.character(mdl_summary$vrbl_name_unlag),
                      coef = mdl_summary$coef,
                      se = mdl_summary$se,
                      pvalues = mdl_summary$p_value)

    return(texreg_mdl)
}

x <- lapply(names(cbn_dfs)[1:3], generate_plot_models)

y <- plotreg(x, type = "facet")

## *** manual plotreg





    



createTexreg(coef.names = mdl_summary$vrbl_name_unlag, coef = )


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


    



    



