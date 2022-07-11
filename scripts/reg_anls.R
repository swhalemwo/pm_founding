## * header
## ** functions 

library(stringr)
library(ggbeeswarm)

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
        pivot_wider(id_cols = c(mdl_id, lag_spec), names_from = variable, values_from = value)


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


construct_df_anls_within_prep <- function(df_anls_base) {
    #' construct the within-base-spec changes:
    #' only get the coefs of variables where the variable is varied (within base-spec changes)
    #' only get coefs where all 5 variations converged
    
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

    return(df_anls_within_prep)

}




construct_time_invariant_coefs <- function(df_anls_base, vvs) {
    #' grab some coefs of cross-sectional variables, to be rbinded with the longitudinal coefs
    
    df_anls_time_invariant <- df_anls_base %>%
        filter(vrbl_name %in% vvs$crscn_vars) %>%
        group_by(cbn_name, vrbl_name) %>%
        slice_sample(n=15) %>%
        mutate(base_lag_spec_id = 1,
               lag = 1,
               nbr_mdls_cvrgd = 1)

    return(df_anls_time_invariant)
}

construct_df_anls_within <- function(df_anls_base, vvs, NBR_MDLS) {
    #' construct the dataset of the within lag-spec changes

    df_anls_within_prep <- construct_df_anls_within_prep(df_anls_base)


    ## filter out a number of models per lag (and cbn)
    df_anls_within_prep2 <- df_anls_within_prep %>%
        group_by(cbn_name, vrbl_name_unlag) %>%
        filter(base_lag_spec_id %in% sample(unique(base_lag_spec_id), NBR_MDLS))

    
    df_anls_time_invariant <- construct_time_invariant_coefs(df_anls_base, vvs)

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
    #' construct the data of the best fitting models per combination
    

    ## filter out HNWI 1B for now 
    df_anls_base_no1B <- df_anls_base %>% group_by(mdl_id) %>%
        filter("hnwi_nbr_1B" %!in% vrbl_name_unlag)

    ## select best model per combination
    best_mdls <- gof_df_cbn %>%
        filter(gof_names == "log_likelihood", cbn_name != "cbn_controls",
               mdl_id %in% unique(df_anls_base_no1B$mdl_id)) %>%
        group_by(cbn_name) %>% 
        arrange(gof_value) %>%
        slice_tail(n=1)
    

    best_mdl_coefs <- merge(df_anls_base, best_mdls) %>% atb()
    
    ## best_mdl_coefs$lag <- as.numeric(substring(str_extract(best_mdl_coefs$vrbl_name, "_lag(\\d+)"), 5))
    ## best_mdl_coefs$lag[is.na(best_mdl_coefs$lag)] <- 0
    
    ## vrbl_levels <- c("sum_core", vvs$ti_vars, vvs$density_vars, vvs$hnwi_vars, vvs$inc_ineq_vars,
    ##                  vvs$weal_ineq_vars, vvs$cult_spending_vars, vvs$ctrl_vars_lngtd)
    
    ## other_var_names <- c(unique(best_mdl_coefs$vrbl_name_unlag)[unique(best_mdl_coefs$vrbl_name_unlag) %!in% vrbl_levels])
    ## levels = c(vrbl_levels, other_var_names))
    
    ## reordering the variables
    best_mdl_coefs$vrbl_name_unlag <- factor(best_mdl_coefs$vrbl_name_unlag,
                                             levels = names(vvs$vrbl_lbls)[names(vvs$vrbl_lbls) %in%
                                                                           unique(best_mdl_coefs$vrbl_name_unlag)])
    
    ## filter out stuff I don't need
    best_mdl_coefs <- filter(best_mdl_coefs, vrbl_name_unlag %!in% c("ln_s", "cons", "ln_r"))

    return(best_mdl_coefs)
}


construct_best_mdls_summary <- function(df_best_mdls) {
    #' summary variables for best models 
    
    mdl_summary <- df_best_mdls %>% group_by(vrbl_name_unlag, cbn_name) %>%
        summarize(coef = mean(coef), lag_mean = mean(lag), lag_sd = sd(lag), p_value = mean(pvalues),
                  t_value = mean(t_value), se = mean(se), min = coef - 1.96*se, max = coef + 1.96*se,
                  sig = ifelse(abs(t_value) > 1.96, 1,0))

    ## have to reverse order to make points look good 
    mdl_summary$vrbl_name_unlag <- factor(mdl_summary$vrbl_name_unlag,
                                          levels = rev(levels(mdl_summary$vrbl_name_unlag)))

    return(mdl_summary)
}


proc_reg_res_objs <- function(reg_anls_base, vvs) {
    #' further processing of the regression res objects, no reading-in here

    
    coef_df <- reg_anls_base$coef_df
    df_reg_anls_cfgs_wide <- reg_anls_base$df_reg_anls_cfgs_wide
    gof_df_cbn <- reg_anls_base$gof_df_cbn

    ## merging significance to all coefs (maybe can be 
    df_anls_base <- add_coef_sig(coef_df, df_reg_anls_cfgs_wide)

    ## number of models to pick for the analyses
    NBR_MDLS <- 10
    
    
    df_anls_within <- construct_df_anls_within(df_anls_base, vvs, NBR_MDLS)
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
    #' generate plot of likelihoods

    ## ok makes sense: cbn_all models have best fit: most variables, least observations
    ## fit gets worse the more variables are removed and the more cases are added

   
    gof_df_cbn %>% 
        filter(gof_names == "log_likelihood") %>%
        ggplot(aes(x = gof_value, fill = cbn_name)) +
        geom_histogram(binwidth = 1)

}

gen_plt_reg_res_within <- function(df_anls_within, vvs) {

    ggplot(df_anls_within, aes(x=lag, y=coef, group = base_lag_spec)) +
        geom_line(show.legend = F, alpha = 0.15) +
        geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) + 
        facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y", 
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0)) +
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))
    
}

gen_plt_reg_res_all <- function(df_anls_all, vvs) {
    #' plot coefs from all models 


    ggplot(df_anls_all, aes(x=lag, y=coef)) +
        geom_quasirandom(aes(color = t_value, shape = factor(sig)), size = 2, height = 0, width = 0.3) +
        facet_grid(cols = vars(cbn_name), rows = vars(vrbl_name_unlag), scales = "free", switch = "y",
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0)) +
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))
}

gen_plt_best_mdls_wlag <- function(df_best_mdls, vvs) {
    #' generate plot of coefs of beste models with lag

    ggplot(df_best_mdls, aes(x=lag, y=exp(coef), color = t_value)) +
        geom_quasirandom(aes(shape = factor(sig)), height = 0, width = 0.33, show.legend=T, size = 3) +
        facet_grid(vrbl_name_unlag~cbn_name, scales="free", switch = "y",
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0)) + 
        scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
        scale_shape_manual(values = c(1,4))

}

gen_plt_mdl_summary <- function(mdl_summary, vvs) {
    #' generate the summary plot of the best models 
    
    ggplot(mdl_summary, aes(color = factor(sig), y = vrbl_name_unlag)) +
        geom_point(aes(x = coef, shape = factor(sig)), size = 2.5, alpha = 0.95, show.legend = F) +
        geom_errorbarh(aes(xmin = min, xmax = max, , height= 0.1), alpha = 0.6, show.legend = F) + 
        facet_wrap(~cbn_name) +
        geom_vline(xintercept =0, linetype = "dashed") +
        scale_shape_manual(values = c(15,16)) +
        ## scale_shape_manual(values = c(0,1)) +
        ## scale_shape_manual(values = c(21,12)) +
        scale_color_manual(values = c("#1C5BA6", "#BD0017")) +
        scale_y_discrete(labels = vvs$vrbl_lbls) +
        ## coord_cartesian(xlim=c(-1, 1)) +
        labs(x="coefficient size, 95% CI", y="coefficient")

}

gen_reg_res_plts <- function(reg_res_objs, vvs) {
    #' generate all the plots
    

    gof_df_cbn <- reg_res_objs$gof_df_cbn
    df_anls_within <- reg_res_objs$df_anls_within
    df_best_mdls <- reg_res_objs$df_best_mdls
    mdl_summary <- reg_res_objs$mdl_summary
    
    return(
        list(
            cbn_log_likelihoods = gen_plt_cbn_log_likelihoods(gof_df_cbn),
            reg_res_within = gen_plt_reg_res_within(df_anls_within, vvs),
            reg_res_all = gen_plt_reg_res_all(df_anls_within, vvs),
            best_models_wlag = gen_plt_best_mdls_wlag(df_best_mdls, vvs),
            best_models_condensed = gen_plt_mdl_summary(mdl_summary, vvs)
        )
    )
}


## reg_res$plts$cbn_log_likelihoods <- gen_plt_cbn_log_likelihoods(reg_anls_base$gof_df_cbn)


gen_plt_cfgs <- function() {
    #' generate the plot configs (manually specified)
    
    return(
        list(
        cbn_log_likelihoods = list(filename = "cbn_log_likelihoods.pdf", width = 6, height = 3),
        reg_res_within = list(filename = "reg_res_within.pdf", width = 10, height = 12),
        reg_res_all = list(filename = "reg_res_all.pdf", width = 8, height = 12),
        best_models_wlag = list(filename = "best_models_wlag.pdf", width = 8, height = 12),
        best_models_condensed = list(filename = "best_models_condensed.pdf", width = 7, height = 4)
        )
    )

}

plot_reg_res <- function(plt_name, fldr_info) {
    #' general way to plot regression result to file, also adding batch number
    
    plt <- reg_res$plts[[plt_name]]
    ## plt_name <- deparse(substitute(plt)) %>% strsplit("$", fixed = T) %>% unlist() %>% tail(1)

    plt_cfg <- reg_res$plt_cfgs[[plt_name]]

    plt_filename <- paste0(FIG_DIR, fldr_info$batch_version, "_", plt_cfg$filename)
    
    pdf(plt_filename,  width = plt_cfg$width, height = plt_cfg$height)
    plot(plt)
    dev.off()
    
}




## read in stuff, construct objects 
reg_anls_base <- read_reg_res_files(fldr_info)
reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs)


reg_res <- list()

## generate plots, construct configs
reg_res$plts <- gen_reg_res_plts(reg_res_objs, vvs)
reg_res$plt_cfgs <- gen_plt_cfgs()

## render all plots to file
lapply(names(reg_res$plts), \(x) plot_reg_res(x, fldr_info))


## plot_reg_res(reg_res$plts$cbn_log_likelihoods, fldr_info)
## plot_reg_res(reg_res$plts$best_models_condensed, fldr_info)
   
## ** step-wise optimization starts here 


reg_anls_base_optmz <- read_reg_res_files(fldr_info_optmz)

## best result per base_spec after each loop of optimization
filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>%
    group_by(loop_nbr, base_lag_spec) %>%
    slice_max(gof_value) %>% 
    ggplot(aes(x=loop_nbr, y=gof_value, group = base_lag_spec)) +
    geom_line()



## comparison 
## filter(reg_anls_base$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()
## filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()



## progress after each variable
## variables are randomly chosen, so step is different for each base_lag_spec
filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% 
    select(gof_value, base_lag_spec, loop_nbr, vrbl_optmzd) %>%
    mutate(step_base = 1, loop_nbr = as.numeric(loop_nbr),
           vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>%
    group_by(vrbl_choice, base_lag_spec, loop_nbr, vrbl_optmzd) %>%
    slice_max(gof_value) %>% 
    group_by(base_lag_spec) %>% 
    arrange(gof_value) %>%
    mutate(step = ave(step_base, FUN = cumsum)) %>% 
    ggplot(aes(x=step, y=gof_value, group = base_lag_spec, color = vrbl_choice)) +
    geom_line()


## see if different starting coefs of same vrbl_choice lead to same results
best_mdls_optmzd <- filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% 
    select(mdl_id, gof_value, base_lag_spec, loop_nbr, vrbl_optmzd) %>%
    mutate(step_base = 1, loop_nbr = as.numeric(loop_nbr),
           vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>%
    group_by(vrbl_choice, base_lag_spec) %>%
    slice_max(gof_value, n=1) %>% 
    slice_sample(n=1)





## reg_anls_base_optmz$gof_df_cbn$base_lag_spec %>% unique()


plot_stacker <- function(dfx, ystack, xstack, shape_clm = NULL, color_clm="lag") {
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
    
    ## actual plotting 
    ggplot(dfx, aes(y=get(ystack))) + 
        geom_errorbarh(aes(xmin = min, xmax = max, height= 0.2, linetype = factor(sig), size = factor(sig)),
                       alpha = 0.8, show.legend = F)  +
        geom_point(point_aes, size = 2.5,  show.legend = T) +
        facet_grid(vrbl_name_unlag~get(xstack), switch = "y",
                   labeller = labeller(vrbl_name_unlag = vvs$vrbl_lbls)) +
        theme(strip.text.y.left = element_text(angle = 0),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_linetype_manual(values = c(2, 1)) + # setting errorbar linetype
        scale_size_manual(values=c(0.4, 0.7)) + ## setting errorbar size
        theme(panel.spacing.y = unit(0.3, "lines"))
    
    
}

df_anls_base_optmzd <- add_coef_sig(reg_anls_base_optmz$coef_df, reg_anls_base_optmz$df_reg_anls_cfgs_wide)
## construct_df_best_mdls(reg_anls_base_optmz, reg_anls_base_optmz$gof_df_cbn)

best_mdls_optmzd_coefs <- merge(df_anls_base_optmzd, best_mdls_optmzd) %>% atb() %>%
    mutate(min = coef - 1.96*se, max = coef + 1.96*se) %>% 
    filter(vrbl_name_unlag %!in% c("ln_s", "cons", "ln_r"))
    

## condense the ystack to be able to save vertical space when expanding the xstack
best_mdls_optmzd_coefs <- best_mdls_optmzd_coefs %>%
    group_by(cbn_name, vrbl_name_unlag, vrbl_choice) %>%
    mutate(vrbl_choice_factor = row_number(), ## when using xstack=vrbl_choice
           just_one = factor(1)) ## when using xstac=base_lag_spec


## most straightforward way to see if different variable choices lead to different coefs/lags
## coefs/lags are pretty much the same, but handful of marginally significant coefs :/
plot_stacker(best_mdls_optmzd_coefs, ystack = "base_lag_spec", xstack = "cbn_name",
             shape_clm = "vrbl_choice", color_clm = "lag")

## even more effective way to show that reg_specs with same variable choice converge to same results
plot_stacker(best_mdls_optmzd_coefs, ystack = "vrbl_choice_factor", xstack = "vrbl_choice",
             shape_clm = "vrbl_choice", color_clm = "lag")

## similar to first coef visualization (one model per column), but seems like the height of geom_errorbarh doesn't play well together with facet_grid
## column has to be factor 
plot_stacker(best_mdls_optmzd_coefs, ystack = "just_one", xstack = "base_lag_spec",
             shape_clm = "vrbl_choice", color_clm = "lag")







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





    



createTexreg(coef.names = mdl_summary$vrbl_name_unlag, coef = 


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


    



    



