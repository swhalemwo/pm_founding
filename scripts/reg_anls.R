## * header
## ** functions 

library(stringr)
library(ggbeeswarm)
library(patchwork)
library(ggridges)

library(sf)
library(stars)









read_reg_anls_files <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' reads reg_res_files, also adds one-out files 
    #' having wrapper means read_reg_res_files can stay the same

    reg_anls_base_objs <- read_reg_res_files(fldr_info)


    ## setup and read-in ou results
    fldr_info_ou <- setup_regression_folders_and_files(batch_version = paste0(fldr_info$batch_version, "ou"),
                                                       batch_dir_addgn = paste0(fldr_info$batch_version, "/ou/"))

    reg_anls_ou_objs <- read_reg_res_files_ou(fldr_info_ou)

    dt_vif_res <- fread(paste0(fldr_info$BATCH_DIR, "VIF_res.csv"))

    dt_cntrfctl_cons <- fread(paste0(fldr_info$BATCH_DIR, "cntrfctl_cons.csv"))

    ## read value ~ year data back in 
    dt_velp_scalars <- fread(paste0(fldr_info$BATCH_DIR, "dt_velp_scalars.csv"))
    dt_velp_crycoefs <- fread(paste0(fldr_info$BATCH_DIR, "dt_velp_crycoefs.csv"))

    dt_cntrfctl_wse <- fread(paste0(fldr_info$BATCH_DIR, "cntrfctl_wse.csv"))

    return(c(reg_anls_base_objs,
             list(ou_objs = reg_anls_ou_objs),
             list(dt_vif_res = dt_vif_res,
                  dt_cntrfctl_cons = dt_cntrfctl_cons,
                  dt_cntrfctl_wse = dt_cntrfctl_wse,
                  dt_velp_scalars = dt_velp_scalars,
                  dt_velp_crycoefs = dt_velp_crycoefs)))
    
   
}

proc_ou_files <- function(regres_ou_files, gof_df_cbn, top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' process the one-out files 

    ## some (25) just don't converge in v75 
    ## regres_ou_files$df_reg_anls_cfgs_wide %>% adt() %>% .[, .N, cvrgd]
    ## regres_ou_files$df_reg_anls_cfgs_wide %>% adt() %>% .[cvrgd == 0] %>% adf()

    ## ## compare with previous results
    
    dt_ou_base <- regres_ou_files$gof_df_cbn %>% adt() %>%
        .[gof_names %in% c("log_likelihood", "df"),
          .(ou_set_title, gof_name = gof_names, gof_value_ou = gof_value, mdl_id_ou = mdl_id,  nonou_id)] %>%
        .[, gof_name := paste0(gof_name, "_ou")] %>% 
        dcast.data.table(ou_set_title + mdl_id_ou + nonou_id ~ gof_name, value.var = "gof_value_ou") %>%
        .[, df_ou := as.integer(df_ou)]

    ## get the original models LL/df
    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)


    check_df_name_unqns(list(dt_ou_base, mdl_id_dt), skip_var_names = c())

    ## combine one-out results with full models
    ou_anls <- copy(mdl_id_dt)[dt_ou_base, on = .(mdl_id = nonou_id)] %>% # str() %>%
        .[, ou_set_title_unlag := gsub("_lag[1-5]", "", ou_set_title)] %>% # remove lag info
        .[, `:=`(log_likelihood_diff = log_likelihood - log_likelihood_ou,
                 df_diff = df - df_ou)] %>% # calc diffs in LL and df 
        .[, llrt_vlu := -2 * (log_likelihood_ou - log_likelihood)] %>% # start with LLRT
        ##  FIXME: due to changed data some LLRT_vlus are negative
        .[, llrt_p := pchisq(llrt_vlu, df = df_diff, lower.tail = F)] %>% # LLRT p-value
        .[, `:=`(sig = llrt_p < 0.05,
                 z = qnorm(llrt_p/2, lower.tail = F))] %>% # just plug chi2-based p-values into normal dist
        copy(vvs$hyp_mep_dt)[., on =.(vrbl = ou_set_title_unlag)] %>% # add hypothesis coding
        .[, vrbl := factor(vrbl, levels = levels(vvs$hyp_mep_dt$vrbl))] %>% 
        .[, ou_set_title_unlag := vrbl] ## keep ou_set_title_unlag

    
    ## get proporition significant improvement when added to model when variable added
    dt_llrt_improv_prop <- ou_anls %>% copy() %>%
        .[, .(prop_sig = sum(sig)/.N), by = .(vrbl, cbn_name)]

    ## ## investigate number of coefs that make model better when dropped
    ## hist(dt_llrt_improv_prop$prop_sig)
    ## dt_llrt_improv_prop %>% copy() %>%
    ##     .[, prop_sig_bin := round(prop_sig)] %>%
    ##     .[prop_sig != prop_sig_bin, prop_sig_bin := 0.5] %>% 
    ##     .[, .N, prop_sig_bin] %>% 
    ##     ggplot(aes(x=factor(prop_sig_bin), y = N)) +
    ##     geom_col()

    ## reg_res_ou$plts$plt_best_coefs_single
    ## top_coefs, colored by prop_sig (LLRT improvement)
    top_coefs_llrt <- top_coefs %>% copy() %>% 
        .[, .SD[which.max(log_likelihood)], by = .(vrbl_name_unlag, cbn_name)] %>%
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>%
        .[!is.na(hyp)] %>%
        .[dt_llrt_improv_prop, on = .(vrbl, cbn_name)]

    return(list(
        top_coefs_llrt = top_coefs_llrt,
        ou_anls = ou_anls))

}

gendt_oucoefchng <- function(ou_objs, df_anls_base) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## get dt_oucfg_wide for IDs
    dt_oucfg_wide <- adt(ou_objs$df_reg_anls_cfgs_wide)

    ## get one-out coefs
    dt_coefs_ou <- adt(ou_objs$coef_df) %>%
        .[, .(mdl_id_ou = mdl_id, vrbl_name_unlag = gsub("_lag[1-5]", "", vrbl_name), coef_ou = coef)] %>%
        ## need to nonou_id here
        .[dt_oucfg_wide[, .(mdl_id, nonou_id, ou_set_title)], on = .(mdl_id_ou = mdl_id)] %>%
        .[, ou_set_title := gsub("_lag[1-5]", "", ou_set_title)]
        

    ## get original coefs
    dt_coefs_nonou <- adt(df_anls_base) %>%
        ## only select columns needed here atm 
        .[, .(mdl_id, vrbl_name_unlag, coef, cbn_name)] %>%
        ## only need nonou_id here to filter to filter down to original coefs
        .[dt_oucfg_wide[, .(nonou_id = unique(nonou_id))], on = .(mdl_id = nonou_id)]

    ## combine original and one-out coefs
    dt_coef_cprn <- dt_coefs_nonou[dt_coefs_ou, on = .(mdl_id = nonou_id, vrbl_name_unlag)]

    ## some debuggin
    
    ## dt_coef_cprn[ou_set_title == "sptinc992j_p90p100" & vrbl_name_unlag == "shweal992j_p90p100" &
    ##              cbn_name == "cbn_all"]


    ## calculate diffs, merge hyps to filter out stuff, also filter out other stuff
    dt_coef_cprn_vis_prep <- dt_coef_cprn %>% copy() %>%
        .[, diff := coef - coef_ou] %>%
        ## .[cbn_name == "cbn_all"] %>%
        vvs$hyp_mep_dt[, .(vrbl, hyp_nonou = hyp)][.,  on = .(vrbl = vrbl_name_unlag)] %>%
        vvs$hyp_mep_dt[, .(ou_set_title = vrbl, hyp_ou = hyp)][., on = "ou_set_title"] %>%
        .[, `:=`(ou_set_title = factor(ou_set_title, levels = names(vvs$vrbl_lbls)),
                 vrbl = factor(vrbl, levels = rev(names(vvs$vrbl_lbls))))] %>%
        .[vrbl %!in% c("ln_s", "ln_r")] %>% ## yeet model stuff
        .[!(hyp_ou == "h1b" & hyp_nonou == "h1b")] %>% ## yeet h1 self-square (interactions)
        .[!gsub("_sqrd", "", ou_set_title) == vrbl] %>% ## yeet changes to main when adding sqrd (h2, sqrd controls)
        .[vrbl != "cons"] ## yeet intercept 


    ## check xtsum: if within variation-pair in diff is substantial
    ## dt_coef_cprn_vis_prep %>% copy() %>% .[, .(pair = paste0(vrbl, ou_set_title, cbn_name), diff)] %>%
    ##     ## .[, .N, pair]
    ##     xtsum(diff, pair) %>% adt()
        
        
    ## aggregate diffs
    dt_oucoefchng <- dt_coef_cprn_vis_prep %>%
        .[, .(mean_diff = mean(diff)), # diff10 = quantile(diff, 0.10), diff90 = quantile(diff, 0.90)),
          .(vrbl, cbn_name, ou_set_title, hyp_nonou, hyp_ou)]

    return(dt_oucoefchng)
}

## gendt_oucoefchng(reg_anls_base$ou_objs, reg_res_objs$df_anls_base)




gen_plt_oucoefchng_tile <- function(dt_oucoefchng) {
    #' plot of one-out coef changes, but as tiles
    
    colvec_stretched <- color_stretcher(tol9BuRd, midpt = 0,stretch = 0.2, sharp_edge = F,
                                        minpt = min(dt_oucoefchng$mean_diff),
                                        maxpt = max(dt_oucoefchng$mean_diff))


    dt_oucoefchng %>% 
        ## .[ou_set_title %in% c("NY.GDP.PCAP.CDk", "pm_density") & hyp_nonou == "h1b"] %>%
        ## .[hyp_nonou != "zcontrols"] %>% 
        ggplot(aes(y=vrbl, x=ou_set_title,
                   ## fill = mean_diff,
                   ## color = cbn_name,
                   group = cbn_name)) +
        geom_tile(mapping = aes(fill = mean_diff),
                  ## show.legend = T,
                  color = "black",
                  width = 0.9,
                  ## height = 0.9,
                  ## alpha = 0.5,
                  position = position_dodge(width = 0.9),
                  ## position = position_dodgev(height = 0.9),
                  ## show.legend = F
        ) +
        geom_point(mapping = aes(shape = cbn_name, y = vrbl),
                   position = position_dodge(width = 0.9),
                   size = 0.7,
                   stroke = 0.2
                           ## position = position_dodgev(height = 0.9)
                   ) + 
        facet_grid(hyp_nonou ~ hyp_ou, scales = "free", space = "free") +
        scale_fill_gradientn(colors = names(colvec_stretched), values = colvec_stretched) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              legend.position = "bottom") +
        ## scale_shape_manual(values = c(21,22,23), labels = vvs$cbn_lbls) +
        scale_shape_manual(values = c(3,2,1), labels = vvs$cbn_lbls) +
        ## theme_orgpop() + 
        theme(
            legend.box.margin = unit(c(0,0,0,0), "points"),
            strip.text = element_text(size = 6, margin = margin(2,2,2,2, "points")),
            plot.margin = unit(c(0,0,0,0), "points"),
            axis.text = element_text(size = 6),
            axis.title = element_text(size =6),
            legend.margin = margin(0,0,0,0),
            panel.grid = element_blank(),
            panel.background = element_blank()) +
        guides(shape = guide_legend(direction = "horizontal", nrow = 1, title = "Dataset",
                                    title.position = "top",
                                    override.aes = list(size = 3),
                                    order = 1, byrow = T,
                                    label.theme = element_text(size = 6),
                                    title.theme = element_text(size = 6)),
               fill = guide_colorbar(direction = "horizontal", title = "avg. coefficient difference",
                                     title.position = "top",
                                     barwidth = 7,
                                     barheight = 0.75,
                                     title.theme = element_text(size = 6),
                                     label.theme = element_text(size = 6))
               )
}

## gen_plt_oucoefchng_tile(reg_res_objs$dt_oucoefchng)


gen_plt_oucoefchng <- function(dt_oucoefchng) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate plot of how coef of variable changes when variable is removed

    

    colvec_stretched <- color_stretcher(tol9BuRd, midpt = 0,stretch = 0.2, sharp_edge = F,
                                        minpt = min(dt_oucoefchng$mean_diff),
                                        maxpt = max(dt_oucoefchng$mean_diff))
    
    ## default plot: point, uses color and size
    dt_oucoefchng %>%
        ggplot(aes(y=vrbl, x = ou_set_title, size = abs(mean_diff),
                   fill = mean_diff,
                   ## color = mean_diff,
                   group = cbn_name,
                   shape = cbn_name
                   )) +
        geom_point(
            # show.legend = F,
            ## shape = 21,
            stroke = 0.2,
            ## position = position_dodgev(height = 1)
            position = position_dodge(width = 0.85)
        ) +
        ## scale_color_gradient2(low = "blue", high = "red", na.value = "white", mid = "grey75") +
        ## scale_color_gradient2(low = tol9BuRd[1], high = tol9BuRd[9], na.value = "white", mid = "grey82") +
        scale_fill_gradientn(colors = names(colvec_stretched), values = colvec_stretched) +
        scale_color_gradientn(colors = names(colvec_stretched), values = colvec_stretched) + 
        ## theme_orgpop() + 
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              axis.text = element_text(size =6),
              strip.text = element_text(size = 6, margin = margin(2,2,2,2, "points")),
              ## panel.grid = element_line(color = "grey80", linetype = "dotted")
              ## panel.background = element_rect(fill = "grey80")
              legend.position = "bottom",
              legend.margin = margin(0,0,0,0),
              axis.title = element_text(size =6),
              legend.box.margin = unit(c(0,0,0,0), "points")
              ) +
        ## scale_shape_manual(values = c(21,22,23), labels = vvs$cbn_lbls) +
        scale_shape_manual(values = c(21,22, 21), labels = vvs$cbn_lbls) +
                facet_grid(hyp_nonou ~ hyp_ou, scales = "free", space = "free") +
        labs(y = "variable (with coefficient)", x = "variable/group of variables added") +
        scale_size_continuous(range = c(0.5,3.5)) +  # guide = guide_legend(title = "asdf")) +
        guides(shape = guide_legend(direction = "horizontal", nrow = 1,
                                    title = "Dataset (horizontal dataset order in plot follows legend order)",
                                    title.position = "top",
                                    override.aes = list(size = 4),
                                    order = 1, byrow = T,
                                    label.theme = element_text(size = 6),
                                    title.theme = element_text(size = 6)),
               fill = guide_colorbar(direction = "horizontal", title = "avg. coefficient difference",
                                     title.position = "top",
                                     barwidth = 7,
                                     barheight = 0.75,
                                     title.theme = element_text(size = 6), label.theme = element_text(size = 6)),
               size = guide_legend(direction = "horizontal", nrow = 1, title = "abs. avg. coefficient difference",
                                   byrow = T,
                                   title.theme = element_text(size = 6), label.theme = element_text(size = 6),
                                   title.position = "top"))
    
    


    ## tileplot
    ## ## think points better convey message
    ## dt_coef_cprn_vis %>%
    ##     ggplot(aes(y=vrbl, x = ou_set_title, color = mean_diff, fill = mean_diff)) +
    ##     geom_tile() + 
    ##     scale_color_gradient2(low = tol9BuRd[1], high = tol9BuRd[9], na.value = "white",
    ##                           mid = tol9BuRd[5]) +
    ##     scale_fill_gradient2(low = tol9BuRd[1], high = tol9BuRd[9], na.value = "white",
    ##                           mid = tol9BuRd[5]) + 
    ##     ## scale_fill_gradient2(low = "#0077b6", high = "#b60000", na.value = "white", mid = "grey90") +
    ##     theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    ##     facet_grid(hyp_nonou ~ hyp_ou, scales = "free", space = "free") +
    ##     labs(y = "variable (with coefficient)", x = "variable/group of variables added") +
    ##     theme_orgpop() +
    ##     theme(panel.grid.major = element_blank(), # yeet the grid
    ##           ## panel.background = element_rect(fill = "white")) +
    ##           panel.background = element_blank())
        

    
    ## dt_coef_cprn_vis[sign(diff25) != sign(diff75)] %$% hist(mean_diff)
    ## dt_coef_cprn_vis[sign(diff25) != sign(diff75) & abs(mean_diff) > 0.3]

    ## ## quantile data: explore variation
    ## dt_coef_cprn_vis_qnt <- dt_coef_cprn_vis %>% copy() %>% .[, mean_diff := NULL] %>% 
    ##     melt(id.vars = c("ou_set_title", "hyp_ou", "vrbl", "hyp_nonou"))

    
    ## ggplot(dt_coef_cprn_vis_qnt, # [hyp_ou == "h2" & hyp_nonou == "h1b"],
    ##        aes(y=vrbl, x = ou_set_title, size = abs(value), color = value, shape = variable, group = variable)) + 
    ##     geom_point(# shape = 21,
    ##                ## position = "dodge"
    ##                position = position_dodgev(height = 1) # preserve = "total", padding = 0.3)
    ##                ## position = position_nudge(x=0.1)
    ##                ) +
    ##     ## geom_tile() + 
    ##     scale_color_gradient2(low = "blue", high = "red", na.value = "white", mid = "grey75") +
    ##     scale_fill_gradient2(low = "blue", high = "red", na.value = "white", mid = "grey75") +
    ##     theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    ##     facet_grid(hyp_nonou ~ hyp_ou, scales = "free", space = "free") +
    ##     labs(y = "variable (with coefficient)", x = "variable/group of variables added")
        

}

## gen_plt_oucoefchng(reg_anls_base$ou_objs, reg_res_objs$df_anls_base)

gen_plt_cntrfctl <- function(dt_cntrfctl_cons, dt_cntrfctl_wse) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## ## violin of pred_2k_prop
    ## dt_cntrfctl_res %>% copy() %>% .[, vrbl := gsub("_lag[1-5]", "", vrblx)] %>%
    ##     vvs$hyp_mep_dt[., on = "vrbl"] %>%
    ##     .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>%
    ##     ggplot(aes(x=pred_2k_prop, y = vrbl)) +
    ##     ## geom_point() +
    ##     geom_violin() + # bw = 0.05) + 
    ##     facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
    ##     geom_vline(mapping = aes(xintercept = 1), linetype = "dashed")
    
    


    ## ## violin of minusone
    ## dt_cntrfctl_res %>% copy() %>% .[, vrbl := gsub("_lag[1-5]", "", vrblx)] %>%
    ##     .[, diff := pred_minusone - nbr_opened_2k] %>% # effect of not being 1 SD lower
    ##     vvs$hyp_mep_dt[., on = "vrbl"] %>%
    ##     .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>% 
    ##     ggplot(aes(x=diff, y = vrbl)) +
    ##     geom_violin() + # bw = 5) + 
    ##     facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
    ##     geom_vline(mapping = aes(xintercept = 0), linetype = "dashed") +
    ##     labs(x="change in number of PM foundings if variable was 1 SD higher")

    ## ## reproducing "coefs" from predicted change
    ## dt_cntrfctl_res[, .(coef = log(mean(pred_minusone)/mean(pred_2k_N))),
    ##                 by = .(vrbl = gsub("_lag[1-5]", "", vrblx), cbn_name)] %>%
    ##     vvs$hyp_mep_dt[., on = "vrbl"] %>%
    ##     dcast.data.table(vrbl + hyp ~ cbn_name, value.var = "coef") %>%
    ##     .[order(hyp)]
    
        


    ## ## violin of difference between total opened observed and predicted
    ## dt_cntrfctl_res %>% copy() %>% .[, vrbl := gsub("_lag[1-5]", "", vrblx)] %>%
    ##     .[, diff := nbr_opened_2k - pred_2k_N] %>% # effect of not staying constant since 2000
    ##     vvs$hyp_mep_dt[., on = "vrbl"] %>%
    ##     .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>% 
    ##     ggplot(aes(x=diff, y = vrbl)) +
    ##     ## geom_point() +
    ##     geom_violin(bw = 5) + 
    ##     facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
    ##     geom_vline(mapping = aes(xintercept = 0), linetype = "dashed") +
    ##     labs(x="additional PM foundings due to variable change since 2000")
        

    ## ## violin of difference between total opened observed and predicted
    dt_cntrfctl_cons %>% copy() %>% .[, vrbl := gsub("_lag[1-5]", "", vrbl)] %>%
        .[, diff := nbr_opened - pred] %>% # effect of not staying constant since 2000
        vvs$hyp_mep_dt[., on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>%
        .[dt_id %in% c("2k4")] %>% 
        ggplot(aes(x=diff, y = vrbl, color = dt_id)) +
        geom_point() +
        ## geom_violin() + # bw = 5) + 
        facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
        geom_vline(mapping = aes(xintercept = 0), linetype = "dashed") +
        labs(x="additional PM foundings due to variable change since 2000")
        
    
    ## ## distribution of gini of distribution of difference between country-level observed and predicted
    ## dt_cntrfctl_res %>% copy() %>% .[, vrbl := gsub("_lag[1-5]", "", vrbl)] %>%
    ##     vvs$hyp_mep_dt[., on = "vrbl"] %>%
    ##     .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>%
    ##     .[dt_id != "minusone"] %>% 
    ##     ggplot(aes(x=gini, y = vrbl, fill = dt_id)) +
    ##     ## geom_point() +
    ##     geom_violin(bw = 0.02) + 
    ##     facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
    ##     geom_vline(mapping = aes(xintercept = 0), linetype = "dashed")

    
    ## simulate some predictions based on since 2k constance, CY-cbn-model-dataset predictions
    dx <- dt_cntrfctl_wse %>% copy() %>% .[, rid := 1:.N] %>% 
        ## .[, paste0("rnorm", 1:10) := map(as.list(rnorm(n = 10, mean = .SD$pred, sd = .SD$se)), ~.x), rid]
        .[, paste0("rnorm", 1:50) := as.list(exp(rnorm(n = 50, mean = pred, sd = se))), rid] %>%
        .[, map(.SD, sum), by = .(vrbl, cbn_name, mdl_id), .SDcols = keep(names(.), ~grepl("rnorm", .x))] %>%
        melt(id.vars = c("vrbl", "cbn_name", "mdl_id"), variable.name = "rnorm_id", value.name = "pred") %>%
        dt_cntrfctl_cons[dt_id == "2k4", unique(.SD[, .(nbr_opened, cbn_name)])][., on = "cbn_name"] %>%
        .[, diff := nbr_opened - pred] %>%
        .[, vrbl := gsub("_lag[0-9]", "", vrbl)] %>% 
        vvs$hyp_mep_dt[., on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]

    ## individual lines for different models
    dx %>%
        ## .[mdl_id %in% sample(unique(mdl_id), 10)] %>%
        ggplot(aes(x=diff, y=vrbl, group = interaction(mdl_id, cbn_name, vrbl))) +# , group = mdl_id)) +
        geom_density_ridges(scale = 0.9, bandwidth = 1.5, show.legend = F,
                            fill = NA,
                            alpha = 0.2,
                            size = 0.2,
                            color = "#A0A0A0A0") + # need alpha color to get semi-transparent lines
        facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
        scale_y_discrete(expand = expansion(add = c(0.1, 1))) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) 
        ## scale_color_manual(values = rep("#A0A0A0A0", 108))
        

    ## overall plot
    dx %>% 
        ggplot(aes(x=diff, y=vrbl)) +
        geom_density_ridges(scale = 0.9, bandwidth = 1.5) +
        facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
        scale_y_discrete(expand = expansion(add = c(0.1, 1))) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)
        

    ## try without SE
    ## dt_cntrfctl_wse %>% copy() %>% .[, pred_exp := exp(pred)] %>%
    ##     .[, .(sum_pred = sum(pred_exp)), .(cbn_name, 


    ## see why HNWI_5M coefficient changes between cbn1 and 2
    

    ## adt(cbn_dfs_rates$cbn_no_cult_spending) %>%
    ##     .[!adt(cbn_dfs_rates$cbn_all)[, .(iso3c, year)], on = .(iso3c, year)] %>%
    ##     .[, .(iso3c, year, mean_hnwi_200M = mean(hnwi_nbr_200M_lag0), hnwi_nbr_200M_lag0)] %>%
    ##     .[, diff := hnwi_nbr_200M_lag0 - mean_hnwi_200M] %>%
    ##     reg_res_objs$dt_velp_crycoefs[cbn_name == "cbn_no_cult_spending" & vrbl == "hnwi_nbr_200M", .(year, iso3c)][., on = "iso3c"] %>%
    ##     .[order(-diff)] %>% print(n=50)



   
}


## gen_plt_cntrfctl(reg_res_objs$dt_cntrfctl_cons, reg_res_objs$dt_cntrfctl_wse)


gen_plt_velp <- function(dt_velp_crycoefs, dt_velp_scalars) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## check how much hwni variables are zero: per CY and per country
    ## FIXME: move to gen_nbrs? 
    hnwi_cols <- paste0(vvs$hnwi_vars, "_lag0")
    
    
    imap_dfr(cbn_dfs_counts_uscld[1:3],
         ~adt(.x) %>% .[, .SD, .SDcols = c("iso3c", hnwi_cols)] %>% 
             rbind(.[, lapply(.SD, \(x) sum(x== 0)), .SDcols = hnwi_cols][, `:=`(measure = "CY", cbn_name = .y)],
                   .[, lapply(.SD, \(x) all(x==0)), iso3c][, lapply(.SD, sum), .SDcols = hnwi_cols][
                     , `:=`(measure = "cry", cbn_name = .y)],
                   fill = T) %>% tail(2)) %>% # dt gets piped into rbind call -> has to be filtered out by tail
        .[, iso3c := NULL] %>% ## iso3c is leftover from original loop, needs to be yeeted
        melt(id.vars = c("measure", "cbn_name")) %>% dcast.data.table(variable + cbn_name ~ measure)
        
    
    
    ## add hypothesis labeling, ordering
    dt_velp_crycoefs_vis <- dt_velp_crycoefs %>%
        .[vrbl %!in% c("nbr_closed_cum_global", "pm_density_global")] %>%
        vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]

    
    dt_velp_scalars_vis <- dt_velp_scalars %>%
        .[vrbl %!in% c("nbr_closed_cum_global", "pm_density_global")] %>%
        vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]
    

    

    ## density plot of random slopes 
    
    ggplot() + 
        geom_density_ridges(
            dt_velp_crycoefs_vis,
            mapping = aes(x=year, y=vrbl, fill = hyp),
            stat = "binline", scale = 0.95, binwidth = 0.0075, show.legend = F,
            draw_baseline = F, # get rid of baseline -> outliers more visible
            panel_scaling = T, # scale max histogram height per facet
            size = 0) + 
        geom_point(dt_velp_scalars_vis, mapping = aes(x=coef, y=vrbl), show.legend = F,
                   size = 0.7,
                   position = position_nudge(y=0.2)) +
        geom_errorbarh(dt_velp_scalars_vis, mapping = aes(y=vrbl,
                                                 xmin = coef - 1.96*se,
                                                 xmax = coef + 1.96*se),
                       height = 0.3,
                       linewidth = 0.3,
                       position = position_nudge(y=0.2)) +
        geom_text(dt_velp_scalars_vis,
                  mapping = aes(x=0.08, y = vrbl, label = format(round(cor_slpcons,2))),
                  position = position_nudge(y=0.5),
                  size = pt2mm(stylecfg$lbl_fntsz)) +
        facet_grid(hyp~cbn_name, scales = "free", space = "free",
                   switch = "y",
                   labeller = as_labeller(c(# vvs$krnl_lbls,
                       map_chr(vvs$krnl_lbls, ~paste0(strwrap(.x, 14), collapse = "\n")),
                       vvs$cbn_lbls))) +
        coord_cartesian(xlim = c(-0.1, 0.1)) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
        scale_y_discrete(expand = expansion(add = c(0.1, 1)),
                         labels = map_chr(addline_format(vvs$vrbl_lbls),
                                          ~paste(strwrap(.x, 16), collapse = "\n"))) + 
        labs(caption = paste0(c("histogram: distribution of country slopes",
                                "dot+whisker: overall regression coefficient and 95% CI",
                                "number: correlation of country slopes and intercepts"),
                              collapse = "\n"),
             x = "year-coefficient", y= element_blank()) +
        theme_orgpop()

    ## stat_binline
    
}

## gen_res_velps(cbn_dfs_rates)










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

    ## only use the models that have as many models converged as the groups with the most models
    ## hopefully equivalent to number of lags
    nbr_mdls_max <- dt_anls_within_prep4[, .N, lag_variatn][, max(N)]

    df_anls_within_prep5 <- atb(dt_anls_within_prep4) %>%
        group_by(across(all_of(group_vrbls))) %>% # haha plain english verbs make it so EZ AMIRITE
        mutate(nbr_mdls_cvrgd = len(base_lag_spec_id)) %>% 
        filter(nbr_mdls_cvrgd == nbr_mdls_max)


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
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' pick coefs from all models 

    df_anls_all <- df_anls_base %>%
        filter(vrbl_name_unlag != vrbl_name) %>% ## only use lagged variables
        mutate(lag = as.numeric(substring(str_extract(vrbl_name, "_lag(\\d+)"), 5))) %>%
        filter(cbn_name != "cbn_controls") %>%
        group_by(vrbl_name_unlag, cbn_name, lag) %>%
        slice_sample(n=NBR_MDLS)

    ## table(df_anls_all$vrbl_name_unlag)

    ## adt(df_anls_all)[, .N, by = .(vrbl_name_unlag, cbn_name, lag)][order(N)]

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

    ou_objs <- reg_anls_base$ou_objs
    ## number of models to pick for the analyses
    

    optmzd = "loop_nbr" %in% names(gof_df_cbn)
        
    df_anls_within <- construct_df_anls_within(df_anls_base, vvs, NBR_MDLS, optmzd, gof_df_cbn)
    df_anls_all <- construct_df_anls_all(df_anls_base, vvs, NBR_MDLS)

    df_best_mdls <- construct_df_best_mdls(df_anls_base, gof_df_cbn)
    mdl_summary <- construct_best_mdls_summary(df_best_mdls)

    top_coefs <- gen_top_coefs(df_anls_base, gof_df_cbn)
    
    ou_procd <- proc_ou_files(ou_objs, gof_df_cbn, top_coefs)

    ## generate change of coef size from one-out models (what happens to all coefs when variable X gets yeeted)
    dt_oucoefchng <- gendt_oucoefchng(ou_objs, df_anls_base)


    return(
        list(
            gof_df_cbn = gof_df_cbn,
            df_anls_base = df_anls_base,
            df_anls_within = df_anls_within,
            df_anls_all = df_anls_all,
            df_best_mdls = df_best_mdls,
            mdl_summary = mdl_summary,
            top_coefs = top_coefs,
            top_coefs_llrt = ou_procd$top_coefs_llrt,
            ou_anls = ou_procd$ou_anls,
            dt_vif_res = reg_anls_base$dt_vif_res,
            dt_oucoefchng = dt_oucoefchng,
            dt_cntrfctl_cons = reg_anls_base$dt_cntrfctl_cons,
            dt_cntrfctl_wse = reg_anls_base$dt_cntrfctl_wse,
            dt_velp_scalars = reg_anls_base$dt_velp_scalars,
            dt_velp_crycoefs = reg_anls_base$dt_velp_crycoefs
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



    ## dt_hypmep_rcd <- copy(vvs$hyp_mep_dt)
    ## dt_hypmep_rcd[, hyp := "hypx"]
    ## dt_hypmep_rcd[hyp %in% c("h3a", "h3b"), hyp := "h3"]
    

    ## dt_anls_within <- adt(df_anls_within) %>%
    ##     .[, .(lag_variatn, sig, coef, cbn_name, vrbl_name_unlag, lag, t_value)] %>%
    ##     dt_hypmep_rcd[., on = .(vrbl = vrbl_name_unlag)]

    ## dt_anls_within_lbls <- dt_anls_within[lag==5]

    ## dt_anls_within %>% 
    ##     ggplot(aes(x=lag, y=coef, group = vrbl)) +
    ##     geom_line(aes(color = t_value)) +
    ##     geom_point(aes(shape = factor(sig))) + 
    ##     geom_hline(yintercept = 0, linetype = "dashed") +
    ##     geom_text_repel(dt_anls_within_lbls, mapping = aes(x=lag, y=coef, label = vrbl), hjust = 0, size = 3,
    ##                     direction= "y"
    ##                     ) + 
    ##     facet_grid(hyp ~ cbn_name, scales = "free", space = "free") +
    ##     scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    ##     scale_shape_manual(values = c(1,4)) +
    ##     xlim(c(1,8))
        

    
    
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
        select(mdl_id, gof_value, loop_nbr, vrbl_optmzd, cbn_name, regcmd, vrbl_choice, vrbl_choice_cbn_nbr) %>%
        mutate(step_base = 1,
               loop_nbr = as.numeric(loop_nbr),
               base_lag_spec = paste0(cbn_name, vrbl_choice, vrbl_choice_cbn_nbr))
               ## vrbl_choice = gsub("[1-5]", "0", base_lag_spec))

    cvrgnc_df_prep %>% ggplot(aes(x=gof_value)) +
        geom_density(bw = 0.3) +
        facet_wrap(cbn_name~., scales = "free")


    ## developing analysis of value-analyzing
    ## dtx <- data.table(vlu = c(1,1,2, 1,2,2, 2, 2, 1), id = c(rep("a", 3), rep("b", 3), rep("c", 3)))
    ## dtx[id == "a", paste0(table(vlu), collapse = "")]
    ## dtx[, paste0(table(vlu), collapse = ""), by="id"]

    

    cvrgnc_df_test <- cvrgnc_df_prep %>% 
        ## group_by(vrbl_choice, vrbl_choice_cbn_nbr, regcmd, cbn_name) %>% # get top values of each run
        group_by(base_lag_spec, regcmd, cbn_name) %>% # get top values of each run 
        ## group_by(vrbl_choice, base_lag_spec, regcmd, cbn_name) %>% # get top values 
        slice_max(gof_value, n=1, with_ties = F) %>%
        group_by(cbn_name, vrbl_choice, regcmd) %>% ## group by nbr_specs per thld
        summarize(n_gof = len(gof_value), n_distinct_gof = n_distinct(gof_value), var_gof = sd(gof_value),
                  vlu_proc = paste0(table(gof_value), collapse = "")) %>%
        adt()

    ## adt(cvrgnc_df_prep)[, .N, .(cbn_name, vrbl_choice)]
    

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
        group_by(cbn_name, base_lag_spec, loop_nbr, vrbl_optmzd, regcmd) %>%
        slice_max(gof_value, with_ties = F)

    ## group_vrbls <- c("vrbl_choice", "cbn_name", "base_lag_spec", "loop_nbr", "vrbl_optmzd", "regcmd")

    ## cvrgnc_df_prep2 <- cvrgnc_df_prep %>% adt() %>%
    ##     .[, .(gof_value = max(gof_value), step_base = 1), by = group_vrbls] %>% atb()

    cvrgnc_df_prep3 <- cvrgnc_df_prep2 %>% 
        group_by(cbn_name, base_lag_spec, regcmd) %>% 
        arrange(gof_value) %>%
        mutate(step = ave(step_base, FUN = cumsum)) 


    ## find those that didn't converge
    dt_ncvrg <- cvrgnc_df_prep3 %>% adt() %>% 
        .[order(cbn_name, base_lag_spec, regcmd, step)] %>% 
        .[, gof_value_lag := shift(gof_value), .(cbn_name, base_lag_spec, regcmd)] %>%
        .[, diff := gof_value - gof_value_lag] %>% # look at "slope"
        .[, .SD[which.max(step)], .(cbn_name, base_lag_spec, regcmd)] %>% # look at last point
        .[, .(mdl_id, cbn_name, base_lag_spec, regcmd, gof_value, gof_value_lag, step, diff, vrbl_choice)] %>%
        .[diff != 0]


    cvrgnc_df_prep4 <- adt(cvrgnc_df_prep3)[dt_ncvrg, on = .(cbn_name, base_lag_spec, vrbl_choice, regcmd)]

    ##    test_id <- dt_ncvrg$mdl_id[2]
    ##     test_regspec <- get_reg_spec_from_id(test_id, fldr_info)

    ##     reg_setgs_debug <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value =F)
    ##     optmz_reg_spec(test_regspec, fldr_info, reg_setgs_debug)
    ##     lngtd_vrbls_bu <- reg_spec$lngtd_vrbls
    
    cvrgnc_df_prep3 %>% 
        ggplot(aes(x=step, y=gof_value,
                   group = interaction(base_lag_spec, regcmd), color = vrbl_choice,
                   linetype =regcmd)) +
        geom_line(show.legend = ifelse(adt(cvrgnc_df_prep3)[, .N, .(base_lag_spec, regcmd)][, .N] > 5, F, T),
                  linewidth = 0.3) +
        geom_point(dt_ncvrg, mapping = aes(x=step, y=gof_value), show.legend = F) +
        geom_label_repel(dt_ncvrg, mapping = aes(x=step, y=gof_value, label = round(diff,2)),
                         show.legend = F) +
        facet_wrap(~cbn_name, ncol = 1, scales = "free_y")
    

    
}










gen_lbl_raster <- function(dens_dt, method) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    ## library(raster, include.only = c("rasterize", "raster"))

    ## some selection of density 
    ## lbltest_dt <- top_coefs_dens %>% copy() %>%
    ##     .[hyp_id == "h4" & cbn_name == "cbn_all"] # & vrbl_name_unlag == "hnwi_nbr_1M"]

    ## dens_dt %>%
    ##     ggplot(aes(x=x, y=y, group = vrbl_name_unlag)) +
    ##     geom_line()

    ## old raster function, now using st_rasterize
    ## r <- raster(ncols = 45, nrows = 15, xmn = min(dens_dt$x), xmx = max(dens_dt$x),
    ##             ymn = min(dens_dt$y), ymx = max(dens_dt$y))
    
    

    ## scale x up to have good visual inspection
    ## dens_dt2 <- dens_dt %>% copy() %>% .[, x := x*60]
    
    ## combine different variables into polygons
    dens_polys <- split(dens_dt, dens_dt$vrbl_name_unlag) %>% 
        purrr::map(~rbind(.x, .x[1])) %>%
        purrr::map(~.x[c(1, .N-1, .N), y := y-0.01]) %>% # adjust first/last point a bit to get no intersections
        purrr::map(~as.matrix(.x[, .(x,y)])) %>%
        ## sf::st_multipolygon(x=list(.)) %>% 
        purrr::map(~st_polygon(list(.x))) %>% 
        ## sf::st_geometrycollection() %>%
        reduce(st_union)

    ## dens_polys$hnwi_nbr_5M[c(1, .N), y := y-1] %>% .[c(1, .N)]

    ## st_union(dens_polys[[1]], dens_polys[[2]], by_feature = T)
    
    ## plot(dens_polys[[1]])

    ## st_coordinates(dens_polys[[1]]) %>% adt() %>%
    ##     ggplot(aes(x=X, y=Y)) +
    ##     geom_path()

    ## dens_dt$y %>% min()
    
    ## set up raster with sf to compare with random points
    dens_sf <- st_sf(id = "x", geometry = st_sfc(dens_polys))
    dens_raster <- st_rasterize(dens_sf, options = "ALL_TOUCHED=TRUE",
                                st_as_stars(st_bbox(dens_sf), nx = 15, ny=10, values = NA_real_))
    
    raster_pts <- dens_raster %>% adf() %>% adt() %>% .[!is.na(ID), .(x,y)]

        
    ## raster_pts %>% 
    ##     ggplot(aes(x=x, y=y)) +
    ##     geom_point()

    len(dens_polys)

    random_pts <- sf::st_sample(dens_polys, size = 30) %>% st_coordinates() %>% adt() %>% .[, .(x=X, y=Y)]

    ## plot(rnd_pts)
    ## sf::st_coordinates(rnd_pts) %>% adt() %>% #
    ##     ggplot(aes(x=X, y=Y)) + 
    ##     geom_point()

    if (method == "random") {
        return(random_pts)
    } else if (method == "raster") {
        return(raster_pts)

    }
}



gen_plt_oneout_coefs <- function(ou_anls, top_coefs_llrt) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
 
 
    ## combine significance of improvement with coefficient significance
    top_coefs_llrt %>%
        .[!is.na(hyp)] %>% 
        ggplot(aes(x=coef, y=vrbl, color = prop_sig)) +
        geom_point(size = 2) +
        geom_errorbarh(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), height = 0) + 
        facet_grid(hyp~cbn_name, scales = "free", space =  "free") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_color_gradient(low = "#1C5BA6", high = "#BD0017") +
        coord_cartesian(xlim = c(min(top_coefs_llrt$coef, na.rm = T),
                                 max(top_coefs_llrt$coef, na.rm = T)))

}


gen_plt_oneout_llrt_z <- function(ou_anls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## generates violin plot of distribution of z-value (based on LLRT-chisq p-value) of LLRT test
    ## uses z-value due to better visualizability (than p-values)
    
    ## ou_anls[, .(z, ou_set_title_unlag, hyp)]

    ## violin plot z
    ou_anls %>%
        ggplot(aes(x=z, y = ou_set_title_unlag)) +
        geom_violin(bw = 0.1) +
        scale_y_discrete(labels = map_chr(addline_format(vvs$vrbl_lbls),
                                          ~paste(strwrap(.x, 30), collapse = "\n"))) + 
        facet_grid(hyp~cbn_name, scales = "free", space = "free", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 1.96, linetype = "dashed") +
        labs(x="Z-score", y = element_blank()) +
        theme_orgpop()

    ## ## point plot: doesn't scale well 
    ## ou_anls %>%
    ##     ggplot(aes(x=z, y = ou_set_title_unlag, shape = sig, color = sig)) +
    ##     geom_jitter(size = 2, height = 0.2, width = 0) + 
    ##     facet_grid(hyp~cbn_name, scales = "free", space = "free") +
    ##     scale_shape_manual(values = c(1,4)) +
    ##     geom_vline(xintercept = 1.96, linetype = "dashed")

}

gen_plt_oneout_llrt_lldiff <- function(ou_anls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
        
    #' visualizes change in log-likelihood if variable
    ## (or variables, in case of interactions/squared variables/sets)
    ## are removed

    ## violin LL diff
    ou_anls %>%
        ggplot(aes(x=log_likelihood_diff, y = ou_set_title_unlag)) +
        geom_violin(bw = 0.4) + # , size = 0.3, color = "black") +
        scale_y_discrete(labels = map_chr(addline_format(vvs$vrbl_lbls),
                                          ~paste(strwrap(.x, 30), collapse = "\n"))) + 
        facet_grid(hyp~cbn_name, scales = "free", space = "free", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        labs(x="Log likelihood improvement", y = element_blank()) +
        theme_orgpop() +
        theme(plot.margin = unit(rep(5.5, 4), "points"))

    
    ## ou_anls %>%
    ##     ggplot(aes(x=log_likelihood_diff, y = ou_set_title_unlag)) +
    ##     geom_boxplot() + 
    ##     facet_grid(hyp~cbn_name, scales = "free", space = "free")

}



addline_format <- function(x,...){
    #' get custom function for variable labels
    gsub(' \\* ',' *\n',x) %>% # add linebreak in ti_tmitr_interact
        gsub(' \\(\\*100\\)', "", .) %>% # yeet *100 for ginis/inequality percentages
        gsub("\\(thousands\\)", "", .) %>% # yeet thousands from GDP
        gsub("\\(millions\\)", "", .) %>% # yeet millions from gvt cultural spending
        gsub("\\(\\%\\)", "", .) %>% # yeet percentage sign from TMITR
        trimws()
    
}

gen_plt_coef_violin <- function(top_coefs) {
    #' generate violin plot: nicer way of showing distribution of variables (separately) across combinations
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    
    ## # reorder variable names
    ## top_coefs2 <- gen_top_coefs2(top_coefs) %>%
    ##     .[hyp_id != "zcontrols"] %>% 
    ##     .[, vrbl_name_unlag := factor(vrbl_name_unlag, levels = rev(names(vvs$vrbl_lbls)))]
    top_coefs2 <- vvs$hyp_mep_dt[top_coefs, on = .(vrbl = vrbl_name_unlag)] %>%
        .[hyp %!in% c("h1a", "cons")] %>% # yeet intercept and NPO tax exemptions
        .[!is.na(hyp)] # yeet ln_r/ln_s (neg binom regression stuff)
    
    
    


    ## make violin plot
    plt_coef_violin <- ggplot(top_coefs2, aes(y=vrbl, x=coef)) +
        geom_violin(scale = "area", trim = T, bw = 0.04) +
        ## geom_col() + 
        ## use custom bandwidth -> somehow good sizing across facets
        ## geom_jitter(size = 0.4, width = 0, height = 0.1) + # don't litter plot with jitter
        ## scale_y_discrete(labels = addline_format(vvs$vrbl_lbls)) + # actual relabelling of variables (on y-axes)
        scale_y_discrete(labels = map_chr(addline_format(vvs$vrbl_lbls),
                                          ~paste(strwrap(.x, 30), collapse = "\n"))) + 
        facet_grid(hyp ~ cbn_name, scales = "free", switch = "y", space = "free",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        ## theme(# strip.text.y.left = element_text(angle = 0, size = 11),
        ## strip.text.x = element_text(size = 12),
              ## axis.text.y = element_text(size = 11),
              ## axis.text.x = element_text(size = 10)) + 
        labs(x="coefficient value", y = element_blank()) +
        theme_orgpop() +
        theme(plot.margin = unit(rep(4, 4), "points"))
    plt_coef_violin

    return(plt_coef_violin)
    
}




gen_plt_best_coefs_cloud <- function(top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' coef distribution

    top_coefs %>% copy() %>%
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>% 
        ggplot(aes(x=coef, y = vrbl)) +
        geom_point(position = position_jitter(width = 0, height = 0.4, seed = 3), size = 0.2) + 
        geom_errorbarh(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), height = 0,
                       position = position_jitter(width = 0, height = 0.4, seed = 3), alpha = 0.15) + 
        facet_grid(hyp~cbn_name, scales = "free", space =  "free") +
        geom_vline(xintercept = 0, linetype = "dashed")

}

gen_plt_best_coefs_single <- function(top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' pick best-fitting coef for each variable (now mixes a bunch of models together tho)
    ## dtx <- copy(vvs$hyp_mep_dt)[, vrbl := factor(vrbl, level = rev(names(vvs$vrbl_lbls)))]
        


    top_coefs %>% copy() %>%
        .[, .SD[which.max(log_likelihood)], by = .(vrbl_name_unlag, cbn_name)] %>%
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>% # original 
        ## .[copy(vvs$hyp_mep_dt), on = .(vrbl_name_unlag = vrbl)] # different order
        ## dtx[., on = .(vrbl = vrbl_name_unlag)] %>%
        .[!is.na(hyp)] %>% 
        ggplot(aes(x=coef, y=vrbl, color = factor(sig))) +
        geom_point() +
        geom_errorbarh(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), height = 0) + 
        facet_grid(hyp~cbn_name, scales = "free", space =  "free") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_color_manual(values = c("#1C5BA6", "#BD0017")) 

}


gen_plt_lag_dens <- function(top_coefs) {
    #' violin plot of lags
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ## aggregate by vrbl/lag/cbn
    lag_prep1 <- top_coefs %>% copy() %>%
        .[lag != 0, .N, by = .(vrbl_name_unlag, cbn_name, lag)] # yeet time-invariant
        
    ## set up grid 
    lag_grid <- lag_prep1 %$% 
        expand.grid(vrbl_name_unlag = unique(vrbl_name_unlag), cbn_name = unique(cbn_name), lag = unique(lag)) %>%
        adt() 
    
    ## combine grid with actual data
    lag_prep1[lag_grid, on = .(vrbl_name_unlag, cbn_name, lag)] %>%
        .[is.na(N), N := 0] %>% # assign 0 to not matched
        .[, Nprob := N / sum(N), by = .(vrbl_name_unlag, cbn_name)] %>% 
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>%
        .[!is.na(hyp)] %>% # yeet cons, ln_r, ln_s
        ## plotting
        ggplot(aes(x=factor(lag), y=vrbl, fill = Nprob, color = Nprob)) +
        geom_tile(linewidth = 0) + 
        facet_grid(hyp~cbn_name, scales = "free", space =  "free", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls))) + 
        scale_fill_gradient(low = "grey90", high = "#0077b6", na.value = "white") + # grey to only blue
        scale_color_gradient(low = "grey90", high = "#0077b6", na.value = "white") + # yeet remaining border lines
        ## theme(# panel.grid = element_blank(),
        ##       # panel.background = element_rect(fill = "white"),
        ##     strip.text.y.left = element_text(angle = 0)
        ## ) +
        ## geom_hline(yintercept = Inf, color = "white", size = 2) +
        theme_orgpop() +
        theme(panel.grid.major = element_blank(), # yeet the grid
              ## panel.background = element_rect(fill = "white")) +
              
              panel.background = element_blank()) +
        
        theme(legend.position = "bottom") + 
        scale_y_discrete(labels = addline_format(vvs$vrbl_lbls), expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        labs(x="lag", y=element_blank())
        
                  

    
    ## scale_fill_gradient(low = "grey80", high = "blue") # grey to string-blue
    ## scale_fill_gradient(high = "#132B43", low = "#56B1F7") # reversed normal 
    ## scale_fill_gradient(high = "#c0e5f9", low = "#0077b6") # blue 
    ## scale_fill_gradient(high = "#000000", low = "#FFFFFF") # black-white 
}

gen_plt_vrbl_cycnt <- function(df_reg_rts, stylecfg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate coverage of variables
    #' uses df_reg_rts, which is created after filtering out AN years
    #' but that's wrong -> other data is complete

    

    ## just use the same variables as before
    rel_lngtd_vrbls <- c("tmitr_approx_linear20step",
                        "hnwi_nbr_30M",
                        "gptinc992j",
                        ## "sptinc992j_p90p100",
                        ## "shweal992j_p90p100",
                        "ghweal992j",
                        "smorc_dollar_fxm",
                        "NY.GDP.PCAP.CDk",
                        "SP.POP.TOTLm")

    
    dt_cvrg <- melt(adt(df_reg_rts), id.vars = c("iso3c", "year"), measure.vars = rel_lngtd_vrbls) %>%
        .[, .(nbr_CYs = 1.0*sum(!is.na(value))), .(variable, year)]

    dt_vrbl_lbls <- data.table(vrbl = names(vvs$vrbl_lbls), lbl = addline_format(unname(vvs$vrbl_lbls))) %>%
        .[rbind(
            dt_cvrg[variable =="tmitr_approx_linear20step" & year == 2003],
            dt_cvrg[variable == "hnwi_nbr_30M" & year ==1998],
            dt_cvrg[variable == "gptinc992j" & year ==1987],
            dt_cvrg[variable == "ghweal992j" & year ==1994],
            ## dt_cvrg[variable == "shweal992j_p90p100" & year ==1994],
            ## dt_cvrg[variable == "sptinc992j_p90p100" & year ==1994],
            dt_cvrg[variable == "smorc_dollar_fxm" & year ==2005],
            dt_cvrg[variable == "NY.GDP.PCAP.CDk" & year ==2000],
            dt_cvrg[variable == "SP.POP.TOTLm" & year ==2005]), on = .(vrbl = variable)] %>% 
        .[, vrbl := factor(vrbl, levels = levels(dt_cvrg$variable))] %>%
        .[order(vrbl)] %>% 
        .[, linetype := seq(1, .N)]

    ## library(showtext)
    ## font_add("Crimson", "/usr/share/texmf-dist/fonts/opentype/kosch/crimson/Crimson-Roman.otf")

    ## fontfam <- "Crimson"
    ## fontfam <- "Graphik"

    ## try different visualization
    ## color: I get it, but harder for normies, even with numbers.. 
    ggplot(dt_cvrg, aes(x=year, y=variable, fill = nbr_CYs)) +
        geom_tile() +
        scale_fill_gradient(low = "grey90", high = "#0077b6", na.value = "white")  + # grey to only blue
        geom_text(dt_cvrg[year %in% (seq(1990, 2020, 15)-1)],
                  mapping = aes(x=year, y=variable, label = nbr_CYs))

    ## segment thickness
    ## hmm not that bad in combination with color and labels.. 
    dt_cvrg_seg <- dt_cvrg %>% copy() %>% .[, `:=`(nbr_CYs_lag = shift(nbr_CYs, 1), year_lag = year -1), variable]

    ## width + color + text
    dt_cvrg_seg %>%
        ggplot(aes(x = year, xend = year_lag, y=variable, yend = variable, size = nbr_CYs, color = nbr_CYs)) +
        geom_segment() +
        scale_color_gradient(low = "grey90", high = "#0077b6", na.value = "white") +
        geom_text(dt_cvrg_seg[year %in% (seq(1990, 2020, 15)-1)],
                  mapping = aes(x=year, y=variable, label = nbr_CYs), color = "black", nudge_y=-0.3)
        
    ## dt_cvrg_seg %>%
    ##     ggplot(aes(x=year, xend = year_lag, y=variable, yend = variable, nudge_y = nbr_CYs)) +
    ##     geom_segment(mapping = aes(nudge_y = nbr_CYs)) +
    ##     geom_linerange(
    ##         position_dodge


    ## abuse dodge
    dt_dodge_abuse <- data.table(x=c(0,1,2,3), y = c(0,1,2,3)) %>%
        .[, .(fillup = 0:x), .(x,y)] %>% # create fake bars
        .[, id := "a"] %>%
        .[, type := fifelse(y==fillup, "real", "cushion")]

    ## need to have some bottom stuff since dodge always dodges around center
    dt_doge_abuse_bottom <- copy(dt_dodge_abuse)[fillup > 0][, `:=`(fillup = fillup*-1, type = "cushion")]

    rbind(dt_dodge_abuse, dt_doge_abuse_bottom) %>% 
        ggplot(aes(x=x, y = id, xmin = x-1, xmax = x, group = factor(fillup), color = type)) +
        geom_linerange(position = position_dodge(width = 0.1), size = 1, alpha = 0.5)

    ## maybe I can also do the same with segments: just add a huge number of fake ones and dodge them away..
    ## -> check tomorrow

    library(ggridges)
    ## extend/unfold and use ridgeline

    dt_cvrg %>% copy() %>% .[, .(dens = 1:nbr_CYs), by = .(variable, year)] %>%
        ggplot(aes(x=year, y=variable)) +
        ## geom_violin()
        geom_density_ridges(scale = 0.95, panel_scaling = T)
    ## geom_density_ridges: seems to scale area to same size

    
    ## geom_ridgeline: seems to be what I want, but need to hack y-axis: has to be continuous
    dt_cvrg %>% copy() %>% .[, vrbl_fac := factor(variable)] %>%
        .[, nbr_CYs_adj := nbr_CYs/max(nbr_CYs)] %>% # scale to max 1
        ggplot(aes(x=year, y = vrbl_fac, height = nbr_CYs_adj*0.9)) + # can add some scale to not touch top
        geom_ridgeline()
                   




    ## faceted line again..
    ## why doesn't that work?  maybe the other lines are confusion? 
    copy(vvs$hyp_mep_dt)[copy(dt_cvrg), on = .(vrbl = variable)] %>% 
        ggplot(aes(x=year, y = nbr_CYs)) +
        geom_line() +
        facet_grid(hyp ~., switch = "y") +
        theme(panel.grid = element_blank(),
              panel.background = element_rect(color = "black"),
              strip.text.y.left = element_text(angle = 0),
              panel.border = element_rect(color = "black", fill = NA))
        ## theme_bw()
    

    
    
    ggplot() + 
        geom_line(dt_cvrg, mapping = aes(x=year, y =nbr_CYs, group = variable,
                                         linetype = variable),
                  show.legend = F) +
        geom_text_repel(dt_vrbl_lbls, mapping = aes(nudge_x=year + 0.7, nudge_y=nbr_CYs -9, label = lbl,
                                                    x = year, y = nbr_CYs, segment.linetype = linetype),
                        min.segment.length = 0,
                        hjust = 0, max.iter = 0,
                        ## family = fontfam,
                        size = pt2mm(stylecfg$lbl_fntsz)
                        ) + 
        scale_linetype_manual(values = setNames(dt_vrbl_lbls$linetype, dt_vrbl_lbls$vrbl)) + 
        labs(y="Number of countries covered") +
        theme_orgpop()
        
    
    
    
    ## ggplot() +
    ##     geom_line(dt_cvrg, mapping = aes(x=year, y =nbr_CYs),
    ##               show.legend = F) +
    ##     facet_grid(variable~., space = "free")

}

## gen_plt_vrbl_cycnt(df_reg_rts, stylecfg)





gen_plt_vif <- function(dt_vif_res, top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate plot of VIF distribution
    #' values previously generated as part of postestimation

    ## add cbn_name
    dt_vif_res2 <- top_coefs[, unique(.SD), .SDcols = c("mdl_id", "cbn_name")] %>% .[dt_vif_res, on = "mdl_id"] %>%
        .[, vrbl_unlag := gsub("_lag[1-5]", "", Term)] %>% # generate unlag names
        vvs$hyp_mep_dt[., on = .(vrbl = vrbl_unlag)] %>% # add hypothesees
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]
    
    
    ## ## check some outliers
    ## dt_vif_res2[vrbl %in% c("nbr_closed_cum_global", "NY.GDP.PCAP.CDk") | grepl("hnwi", vrbl)]  %>%
    ##     ggplot(aes(x=VIF, y = vrbl, color = vrblset)) +
    ##     geom_violin() + 
    ##     ## geom_point(position_jitterdodge(jitter.width = NULL, jitter.height = 0.2, dodge.width = 0.9)) +
    ##     geom_point(position = position_jitter(width = 0), size = 0.8) + 
    ##     facet_grid(hyp~cbn_name, scales = "free", space = "free", switch = "y",
    ##                labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls)))
    
    ## dt_vif_res2[vrbl == "NY.GDP.PCAP.CDk"][, .N]

    ## dt_vif_res2[vrbl == "NY.GDP.PCAP.CDk" & VIF > 8]

    
    ggplot(dt_vif_res2, aes(x=VIF, y=vrbl, group = interaction(vrblset, vrbl),
                           fill = vrblset, color = vrblset)) +
        geom_violin(bw = 0.1, position = position_dodge(width = 0.5), size = 0.2) + 
        scale_y_discrete(labels = map_chr(addline_format(vvs$vrbl_lbls),
                                          ~paste(strwrap(.x, 30), collapse = "\n"))) + 
        facet_grid(hyp~cbn_name, scales = "free", space = "free", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        theme_orgpop() +
        theme(legend.position = "bottom") +
        scale_color_discrete(guide = guide_legend(title = "Variable set"),
                             labels = c(all = "all", wosqrd = "all except squared variables and interactions")) +
        scale_fill_discrete(guide = guide_legend(title = "Variable set"),
                            labels = c(all = "all", wosqrd = "all except squared variables and interactions"))
        

}



## gen_plt_vif(reg_res_objs$dt_vif_res, reg_res_objs$top_coefs)


gen_plt_cbn_cycnt <- function(cbn_dfs_rates, stylecfg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    
    dt_cbn_cycnts <- imap_dfr(cbn_dfs_rates[1:3], ~adt(.x)[, .N, year][, cbn := .y])

    dt_cbn_lbls <- dt_cbn_cycnts[year == 2006] %>% copy() %>%
        .[, lbl  := vvs$cbn_lbls[[cbn]], cbn]

    
    ggplot() +
        geom_line(dt_cbn_cycnts, mapping = aes(x=year, y=N, group = cbn, linetype = cbn),
                  show.legend = F) +
        geom_text_repel(dt_cbn_lbls, mapping = aes(x = year, y = N, label = lbl),
                        nudge_y = -9, min.segment.length = 100,
                        size = pt2mm(stylecfg$lbl_fntsz)) +
        labs(y = "Number of countries covered") +
        theme_orgpop()
                       

}


## gen_plt_cbn_cycnt(cbn_dfs_rates)




gen_reg_res_plts <- function(reg_res_objs, vvs, NBR_MDLS, only_priority_plts, stylecfg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate all the plots
    

    df_anls_base <- reg_res_objs$df_anls_base
    gof_df_cbn <- reg_res_objs$gof_df_cbn
    df_anls_within <- reg_res_objs$df_anls_within
    df_best_mdls <- reg_res_objs$df_best_mdls
    mdl_summary <- reg_res_objs$mdl_summary
    top_coefs <- reg_res_objs$top_coefs
    top_coefs_llrt <- reg_res_objs$top_coefs_llrt
    ou_anls <- reg_res_objs$ou_anls
    dt_vif_res <- reg_res_objs$dt_vif_res
    dt_oucoefchng <- reg_res_objs$dt_oucoefchng
    dt_cntrfctl_cons <- reg_res_objs$dt_cntrfctl_cons
    dt_cntrfctl_wse <- reg_res_objs$dt_cntrfctl_wse
    dt_velp_scalars <- reg_res_objs$dt_velp_scalars
    dt_velp_crycoefs <- reg_res_objs$dt_velp_crycoefs
    
    l_plts <- list(
        plt_cbn_log_likelihoods = gen_plt_cbn_log_likelihoods(gof_df_cbn),
        plt_coef_violin = gen_plt_coef_violin(top_coefs),
        plt_best_coefs_cloud = gen_plt_best_coefs_cloud(top_coefs),
        plt_best_coefs_single = gen_plt_best_coefs_single(top_coefs),
        plt_lag_dens = gen_plt_lag_dens(top_coefs),
        plt_oneout_coefs = gen_plt_oneout_coefs(ou_anls, top_coefs_llrt),
        plt_oneout_llrt_z = gen_plt_oneout_llrt_z(ou_anls),
        plt_oneout_llrt_lldiff = gen_plt_oneout_llrt_lldiff(ou_anls),
        plt_vrbl_cycnt = gen_plt_vrbl_cycnt(df_reg_rts, stylecfg),
        plt_cbn_cycnt = gen_plt_cbn_cycnt(cbn_dfs_rates, stylecfg),
        plt_vif = gen_plt_vif(dt_vif_res, top_coefs),
        plt_oucoefchng = gen_plt_oucoefchng(dt_oucoefchng),
        plt_cntrfctl = gen_plt_cntrfctl(dt_cntrfctl_cons, dt_cntrfctl_wse),
        plt_velp = gen_plt_velp(dt_velp_crycoefs, dt_velp_scalars)
        ## plt_oucoefchng_tile = gen_plt_oucoefchng_tile(dt_oucoefchng),
        ## plt_oucoefchng_cbn1 = gen_plt_oucoefchng(dt_oucoefchng[cbn_name == "cbn_all"]),
        ## plt_oucoefchng_cbn2 = gen_plt_oucoefchng(dt_oucoefchng[cbn_name == "cbn_no_cult_spending"]),
        ## plt_oucoefchng_cbn3 = gen_plt_oucoefchng(dt_oucoefchng[cbn_name == "cbn_no_cult_spending_and_mitr"])
    )
        
    
    ## plt_hyp_thld_res <- gen_plt_hyp_thld_res(top_coefs)
    ## plt_coef_krnls <- gen_plt_coef_krnls(top_coefs)
    ## plt_best_models_condensed = gen_plt_mdl_summary(mdl_summary, vvs)

    if (!only_priority_plts) {
        l_plts <- c(l_plts,
                    list(
                        plt_reg_res_within = gen_plt_reg_res_within(df_anls_within, vvs, NBR_MDLS),
                        plt_reg_res_all = gen_plt_reg_res_all(df_anls_within, vvs),
                        plt_best_models_wlag = gen_plt_best_mdls_wlag(df_best_mdls, vvs)))
    }
    

        
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
            plt_cbn_log_likelihoods = list(filename = "cbn_log_likelihoods.pdf", width = 16, height = 8,
                                           caption = "model log likelihood distribution per dataset"),
            plt_reg_res_within = list(filename = "reg_res_within.pdf", width = 18, height = 18,
                                      caption = "bottom text"),
            plt_reg_res_all = list(filename = "reg_res_all.pdf", width = 18, height = 18,
                                   caption = "bottom text"),
            plt_best_models_wlag = list(filename = "best_models_wlag.pdf", width = 15, height = 23,
                                        caption = "bottom text"),
            plt_best_models_condensed = list(filename = "best_models_condensed.pdf", width = 18, height = 16,
                                             caption = "bottom text"),
            plt_lag_cprn = list(filename = "lag_cprn.pdf", width = 18, height = 12,
                                caption = "Lag comparison"),
            plt_cvrgnc = list(filename = "cvrgnc.pdf", width = 12, height = 15,
                              caption = "Model improvement"),
            ## plt_hyp_thld_res = list(filename = "hyp_thld_res.pdf", width = 7, height = 6),
            ## plt_coef_krnls = list(filename = "coef_krnls.pdf", width = 9, height = 6),
            plt_coef_violin = list(filename = "coef_violin.pdf", width = 18, height = 18,
                                   caption = paste0("Distribution of coefficient point estimates ",
                                                    "(Gaussian kernel density estimate; bandwidth = 0.04)")),
            plt_best_coefs_cloud = list(filename = "best_coefs_cloud.pdf", width = 18, height = 12,
                                        caption = "Coefficient point estimate and 95% CI"),
            plt_best_coefs_single = list(filename = "best_coefs_single.pdf", width = 18, height = 12,
                                         caption = "Model coefficients (best fitting model)"),
            plt_lag_dens = list(filename = "lag_dens.pdf", width = 18, height = 14,
                                caption = "Distribution of lag choice after optimization"),
            plt_oneout_coefs = list(filename = "oneout_coefs.pdf", width = 18, height = 12,
                                    caption = paste0("Model coefficients (best fitting model; ",
                                                     "colored by significance of model improvement)")),
            plt_oneout_llrt_lldiff = list(filename = "oneout_llrt_lldiff.pdf", width = 18, height = 18,
                                          caption = paste0("Model improvement given variable inclusion ",
                                                           "(Gaussian kernel density estimate; bandwidth = 0.4)")),
            plt_oneout_llrt_z = list(filename = "oneout_llrt_z.pdf", width = 18, height = 18,
                                     caption = paste0("Distribution of Z-score of log-likelihood ",
                                                      "ratio test p-value ",
                                                      "(Gaussian kernel density estimate; bandwidth = 0.1)")), 
            ## "given variable inclusion")),
            plt_vrbl_cycnt = list(filename = "vrbl_cycnt.pdf", width = 18, height = 10,
                                  caption = "Number of countries with per year per variable"),
            plt_cbn_cycnt = list(filename = "cbn_cycnt.pdf", width = 18, height = 8,
                                 caption = "Number of countries per year per variable combination"),
            plt_vif = list(filename = "vif.pdf", width = 18, height = 18,
                           caption = paste0("Distribution of VIF estimates ",
                                            "(Gaussian kernel density estimate; bandwidth = 0.1)")),
            plt_velp = list(filename = "velp.pdf", width = 24, height = 16,
                            caption = "Results of regressing longitudinal variables on year"),
            plt_cntrfctl = list(filename = "cntrfctl.pdf", width = 16, height = 12,
                            caption = "Counterfactual simulations"),
            plt_oucoefchng = list(filename = "oucoefchng.pdf", width = 24, height = 12,
                                  caption = "Coefficient changes given addition of other variables"),
            plt_oucoefchng_tile = list(filename = "oucoefchng_tile.pdf", width = 24, height = 12,
                                  caption = "Coefficient changes given addition of other variables"),
            plt_oucoefchng_cbn1 = list(filename = "oucoefchng_cbn1.pdf", width = 14, height = 12,
                                       caption = paste0("Coefficient changes given addition of other variables ",
                                                        "(DS all IVs)")),
            plt_oucoefchng_cbn2 = list(filename = "oucoefchng_cbn2.pdf", width = 14, height = 12,
                                       caption = paste0("Coefficient changes given addition of other variables ",
                                                        "(DS --CuSp)")),
            plt_oucoefchng_cbn3 = list(filename = "oucoefchng_cbn3.pdf", width = 14, height = 12,
                                       caption = paste0("Coefficient changes given addition of other variables ",
                                                        "(DS --CuSp/TMITR"))

        )
    )

}


c.screenreg <- function(trl) {
    #' custom screenreg function
    #' takes a tex reg list object, which contains
    #' - l: list of texreg objects
    #' - custom.coef.map: order/labels of coefficients
    #' - groups: group information
    
    do.call("screenreg", trl)
    
}

## c.texreg <- function(trl, ) {
## }
    

gen_tblcfg <- function(label, TABLE_DIR, caption) {
    list(label = label,
         file = paste0(TABLE_DIR, label, ".tex"),
         caption = caption)
}
    

gen_tblcfgs <- function(TABLE_DIR) {

    ## trstylcfg <- list(dcolumn = T, single.row = T, leading.zero = F)

    list(
        tbl_regrslts_wcptblF = gen_tblcfg(label = "tbl:regrslts_wcptblF", TABLE_DIR = TABLE_DIR,
                                          caption = "Negative binomial models of private museum founding rate"),
        tbl_regrslts_wcptblT = gen_tblcfg(label = "tbl:regrslts_wcptblT", TABLE_DIR = TABLE_DIR,
                                          caption = "Negative binomial models of private museum founding rate"),
        tbl_descs = gen_tblcfg(label = "tbl:descs", TABLE_DIR = TABLE_DIR,
                               caption = "Summary Statistics"),
        tbl_cbn_cpsgn = gen_tblcfg(label = "tbl:cbn_cpsgn", TABLE_DIR = TABLE_DIR,
                                   caption = "Dataset composition by region")        
    )
}



fmt_pvlu <- function(p) {
    if (p >= 0.05) stars <- ""
    if (p < 0.05) stars <- "^{*}"
    if (p < 0.01) stars <- "^{**}"
    if (p < 0.001) stars <- "^{***}"
    stars
}

fmt_cell <- function(coef, se, pvalue, wcptbl) {

    cell_proc <- sprintf("%s \\; (%s)%s", # \\; 
            coeftostring(coef, lead.zero = F, digits = 2),
            format(round(se,2), nsmall = 2),
            fmt_pvlu(pvalue))

    ## if mswcptbl = T, wrap cell contents in $ for math mode
    if (wcptbl) cell_proc <- sprintf("$%s$", cell_proc)

    return(cell_proc)

}
    
gentbl_regrslts <- function(top_coefs, gof_df_cbn, df_best_mdls, wcptbl) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the regression result table:
    #' first generate the coefficient and gof dts
    
    ## coef table: coefficient of best-fitting variable-combination
    top_coefs_prepd <- top_coefs %>% copy() %>%
        .[, .SD[which.max(log_likelihood)], by = .(vrbl_name_unlag, cbn_name)] %>%
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>%
        .[vrbl %!in% c("ln_s", "ln_r")] %>% 
        .[, .(vrbl, mdl_name = cbn_name, coef, se, pvalue = pvalues)]
    ## assign mdl_name to cbn_name (cbn_name is the model column needed for genxtbl_regtbl)

    ## set up gof label dts
    dt_gof_lbls <- list(
        list(gof_name = "N"              , gof_lbl = "N"              , decimal = 0),
        list(gof_name = "N_g"            , gof_lbl = "No. countries"  , decimal = 0),
        list(gof_name = "log_likelihood" , gof_lbl = "Log likelihood" , decimal = 2)) %>%
        rbindlist() %>% 
        .[, gof_lbl := factor(gof_lbl, levels = gof_lbl)] 
        ## .[order(gof_lbl)] %>% 
        ## .[, gof_name := factor(gof_name, levels = gof_name)]
             
    ## set up gof values: get best models, then get the gofs for them 
    dt_gofs_prepd <- adt(gof_df_cbn)[gof_names == "log_likelihood"] %>%
        .[, .SD[which.max(gof_value), .(mdl_id)], cbn_name] %>%
        .[adt(gof_df_cbn), on = "mdl_id", nomatch = NULL] %>%
        .[gof_names %in% c("N", "N_g", "log_likelihood"), .(cbn_name, gof_names, gof_value)] %>% 
        dt_gof_lbls[., on = .(gof_name = gof_names)] %>% # label gof
        .[order(gof_lbl)] %>% 
        .[, .(mdl_name = cbn_name, gof_name = gof_lbl, gof_value, decimal)]

    ## pass stuff to genxtbl_regtbl
    xtbl_regrslt <- genxtbl_regtbl(dt_coefs = top_coefs_prepd,
                                   vrbl_lbls = addline_format(vvs$vrbl_lbls),
                                   dt_gofs = dt_gofs_prepd,
                                   mdl_lbls = vvs$cbn_lbls,
                                   wdth_vrbl = "6cm",
                                   wcptbl = wcptbl,
                                   vrbl_grps = vvs$hyp_mep_dt[order(hyp)] %$% setNames(hyp, vrbl),
                                   grp_lbls = gsub("\n", " ", vvs$krnl_lbls),
                                   wdth_grp = "0.5mm"
                                   )
    ## pvxtbl(xtbl_regrslt, crop = F)

    return(xtbl_regrslt)
}


gen_cmidrule_header <- function(ubrcol_lbls, ubr_colwidth, row_lbl, col_lbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## generate multi-row header for multicolumns
    ## assumes first cell (first row in top column row) is empty
    ## row_lbl: name for row-entities

    ubr_cols <- map_chr(ubrcol_lbls, ~sprintf("\\multicolumn{%s}{c}{%s}", ubr_colwidth, .x))

    cmidrules <- map_chr(1:len(ubrcol_lbls),
                         ~sprintf("\\cmidrule(r){%s-%s}",
                                  ## beginning is end - ubr_colwidth + 1 ("inclusive")
                         (((.x * ubr_colwidth) + 1) - ubr_colwidth) + 1, 
                         (.x * ubr_colwidth) + 1)) %>% # end is number of uber-columns + 1
        paste0(collapse = "")
                   
    ubr_cols_clapsd <- paste0(c("\\hline \n", ## hline on top 
                                ubr_cols), collapse = " & ")

    ubr_row <- paste0(ubr_cols_clapsd, " \\\\ \n", cmidrules)

    ## collapse raow_lbl and col_lbls, and linebreaks before/after
    second_row <- sprintf("\n %s \\\\ \n", paste0(c(row_lbl, col_lbls), collapse = " & "))

    c(ubr_row, second_row)
}
    
        
    
    


gentbl_cbn_cpsgn <- function(cbn_dfs_rates) {
    #' generate table of combination region composition
    
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    dt_reg6_lbls <- data.table(reg6 = names(reg6_lbls), lbl = unlist(reg6_lbls))

    dt_cbn_cpsn <- imap_dfr(cbn_dfs_rates[1:3], ~adt(.x)[, .N, .(reg6 = rcd_iso3c_reg6(iso3c))][, cbn := .y]) %>%
        .[, `:=`(prop = latexTranslate(paste0(format(round(100*N/sum(N),1), nsmall = 1), "%")), 
                 N_fmt = fmt_nbr_flex(N)), cbn] %>% .[, N := NULL] %>% # format N into chr, yeet original int
        melt(id.vars = c("reg6", "cbn")) %>%
        .[, variable := factor(variable, levels = c("N_fmt", "prop"))] %>% 
        dcast.data.table(reg6 ~ cbn + variable) %>%
        dt_reg6_lbls[., on = "reg6"] %>%
        .[, reg6 := NULL]

 
    

    clm_names <- list()
    clm_names$pos <- list(-1,-1)
    clm_names$command <- gen_cmidrule_header(vvs$cbn_lbls, 2, "region", rep(c("N", "Percent"), 3))

    list(dt_fmtd = dt_cbn_cpsn,
         align_cfg = align_cfg <- c("l", "p{2.5cm}", rep("r", 6)),
         add_to_row = clm_names,
         hline_after = c(0, nrow(dt_cbn_cpsn)))
    
    
        
    


}

## gentbl_cbn_cpsgn(cbn_dfs_rates) %>% pvxtbl(crop = T, landscape = T)


gen_res_tbls <- function(reg_res_objs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    top_coefs <- reg_res_objs$top_coefs
    df_best_mdls <- reg_res_objs$df_best_mdls
    
    gof_df_cbn <- reg_res_objs$gof_df_cbn


    tbl_regrslts_wcptblF <- gentbl_regrslts(top_coefs, gof_df_cbn, df_best_mdls, wcptbl = F)
    tbl_regrslts_wcptblT <- gentbl_regrslts(top_coefs, gof_df_cbn, df_best_mdls, wcptbl = T)
    ## pvxtbl(tbl_regtbl)
    ## do.call("render_xtbl", c(gen_tblcfgs(TABLE_DIR)$regrslts, xtbl_regrslt))
    
    tbl_descs <- gentbl_sum_stats_rates(df_reg_rts, cbn_dfs_rates_uscld, vvs)
    ## pvxtbl(tbl_descs, crop = T, landscape = T)
    
    tbl_cbn_cpsgn <- gentbl_cbn_cpsgn(cbn_dfs_rates)
    
    ## do.call("render_xtbl", c(tbl_descs, list(label = "descs2", caption = "descs2",
    ##                                          file = paste0(TABLE_DIR, "descs2.tex"))))

    list(tbl_regrslts_wcptblF = tbl_regrslts_wcptblF,
         tbl_regrslts_wcptblT = tbl_regrslts_wcptblT, 
         tbl_descs = tbl_descs,
         tbl_cbn_cpsgn = tbl_cbn_cpsgn)
}






render_all_reg_res_plts <- function(reg_res, batch_version) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' wrapper to comfily render all the plots of a regression results version

    
    lapply(names(reg_res$plts), \(x)
           render_reg_res(x, reg_res, batch_version))

    
}
    


render_reg_res <- function(plt_name, reg_res, batch_version) {
    #' general way to plot regression result to file, also adding batch number
    
    plt <- reg_res$plts[[plt_name]]

    
    ## plt_name <- deparse(substitute(plt)) %>% strsplit("$", fixed = T) %>% unlist() %>% tail(1)

    plt_cfg <- gen_plt_cfgs() %>% chuck(plt_name)
    
    ## plt_cfg <- reg_res$plt_cfgs[[plt_name]]

    plt_filename <- paste0(FIG_DIR, "plt_", batch_version, "_", plt_cfg$filename)
    
    pdf(plt_filename,  width = plt_cfg$width/2.54, height = plt_cfg$height/2.54)
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
                                              ## mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>% 
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

    NBR_MDLS <- 1
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

lnbr <- function(nbr, digits) {
            #' generate list number: make sure numbers are properly handled and rounded
            nbr_name = achr(substitute(nbr))
            list(nbr_name = nbr_name, nbr = nbr, digits = digits)
        }


gen_plt_chng <- function(l_cbndfs) {
    #' generate some stuff of average change over time
    #' issue is basically that there's not so much change
    #' output can be table, violin, boxbplot, density, spaghetti, 
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    dt_grwth <- l_cbndfs$cbn_all %>% adt() %>% melt(id.vars = c("iso3c", "year")) %>%
        .[grepl("_lag0", variable) & !grepl("sqrd", variable)]
    
        
    dt_chng <- dt_grwth[, .SD[(year == max(year) | year == min(year)) & max(year) != min(year), # filter out 1 CYs
                   .(value, year)], .(iso3c, variable)] %>% 
        .[, year_abs := fifelse(year > mean(year), "vlu_end", "vlu_start"), .(iso3c)] %>%
        dcast.data.table(iso3c + variable ~ year_abs) %>%
        .[, chng := vlu_end - vlu_start]
    
    dt_chng %>% copy() %>%
            ggplot(aes(x=chng)) + geom_density() + facet_wrap(~variable, scales = "free")
        ## .[!grepl("SP.POP", variable)] %>% ggplot(aes(y=variable, x= chng)) + geom_boxplot()

    dt_chng[, .(mean_chng = mean(chng), sd_chng = sd(chng)), variable] %>% print(n=200)

    dt_chng[, .(iso3c, variable, vlu_start, vlu_end)] %>%
        melt(id.vars = c("iso3c", "variable"), variable.name = "timepoint") %>%
        .[, timepoint := factor(timepoint, levels = c("vlu_start", "vlu_end"))] %>% 
        ggplot(aes(x=timepoint, y = value, group = iso3c)) +
        geom_line(alpha = 0.2) +
        facet_wrap(~variable, scales = "free")

    ## dt_chng %>% copy() %>% .[variable == "pm_density_lag0"] %>% .[, chng := vlu_end - vlu_start] %>%
        ## .[chng < 0]
    
}

## gen_plt_chng(cbn_dfs_rates_uscld)



gen_nbrs_pred <- function(top_coefs, cbn_dfs_rates_uscld, df_reg, print_examples = print_examples) {
    #' generate the numbers related to prediction 
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## intercept numbers
    intcpt <- top_coefs[vrbl_name_unlag == "cons" & cbn_name == "cbn_all", .SD[which.max(log_likelihood), coef]]
    intcpt_exp <- exp(intcpt)

    intcpt_info <- list(
        lnbr(intcpt, 2),
        lnbr(intcpt_exp, 3))

    ## predicted impact of tax deductibility, just visually cause no time for proper prediction
    txdctblt_cbn2 <- top_coefs[vrbl_name_unlag == "Ind.tax.incentives" & cbn_name == "cbn_no_cult_spending",
              .SD[which.max(log_likelihood), coef]]
    tmitr_cbn2 <- top_coefs[vrbl_name_unlag == "tmitr_approx_linear20step" & cbn_name == "cbn_no_cult_spending",
                            .SD[which.max(log_likelihood), coef]]
        
    txdctblt_tmitr_interact_cbn2 <- top_coefs[vrbl_name_unlag == "ti_tmitr_interact" &
                                         cbn_name == "cbn_no_cult_spending",
                                         .SD[which.max(log_likelihood), coef]]

    txdctblt_tmitr_interact_cbn2_exp <- exp(txdctblt_tmitr_interact_cbn2)

    txdctblt_cbn3 <- top_coefs[vrbl_name_unlag == "Ind.tax.incentives" &
                               cbn_name == "cbn_no_cult_spending_and_mitr",
                               .SD[which.max(log_likelihood), coef]]
    txdctblt_cbn3_exp <- exp(txdctblt_cbn3)

    ## top_coefs[, .N, vrbl_name_unlag] %>% print(n=200)
    
    tmitr_scale_cbn2 <- scale(cbn_dfs_rates_uscld$cbn_no_cult_spending$tmitr_approx_linear20step_lag0)
    tmitr_1SD_cbn_no_cult_spending <- attr(tmitr_scale_cbn2, "scaled:scale")
    
    tmitr_neteffct_txdctblt1_cbn_no_cult_spending <- tmitr_cbn2 + txdctblt_tmitr_interact_cbn2
    tmitr_neteffct_txdctblt1_cbn_no_cult_spending_exp <- exp(tmitr_neteffct_txdctblt1_cbn_no_cult_spending)

    
    dt_ti_pred <- expand.grid(tax_ddctblt = c(0,1),
                              tmitr = seq(round(min(tmitr_scale_cbn2),2),
                                          round(max(tmitr_scale_cbn2),2), 0.05)) %>% adt() %>%
        .[, pred := txdctblt_cbn2 * tax_ddctblt + tmitr_cbn2 *tmitr +
                txdctblt_tmitr_interact_cbn2 * tax_ddctblt * tmitr]

    ggplot(dt_ti_pred, aes(x=tmitr, y=pred, color = factor(tax_ddctblt))) +
        geom_line()

    ## some more comparison
    ## CHL <- txdctblt_cbn2 * 1 + tmitr_cbn2 * 0 + txdctblt_tmitr_interact_cbn2 * 1 * 0 # "Chile"
    ## NLD <- txdctblt_cbn2 * 1 + tmitr_cbn2 * 1 + txdctblt_tmitr_interact_cbn2 * 1 * 1 #

    ## NLD-CHL
    ## y = x0 + a + b + x
    ## x0: y if a and b are 0
    ## a: where b = 0, difference between a = 0 and a = 1
    ## at mean TMITR, countries with txdctblt have 0.48 higher log-rates
    ## b: where a = 0, difference between b = 0 and b = 1
    ## at group with no tax deductibility of donations, countries with tmitr = 1 have -0.38 lower log rate than countries with tmitr = 0
    ## interaction of x shows that 1 unit increase of a/b increases effect of b/a on y by x
    ## interaction x: difference in effectiveness of tmitr between countries with tax deductibility and those without (1 unit change in tax deductibility)
    ## in countries with tax deductibility, the effect of 1 unit increase (1SD) tmitr is 0.77 larger
    
    ## rbind(
    ##     dt_ti_pred[tax_ddctblt == 1 & tmitr %in% c(0,1)] %>%
    ##     dcast.data.table(tax_ddctblt ~ tmitr, value.var = "pred"),
    ##     dt_ti_pred[tax_ddctblt == 0 & tmitr %in% c(0,1)] %>%
    ##     dcast.data.table(tax_ddctblt ~ tmitr, value.var = "pred")) %>%
    ##     .[, diff := `1` - `0`] %>%
    ##     dcast.data.table(.~tax_ddctblt, value.var = "diff") %>%
    ##     .[, diff2 := `1` - `0`]
        

    
    dt_tmitr_exmpl <- adt(cbn_dfs_rates_uscld$cbn_no_cult_spending) %>%
        .[year == 2020, .(iso3c, tmitr = tmitr_approx_linear20step_lag0, mrg = "mrg")] %>% 
        .[., on = "mrg", allow.cartesian = T] %>%
        ## .[iso3c > i.iso3c] %>%
        .[, diff := tmitr - i.tmitr] %>%
        .[diff > tmitr_1SD_cbn_no_cult_spending * 0.95 & diff < tmitr_1SD_cbn_no_cult_spending*1.05]
        
    if (print_examples) print(dt_tmitr_exmpl, n = 200)

    tmitr_cprn_iso3c1 <- "NLD"
    tmitr_cprn_iso3c2 <- "CHL"

    dt_tmitr_exmpl_fltrd <- dt_tmitr_exmpl[iso3c == tmitr_cprn_iso3c1 & i.iso3c == tmitr_cprn_iso3c2]
    tmitr_iso3c1 <- dt_tmitr_exmpl_fltrd$tmitr
    tmitr_iso3c2 <- dt_tmitr_exmpl_fltrd$i.tmitr


    txinctvs <- list(
        lnbr(txdctblt_cbn3, 2),
        lnbr(txdctblt_cbn3_exp, 1),
        lnbr(txdctblt_cbn2, 2),
        lnbr(tmitr_cbn2,2),
        lnbr(txdctblt_tmitr_interact_cbn2, 2),
        lnbr(txdctblt_tmitr_interact_cbn2_exp, 2),
        lnbr(tmitr_1SD_cbn_no_cult_spending, 1),
        lnbr(tmitr_neteffct_txdctblt1_cbn_no_cult_spending,2),
        lnbr(tmitr_neteffct_txdctblt1_cbn_no_cult_spending_exp,2),
        lnbr(tmitr_iso3c1, 0),
        lnbr(tmitr_iso3c2, 0))


    ## predicted numbers for government spending 

    ## hist(cbn_dfs_rates$cbn_all$smorc_dollar_fxm_lag0)

    smorc_lin <- top_coefs[vrbl_name_unlag == "smorc_dollar_fxm", .SD[which.max(log_likelihood), coef]]
    smorc_lin_flipped <- -smorc_lin
    smorc_sqrd <- top_coefs[vrbl_name_unlag == "smorc_dollar_fxm_sqrd", .SD[which.max(log_likelihood), coef]]
    
    smorc_top_point_std <- smorc_lin_flipped/(2*smorc_sqrd)

    smorc_scale <- scale(cbn_dfs_rates_uscld$cbn_all$smorc_dollar_fxm_lag0)
    
    smorc_top_point <- (smorc_top_point_std * attr(smorc_scale, "scaled:scale")) +
        attr(smorc_scale, "scaled:center")

    ## plotting predicted values (
    expand.grid(gvt_spending = seq(min(smorc_scale), max(smorc_scale), by = 0.01)) %>% adt() %>%
        .[, gvt_spending_sqrd := gvt_spending^2] %>%
        .[, pred := smorc_lin * gvt_spending + smorc_sqrd * gvt_spending_sqrd] %>% 
        ## .[, .SD[which.max(pred)]]
        ggplot(aes(x=gvt_spending, y = pred)) +
        geom_line()
    
    ## get countries around smorc_top_point (0.2 around), also at least 1.4 of those

    smorc_vlus_2020 <- filter(cbn_dfs_rates_uscld$cbn_all, year == 2020) %>% 
        filter((smorc_dollar_fxm_lag0 < smorc_top_point * 1.2 & smorc_dollar_fxm_lag0 > smorc_top_point * 0.8) |
               smorc_dollar_fxm_lag0 > smorc_top_point * 1.4) %>%
        select(iso3c, smorc_dollar_fxm_lag0) %>% arrange(smorc_dollar_fxm_lag0) %>% adt() %>%
        ## converting to list in lnbr format with apply 
        apply(1, \(x) list(nbr_name = paste0("smorc_2020_", x[["iso3c"]]),
                           nbr = as.numeric(x[["smorc_dollar_fxm_lag0"]]),
                           digits = 0))
    
    
    
        

 ## %$%
 ##        setNames(smorc_dollar_fxm_lag0, paste0("smorc_2020_", iso3c))

    ## get statistics of how many CY are above/below smorc_top_point
    smorc_top_point_stats <- adt(cbn_dfs_rates_uscld$cbn_all)[
      , above_smorc_top_point := ifelse(smorc_dollar_fxm_lag0 > smorc_top_point, "above", "below")] %>%
        adt(df_reg)[, .(iso3c, year, nbr_opened)][., on = .(iso3c, year)] %>% 
        .[, .(CYs = .N*1.0, nbr_opened = sum(nbr_opened)), above_smorc_top_point] %>%
        melt(id.vars = "above_smorc_top_point", value.name = "cnt") %>% 
        .[, prop := (cnt/sum(cnt))*100, variable] %>% 
        melt(id.vars = c("above_smorc_top_point", "variable"), variable.name = "measure") %>%
        .[, digits := fifelse(measure == "cnt", 0, 1)] %>%
        apply(1, \(x) list(
                          nbr_name = sprintf("smorctop_%s_%s_%s", x[["above_smorc_top_point"]], x[["variable"]],
                                             x[["measure"]]),
                          nbr = as.numeric(x[["value"]]),
                          digits = as.numeric(x[["digits"]])))

    
        ## .[measure == "cnt", value_fmt := fmt_nbr_flex(value, 0)] %>% # counts get formated with zero digits
        ## .[measure == "prop", value_fmt := fmt_nbr_flex(value*100, 1)] # proportions with 1
        ## .[, name_fmt := sprintf("smorctop_%s_%s_%s", above_smorc_top_point, variable, measure)] %$% 
        ## setNames(value_fmt, name_fmt)

    smorc_stats <- list(
        lnbr(smorc_lin, 2),
        lnbr(smorc_lin_flipped, 2),
        lnbr(smorc_sqrd, 2),
        lnbr(smorc_top_point_std, 2),
        lnbr(smorc_top_point,0))
        

    ## comparison for inequality variables
    ## cross-sectional 2020 change for shweal992j_p90p100_lag0

    shweal_scale <- scale(cbn_dfs_rates_uscld$cbn_all$shweal992j_p90p100_lag0)
    shweal_1SD_cbn_all <- attr(shweal_scale, "scaled:scale")

    ## inspect which countries are interesting to compare
    dt_shweal_cprn <- filter(cbn_dfs_rates_uscld$cbn_all, year == 2020) %>%
        select(iso3c, shweal992j_p90p100_lag0) %>% adt() %>%
        .[, mrg := "a"] %>% .[., on = "mrg", allow.cartesian = T] %>% # self-join to get country-comparisons
        .[, .(iso3c_1 = iso3c, iso3c_2 = i.iso3c,
              shweal1 = shweal992j_p90p100_lag0, shweal2 = i.shweal992j_p90p100_lag0)] %>%
        .[, diff := shweal2 - shweal1] %>%
        .[diff > shweal_1SD_cbn_all *0.97 & diff < shweal_1SD_cbn_all *1.03]

    if (print_examples) print(dt_shweal_cprn, n=200)

    ## select some countries after inspection
    shweal_cprn_iso3c1 <- "DNK"
    shweal_cprn_iso3c2 <- "CZE"

    ## check there's data on them 
    dt_shweal_cprn_fltrd <- dt_shweal_cprn[iso3c_1 == shweal_cprn_iso3c1 &  iso3c_2 == shweal_cprn_iso3c2]
    if (nrow(dt_shweal_cprn_fltrd) != 1) {stop("wealth comparison doesn't have exactly one row")}

    shweal_iso3c1 <- dt_shweal_cprn_fltrd$shweal1
    shweal_iso3c2 <- dt_shweal_cprn_fltrd$shweal2
    
    ## longitudinal comparison with self-join
    

    dt_shweal_cprn_lngtd <- get_wealth_ineq("p90p100", WID_VX) %>% adt() %>%
        .[, .(iso3c, year, shweal = shweal992j_p90p100*100)] %>% 
        ## adt(cbn_dfs_rates_uscld$cbn_all)[, .(iso3c, year, shweal = shweal992j_p90p100_lag0)] %>%
        .[., on = "iso3c", allow.cartesian = T] %>%
        .[i.year > year] %>%
        .[, diff := i.shweal - shweal] %>%
        ## .[, max(diff)]
        ## .[iso3c == "USA" & year == 1991] %>% print(n=200)
        ## ggplot(aes(x=diff, y = ..density..)) + geom_density()
        .[diff > shweal_1SD_cbn_all *0.97 & diff < shweal_1SD_cbn_all*1.03]

    if (print_examples) print(dt_shweal_cprn_lngtd, n = 2000)
        

    ## select country and years
    shweal_iso_lngtd <- "USA"
    shweal_lngtd_year1 <- 1991
    shweal_lngtd_year2 <- 2011
    

    dt_shweal_cprn_lngtd_fltrd <- dt_shweal_cprn_lngtd[iso3c == shweal_iso_lngtd &
                                                       year == shweal_lngtd_year1 & i.year == shweal_lngtd_year2]

    if (nrow(dt_shweal_cprn_lngtd_fltrd) != 1) {stop("dt_shweal_cprn_lngtd_fltrd doesn't have exactly 1 row")}

    shweal_lngtd_vlu_year1 <- dt_shweal_cprn_lngtd_fltrd$shweal
    shweal_lngtd_vlu_year2 <- dt_shweal_cprn_lngtd_fltrd$i.shweal

    shweal_cbn_all <- top_coefs[vrbl_name_unlag == "shweal992j_p90p100" & cbn_name == "cbn_all",
                                .SD[which.max(log_likelihood), coef]]
    shweal_cbn_all_exp <- exp(shweal_cbn_all)

    shweal_stats <- list(
        lnbr(shweal_1SD_cbn_all, 1),
        lnbr(shweal_iso3c1, 1),
        lnbr(shweal_iso3c2, 1),
        lnbr(shweal_lngtd_vlu_year1,1),
        lnbr(shweal_lngtd_vlu_year2,1),
        lnbr(shweal_cbn_all, 2),
        lnbr(shweal_cbn_all_exp, 2))


    ## income inequality 
    sptinc_scale <- scale(cbn_dfs_rates_uscld$cbn_all$sptinc992j_p90p100_lag0)
    sptinc_1SD_cbn_all <- attr(sptinc_scale, "scaled:scale")

    ## cross-sectional 2020 change for sptinc992j_p90p100_lag0
    dt_sptinc_cprn <- filter(cbn_dfs_rates_uscld$cbn_all, year == 2020) %>%
        select(iso3c, sptinc992j_p90p100_lag0) %>% adt() %>%
        .[, mrg := "a"] %>%
        .[., on = "mrg", allow.cartesian = T] %>%
        .[, .(iso3c_1 = iso3c, iso3c_2 = i.iso3c,
              sptinc1 = sptinc992j_p90p100_lag0, sptinc2 = i.sptinc992j_p90p100_lag0)] %>%
        .[, diff := sptinc2 - sptinc1] %>%
        .[diff > sptinc_1SD_cbn_all*0.97 & diff < sptinc_1SD_cbn_all*1.03]
    ## print(n=200)

    if (print_examples) print(dt_sptinc_cprn, n = 2000)

    sptinc_cbn_all <- top_coefs[vrbl_name_unlag == "sptinc992j_p90p100" & cbn_name == "cbn_all",
                                .SD[which.max(log_likelihood), coef]]
    sptinc_cbn_all_exp <- exp(sptinc_cbn_all)


    sptinc_cprn_iso3c1 <- "SWE"
    sptinc_cprn_iso3c2 <- "CAN"
    
    dt_sptinc_cprn_fltrd <- dt_sptinc_cprn[iso3c_1 == sptinc_cprn_iso3c1 & iso3c_2 == sptinc_cprn_iso3c2]

    if (nrow(dt_sptinc_cprn_fltrd) != 1) {stop("dt_sptinc_cprn_fltrd doesn't have exactly 1 row")}

    sptinc_iso3c1 <- dt_sptinc_cprn_fltrd$sptinc1
    sptinc_iso3c2 <- dt_sptinc_cprn_fltrd$sptinc2

    dt_sptinc_cprn_lngtd <- adt(cbn_dfs_rates_uscld$cbn_all)[, .(iso3c, year, sptinc = sptinc992j_p90p100_lag0)] %>%
        .[., on = "iso3c", allow.cartesian = T] %>%
        .[i.year > year] %>%
        .[, diff := i.sptinc - sptinc] %>%
        ## ggplot(aes(x=diff, y = ..density..)) + geom_density()
        .[diff > sptinc_1SD_cbn_all *0.95 & diff < sptinc_1SD_cbn_all*1.05] 
        ## print(n=200) 

    sptinc_iso_lngtd <- "LTU"
    sptinc_lngtd_year1 <- 2000
    sptinc_lngtd_year2 <- 2014

    dt_sptinc_cprn_lngtd_fltrd <- dt_sptinc_cprn_lngtd[iso3c == sptinc_iso_lngtd &
                                                       year == sptinc_lngtd_year1 & i.year == sptinc_lngtd_year2]
    
    sptinc_lngtd_vlu_year1 <- dt_sptinc_cprn_lngtd_fltrd$sptinc
    sptinc_lngtd_vlu_year2 <- dt_sptinc_cprn_lngtd_fltrd$i.sptinc
    
    
    ## income inequality hasn't actually increased much...
    ## adt(cbn_dfs_rates_uscld$cbn_all)[iso3c %in% c("DEU", "USA", "FRA", "CHN"),
    ##                                  .(iso3c, year, vlu = sptinc992j_p90p100_lag0)] %>%
    ##     ggplot(aes(x=year, y=vlu, color = iso3c)) +
    ##     geom_line()

    sptinc_stats <- list(
        lnbr(sptinc_1SD_cbn_all, 1),
        lnbr(sptinc_iso3c1, 1),
        lnbr(sptinc_iso3c2, 1),
        lnbr(sptinc_lngtd_vlu_year1, 1),
        lnbr(sptinc_lngtd_vlu_year2, 1),
        lnbr(sptinc_cbn_all,2),
        lnbr(sptinc_cbn_all_exp,2))

        
    ## density numbers
    pm_density_cbn_all <- top_coefs[vrbl_name_unlag == "pm_density" & cbn_name == "cbn_all",
                            .SD[which.max(log_likelihood), coef]]
    pm_density_sqrd_cbn_all <- top_coefs[vrbl_name_unlag == "pm_density_sqrd" & cbn_name == "cbn_all",
                               .SD[which.max(log_likelihood), coef]]

    pm_density_glbl_cbn_all <- top_coefs[vrbl_name_unlag == "pm_density_global" & cbn_name == "cbn_all",
                               .SD[which.max(log_likelihood), coef]]
    pm_density_glbl_sqrd_cbn_all <- top_coefs[vrbl_name_unlag == "pm_density_global_sqrd" & cbn_name == "cbn_all",
                                      .SD[which.max(log_likelihood), coef]]
    
    dens_cry_top_point_std_cbn_all <- -pm_density_cbn_all/(2*pm_density_sqrd_cbn_all)
    dens_glbl_top_point_std_cbn_all <- -pm_density_glbl_cbn_all/(2*pm_density_glbl_sqrd_cbn_all)
    
    
    dens_cry_scale <- scale(cbn_dfs_rates_uscld$cbn_all$pm_density_lag0)
    dens_glbl_scale <- scale(cbn_dfs_rates_uscld$cbn_all$pm_density_global_lag0)

    dens_cry_top_point_cbn_all <- (dens_cry_top_point_std_cbn_all * attr(dens_cry_scale, "scaled:scale")) +
        attr(dens_cry_scale, "scaled:center")

    dens_glbl_top_point_cbn_all <- (dens_glbl_top_point_std_cbn_all * attr(dens_glbl_scale, "scaled:scale")) +
        attr(dens_glbl_scale, "scaled:center")

    dens_coef_stats <- list(
        lnbr(pm_density_cbn_all, 2),
        lnbr(pm_density_sqrd_cbn_all, 2),
        lnbr(pm_density_glbl_cbn_all, 2),
        lnbr(pm_density_glbl_sqrd_cbn_all, 2),
        lnbr(dens_cry_top_point_std_cbn_all, 2),
        lnbr(dens_glbl_top_point_std_cbn_all, 2),
        lnbr(dens_cry_top_point_cbn_all, 2),
        lnbr(dens_glbl_top_point_cbn_all, 2))

    
        
    dens_cry_top_point_stats <- adt(cbn_dfs_rates_uscld$cbn_all)[
      , above_dens_cry_top_point := ifelse(pm_density_lag0 > dens_cry_top_point_cbn_all, "above", "below")] %>% 
        adt(df_reg)[, .(iso3c, year, nbr_opened)][., on = .(iso3c, year)] %>% # merge nbr_opened
        .[, .(CYs = .N*1.0, nbr_opened = sum(nbr_opened)), above_dens_cry_top_point]  %>%
        melt(id.vars = "above_dens_cry_top_point", value.name = "cnt") %>% 
        .[, prop := 100*cnt/sum(cnt), variable] %>% 
        melt(id.vars = c("above_dens_cry_top_point", "variable"), variable.name = "measure") %>%
        .[, digits := fifelse(measure == "cnt", 0, 1)] %>%
        .[, name_fmt := sprintf("dens_cry_top_%s_%s_%s", above_dens_cry_top_point, variable, measure)] %>%
        pmap(~with(list(...), list(nbr_name = name_fmt, nbr = value, digits = digits)))
        
    

    dens_glbl_top_point_stats <- adt(cbn_dfs_rates_uscld$cbn_all)[
      , above_dens_glbl_top_point := ifelse(pm_density_global_lag0 > dens_glbl_top_point_cbn_all, "above", "below")] %>%
        adt(df_reg)[, .(iso3c, year, nbr_opened)][., on = .(iso3c, year)] %>% 
        .[, .(CYs = .N*1.0, nbr_opened = sum(nbr_opened)), above_dens_glbl_top_point]  %>%
        melt(id.vars = "above_dens_glbl_top_point", value.name = "cnt") %>% 
        .[, prop := 100*cnt/sum(cnt), variable] %>% 
        melt(id.vars = c("above_dens_glbl_top_point", "variable"), variable.name = "measure") %>%
        .[, digits := fifelse(measure == "cnt", 0, 1)] %>%
        .[, name_fmt := sprintf("dens_glbl_top_%s_%s_%s", above_dens_glbl_top_point, variable, measure)] %>%
        pmap(~with(list(...), list(nbr_name = name_fmt, nbr = value, digits = digits)))

    ## fix the density measures first: fixed now 

    ## GDP stuff
    ## coefs
    gdp_stats1 <- copy(top_coefs)[vrbl_name_unlag == "NY.GDP.PCAP.CDk",
                    .SD[which.max(log_likelihood), .(coef)], cbn_name] %>%
        .[, name_fmt := paste0("gdp_coef_", cbn_name)] %>%
        pmap(~with(list(...), list(nbr_name = name_fmt, nbr = coef, digits = 2)))

    ## 1 SD 
    gdp_stats2 <- imap(cbn_dfs_rates_uscld[1:3], ~adt(.x) %$% scale(NY.GDP.PCAP.CDk_lag0) %>%
                                       attr(., "scaled:scale") %>% 
                                       list(nbr_name = paste0("gdp_1SD_", .y),
                                            nbr = .,
                                            digits = 2)) %>% unname()

    gdp_stats <- c(gdp_stats1, gdp_stats2)
    
        


    
    ## numbers that can get formated comfily with lnbr/fmt_nbr_flex
    l_res <- list(
        intcpt_info = intcpt_info,
        txinctvs = txinctvs,
        smorc_stats = smorc_stats,
        smorc_vlus_2020 = smorc_vlus_2020,
        smorc_top_point_stats = smorc_top_point_stats,
        shweal_stats = shweal_stats,
        sptinc_stats = sptinc_stats,
        dens_coef_stats = dens_coef_stats,
        dens_cry_top_point_stats = dens_cry_top_point_stats,
        dens_glbl_top_point_stats = dens_glbl_top_point_stats,
        gdp_stats = gdp_stats)
    
    dt_nbrs_pred_prep <- imap_dfr(l_res, ~rbindlist(.x)[, grp := .y]) %>%
        .[, nbr_fmt := fmt_nbr_flex(nbr, digits)] %>%
        .[, .(nbr_name, nbr_fmt, grp)]

    
    ## year numbers that get formated separately 
    dt_yearnbrs <- list(      
      c(lnbr(shweal_lngtd_year1, 0), list(grp = "shweal_stats")),
      c(lnbr(shweal_lngtd_year2, 0), list(grp = "shweal_stats")),
      c(lnbr(sptinc_lngtd_year1, 0), list(grp = "sptinc_stats")),
      c(lnbr(sptinc_lngtd_year2, 0), list(grp = "sptinc_stats"))) %>% rbindlist() %>%
        .[, .(nbr_name, nbr_fmt = as.character(nbr), grp)]

    dt_nbrs_pred <- rbind(dt_nbrs_pred_prep, dt_yearnbrs)

    if (dt_nbrs_pred[, .N, nbr_name][, max(N)] != 1) print("dt_nbrs_pred$nbr_name not unique")

    return(dt_nbrs_pred)


    
    ## return(res)
    
}
    
## gen_nbrs_pred(reg_res_objs$top_coefs, cbn_dfs_rates_uscld)



gen_nbrs <- function(df_excl, df_open, cbn_dfs_rates, cbn_dfs_rates_uscld,  df_reg_anls_cfgs_wide, df_reg,
                     dt_velp_scalars, dt_velp_crycoefs,
                     batch_version, print_examples = F, pltnames) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    dt_excl <- df_excl %>% adt() %>% .[, .(ID, museum_status, year_opened_int, year_closed, countrycode)]
    ## number of museums in database as a whole 
    nbr_muem_in_pmdb <- dt_excl[, .N]

    nbr_pm_openatm <- dt_excl[museum_status == "private museum", .N]

    ## number of opened/closed PMs (based on PMs with complete info)
    nbr_opnd_wld <- sum(df_open$nbr_opened, na.rm = T)
    ## nbr_clsd_wld <- ## sum(df_open$nbr_closed, na.rm = T)
    nbr_clsd_wld <- dt_excl[museum_status != "private museum", .N]
    ## number of those countries that have at least 1 pm, based on all PMs in database
    nbr_cry_wal1pm_all <- dt_excl[, uniqueN(na.omit(countrycode))]

    pmdb_stats <- list(nbr_muem_in_pmdb = nbr_muem_in_pmdb,
                       nbr_opnd_wld = nbr_opnd_wld,
                       nbr_clsd_wld = nbr_clsd_wld,
                       nbr_cry_wal1pm_all = nbr_cry_wal1pm_all,
                       nbr_pm_openatm = nbr_pm_openatm)


    ## number of those countries that have at least 1 pm, based on PMs with complete information
    ## nbr_cry_wal1pm_cplt <- df_open %>% adt() %>% .[, uniqueN(na.omit(iso3c))]

    ## debugging difference between nbr_cry_wal1pm_all and nbr_cry_wal1pm_cplt: HKG museum (ID = 7) with no info
    ## setdiff(dt_excl[, unique(na.omit(countrycode))], adt(df_open)[, unique(iso3c)])

    ## number of museums that have been opened per UN sub region
    nbr_pm_regsub <- dt_excl %>% copy() %>% .[, region := countrycode(countrycode, "iso3c", "un.regionsub.name",
                                                     custom_match = c("TWN" = "South-eastern Asia"))] %>%
        .[, .N, region] %>% .[order(-N)] %>% .[1:5] %$%
        setNames(N, paste0("regsub_cnt_", gsub(" ", "_", region)))
    
    
    ## combination info: nbr CYs, nbr countries, percentage of foundings covered
    cbn_info <- imap(cbn_dfs_rates[1:3],
         ~list(nbr_cy = nrow(.x),
               nbr_crys = n_distinct(.x$iso3c),
               nbr_opngs = sum(.x$nbr_opened),
               prop_opngs_cvrd = fmt_nbr_flex(round(sum(.x$nbr_opened, na.rm = T)/nbr_muem_in_pmdb,3), digits = 3)
               )) %>%
        rbindlist(idcol = T) %>% melt(id.vars = ".id") %>%
        .[, vrbl := paste0(variable, "_", .id)] %$%
        setNames(value, vrbl)

    ## look at within/between iso3c/dataset variation in year coverage, is both within/between
    ## imap_dfr(cbn_dfs_rates[1:3], ~adt(.x)[, .(N = .N, cbn = .y), iso3c]) %>%
    ##     .[, .(meanN = mean(N)), cbn]
    ##     ## xtsum(N, iso3c) %>% adt()
    ##     ## dcast.data.table(iso3c ~ cbn, value.var = "N") %>% na.omit() %>% print(n=200)
        
        
    ## average country-level rates
    
    opng_rates_vlus <- map(cbn_dfs_rates, ~adt(.x)[, mean(nbr_opened/SP_POP_TOTLm_lag0_uscld)])

    ## number of CYs with zero/non-zero
    opng_prop_vlus <- imap_dfr(cbn_dfs_rates, ~adt(.x)[, .(nbr_zero = 1.0*sum(nbr_opened == 0),
                                         nbr_nzero = 1.0*sum(nbr_opened != 0))] %>%
                                .[, `:=`(prop_nzero = nbr_nzero/(nbr_zero + nbr_nzero)*100)] %>% 
                                         melt(id.vars = NULL, measure.vars = names(.)) %>%
                                .[, cbn_name := .y]) %>% 
        .[, `:=`(nbr_name = paste0(variable, "_", cbn_name), value_fmt = fmt_nbr_flex(value, digits = 1))] %$%
        setNames(value_fmt, nbr_name)
        
                            
    opng_rates_fmt <- c(
        opng_rate_cbn1 = nicely_fmt_number(opng_rates_vlus$cbn_all,4),
        opng_rate_cbn2 = nicely_fmt_number(opng_rates_vlus$cbn_no_cult_spending,4),
        opng_rate_cbn3 = nicely_fmt_number(opng_rates_vlus$cbn_no_cult_spending_and_mitr,4)) %>% as.list()
        
    popnbrs_p1pm <- map(cbn_dfs_rates, ~adt(.x)[, 1/mean(nbr_opened/SP_POP_TOTLm_lag0_uscld)]) %>%
        nicely_fmt_number_v() %>% setNames(., paste0("nbr_pop_p_1pm_", names(.))) %>% as.list()


    ## global rates
    ## of all country years
    rate_opng_glbl <- map(cbn_dfs_rates, ~adt(.x)[, sum(nbr_opened)/sum(SP_POP_TOTLm_lag0_uscld)]) %>%
        imap(~list(nbr_name = paste0("rate_opng_glbl_", .y), nbr = .x, digits = 5))

    ## setNames(paste0("rate_opng_glbl_", names(.)))

    ## first aggregate to year 
    rate_opng_glbl_yearly <- map(cbn_dfs_rates, ~adt(.x)[, .(nbr_opened_year = sum(nbr_opened),
                                    pop_year = sum(SP_POP_TOTLm_lag0_uscld)), year][
                                                 , mean(nbr_opened_year/pop_year)]) %>%
        imap(~list(nbr_name = paste0("rate_opng_glbl_yearly_", .y), nbr = .x, digits = 5))

    opng_p1m_glbl_yearly <- map(rate_opng_glbl_yearly,
                                ~list(nbr_name = paste0("opng_p1m_glbl_yearly_cbn_",
                                                        str_split(.x$nbr_name,  "cbn_")[[1]][2]),
                                      nbr = 1/.x$nbr, digits = 0))

    
    ## model succesrate
    l_cvrgnc <- df_reg_anls_cfgs_wide %>% adt() %>%
        .[, .N, cvrgd] %$%
        setNames(N, paste0("cvrgd", cvrgd)) %>% as.list()
    
    l_cvrgnc$mdlcnt_ttl <- l_cvrgnc$cvrgd1 + l_cvrgnc$cvrgd0
    l_cvrgnc$cvrgnc_rate <- round((l_cvrgnc$cvrgd1 / l_cvrgnc$mdlcnt_ttl)*100,3)

    l_cvrgnc$nbr_runs_p_cbn_spec <- adt(df_reg_anls_cfgs_wide)[, .N, .(cbn_name, base_lag_spec)][, round(mean(N),0)]

    ## value ~ year development numbers (velp)
    

    ## should filter out only for min and max
    dt_velp_tmitr_minmax <- cbn_dfs_rates$cbn_all %>% adt() %>% .[, .N, iso3c] %>% .[N >= 20, .(iso3c)] %>%
        dt_velp_crycoefs[., on = "iso3c"] %>% # filter out countries with less than 20 CYs
        .[vrbl == "tmitr_approx_linear20step" & cbn_name == "cbn_all"]
    
    tmitr_scale <- scale(cbn_dfs_counts_uscld$cbn_all$tmitr_approx_linear20step_lag0) %>% attr("scaled:scale")

    ## generate minmax nbrs
    l_velp_minmax <- dt_velp_tmitr_minmax %>%
        .[year == min(year), src := "min"] %>% .[year == max(year), src := "max"] %>% na.omit() %>%         
        .[, .(cryname = countrycode(iso3c, "iso3c", "country.name"),
              year_scld = fmt_nbr_flex(year, 3), # coef (on standardized variable)
              year_pct = fmt_nbr_flex(abs(tmitr_scale * year), 1), # absolute percentage change
              src)] %>% 
        melt(id.vars = "src", variable.name = "measure") %$%
        setNames(value, paste0("velp_", measure , "_", src)) %>% as.list()


    ## get average and quantiles from entire slope dataset
    dt_velp_tmitr_ctrl <- copy(dt_velp_crycoefs)[vrbl == "tmitr_approx_linear20step" & cbn_name == "cbn_all"]

    l_velp_mean <- dt_velp_tmitr_ctrl[, .(mean = mean(year),
                      quantl25 = quantile(year, probs = c(0.25)),
                      quantl75 = quantile(year, probs = c(0.75)))] %>%
        melt(measure.vars = names(.), variable.name = "measure", value.name = "uscld") %>%
        .[, scld := abs(uscld * tmitr_scale)] %>% # absolute percentage, refer to "decline" in text
        melt(id.vars = "measure") %>%
        .[, .(nbr_name = paste0("velp_", measure, "_", variable), nbr = value)] %>%
        .[, digits := fifelse(grepl("uscld", nbr_name), 3,2)] %>% # uscld get three digits, scaled two (bc small)
        ## adf() %>% split.data.frame(seq(nrow(.)))
        split(by = "nbr_name") %>% map(as.list)
        
    
    ## overall coefs are average of country-coefs, no weird weighting going on
    ## dt_velp_crycoefs %>% copy() %>% .[, .(year_mean = mean(year)), .(cbn_name, vrbl)] %>%
        ## dt_velp_scalars[, .(cbn_name, vrbl, coef)][., on = .(cbn_name, vrbl)] %>%
        ## .[, diff := coef - year_mean] %$% hist(diff)
        


    ## generate the macros for the plot insertions
    dt_pltcfgs <- gen_plt_cfgs() %>% rbindlist() %>%
        .[, lbl := gsub(".pdf", "", filename)] %>%
        .[, pltname := paste0("plt_", lbl)] %>%
        .[pltname %in% pltnames] %>% # don't generate macro for unused plots
        .[, macro :=
                sprintf(paste0('(eval (concat "#+label: fig:%s\\n" "#+caption: %s\\n" ',
                               '"#+attr_latex: :width %scm\\n" "[[file:figures/%s]]"))'),
                        lbl,
                        caption,
                        width,
                        paste0("plt_", batch_version, "_", filename))] %>%
        .[, .(nbr_name = paste0("ynkplt_", lbl), nbr_fmt = macro, grp = "pltcfgs")]
        
    
    dt_pltlbls <- gen_plt_cfgs() %>% rbindlist() %>% 
        .[, core := gsub(".pdf", "", filename)] %>% 
        .[, .(nbr_fmt = sprintf("\\ref{fig:%s}", core), nbr_name=  paste0("rplt_", core), grp = "figlbls")] 
        
    dt_tbllbls <- gen_tblcfgs(TABLE_DIR) %>% rbindlist(idcol = "name") %>%
        .[, .(nbr_fmt = sprintf("\\ref{%s}", label), nbr_name = paste0("r", name), grp = "tbllbls")]

    dt_cfgs_lbls <- reduce(list(dt_pltcfgs, dt_pltlbls, dt_tbllbls), rbind)
    

    ## collect all results
    res_desc <- c(list(
        pmdb_stats = pmdb_stats,
        nbr_pm_regsub = nbr_pm_regsub,
        cbn_info = cbn_info,
        opng_rates_fmt = opng_rates_fmt,
        opng_prop_vlus = opng_prop_vlus,
        popnbrs_p1pm = popnbrs_p1pm,
        cvrgnc = lapply(l_cvrgnc, nicely_fmt_number),
        velp_minmax = l_velp_minmax))

    dt_nbrs_desc_prep <- imap_dfr(res_desc, ~data.table(nbr_name = names(.x),
                                                        nbr_fmt = as.character(unname(.x)), grp = .y)) 
        

    ## start replacing some numbers here with lnbr as well 
    lnbr_res <- list(
        rate_opng_glbl = rate_opng_glbl,
        rate_opng_glbl_yearly = rate_opng_glbl_yearly,
        opng_p1m_glbl_yearly = opng_p1m_glbl_yearly,
        velp_mean = l_velp_mean)

    dt_lnbr_res <- imap_dfr(lnbr_res, ~rbindlist(.x)[, grp := .y]) %>%
        .[, nbr_fmt := fmt_nbr_flex(nbr, digits)] %>%
        .[, .(nbr_name, nbr_fmt, grp)]

    dt_nbrs_desc <- rbind(dt_nbrs_desc_prep, dt_lnbr_res) %>%
        .[, nbr_name := factor(nbr_name, levels = nbr_name)]

    dt_nbrs_pred <- gen_nbrs_pred(reg_res_objs$top_coefs, cbn_dfs_rates_uscld, df_reg, print_examples)

    dt_res <- rbind(dt_nbrs_desc, dt_nbrs_pred, dt_cfgs_lbls)

    if (dt_res[, .N, nbr_name][, max(N)] != 1) {stop("nbr_name not not unique")}

    return(dt_res)

}

theme_orgpop <- function(extra_space_top=2) {
    ## pmdb theme for minimal layout
    
    ## theme_minimal() %+replace%

    base_size = 10
    half_line <- base_size/2

    ## ggplot() +
    ##     theme(axis.title

    theme(
        ## panel.grid.major = element_blank(),
        ## panel.background = element_rect(fill = "white"),
        ## axis.ticks = element_blank(),
        ## axis.ticks.length = unit(0, "pt"),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        strip.text.y.left = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 9),
        ## axis.text = element_blank(),
        plot.margin = unit(c(extra_space_top,2,2,2), "points")
    )
}



stop("functions done")

## ** main analysis
stylecfg <- list(lbl_fntsz = 9)

NBR_MDLS <- 1
batch_version <- "v88"
## fldr_info <- fldr_info_optmz
fldr_info <- setup_regression_folders_and_files(batch_version)
reg_anls_base <- read_reg_anls_files(fldr_info)
reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs, NBR_MDLS)



reg_res <- list()

## generate plots, construct configs
reg_res$plts <- gen_reg_res_plts(reg_res_objs, vvs, NBR_MDLS, only_priority_plts = T, stylecfg)

reg_res$plts$plt_cvrgnc <- gen_plt_cvrgnc(reg_res_objs$gof_df_cbn)
reg_res$plts$plt_cntrfctl <- gen_plt_cntrfctl(reg_res_objs$dt_cntrfctl_cons, reg_res_objs$dt_cntrfctl_wse)
render_reg_res("plt_cntrfctl", reg_res, batch_version = "v85")


## reg_res$plts$plt_oucoefchng_tile <- gen_plt_oucoefchng_tile(reg_res_objs$dt_oucoefchng)
## render_reg_res("plt_oucoefchng_tile", reg_res, batch_version = "v75")

plt_inspector(reg_res$plts)
## reg_res$plts$plt_oneout_llrt_z

purrr::map(names(reg_res$plts), ~render_reg_res(.x, reg_res, batch_version = batch_version))

## only render those plots that are generated (not all version plots)
pdftk_cmd <- sprintf("cd %s && pdftk %s output plts_%s.pdf", FIG_DIR,
                     paste0(paste0("plt_", batch_version, "_", gsub("plt_", "", names(reg_res$plts)), ".pdf"),
                            collapse = " "),
                     batch_version)
system(pdftk_cmd)

## tables
res_tbls <- gen_res_tbls(reg_res_objs)
pvxtbl(res_tbls$tbl_regrslts_wcptblF, landscape = T)
pvxtbl(res_tbls$tbl_descs, landscape = T)
pvxtbl(res_tbls$tbl_cbn_cpsgn, landscape = T)

iwalk(res_tbls, ~do.call("render_xtbl", c(.x, gen_tblcfgs(TABLE_DIR)[[.y]])))

## ** predicting

## gen_nbrs_pred(reg_res_objs$top_coefs, cbn_dfs_rates_uscld, df_reg, print_examples = F)

dt_nbrs <- gen_nbrs(df_excl, df_open, cbn_dfs_rates, cbn_dfs_rates_uscld,
                    reg_anls_base$df_reg_anls_cfgs_wide, df_reg,
                    reg_res_objs$dt_velp_scalars, reg_res_objs$dt_velp_crycoefs,
                    batch_version, print_examples = F,
                    names(reg_res$plts))

dt_nbrs %>% print(n=300)

## run again after v75, then I get all changes in one commit
fwrite(dt_nbrs, paste0(TABLE_DIR, "tbl_nbrs_", batch_version, ".csv"), quote = F)

dt_nbrs2 <- fread("/home/johannes/Dropbox/phd/papers/org_pop/tables/tbl_nbrs_v75.csv", quote = "")
tail(dt_nbrs2)

xtable(adt(mtcars)[1:3, .(mpg, cyl, disp)]) %>%
    print.xtable(floating = F, file = paste0(TABLE_DIR, "testtable.tex"))


mutate(cbn_dfs_rates$cbn_all, region = countrycode(iso3c, "iso3c", "un.region.name")) %>%
    viz_lines(y="ghweal992j_lag0", duration = 1, facets = "region", max_lines = 6)

## ** more version comparison 

library(patchwork)

reg_res62 <- gen_reg_res(setup_regression_folders_and_files("v62"))
reg_res63 <- gen_reg_res(setup_regression_folders_and_files("v63"))
reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))

reg_res63$plts$plt_best_models_condensed + reg_res64$plts$plt_best_models_condensed
reg_res63$plts$plt_reg_res_within + reg_res64$plts$plt_reg_res_within
reg_res63$plts$plt_best_models_wlag + reg_res64$plts$plt_best_models_wlag
reg_res63$plts$plt_coef_violin + reg_res64$plts$plt_coef_violin

    


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

## ** version comparison: compare v63 and v64 to v62

## compare log-likelihood distributions between v62 (all variables with up to 5 lags), v63 (all lags = 1),
## and v64 (some variable combinations, up to 3 lags)
dt_llcpr_long <- rbind(reg_res62$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v62"),
        by = .(cbn_name, vrbl_choice)],
      reg_res63$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v63"),
        by = .(cbn_name, vrbl_choice)],
      reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v64"),
        by = .(cbn_name, vrbl_choice)])

## actual comparison: first cast into wide
dt_llcpr_wide <- dt_llcpr_long %>% 
    dcast.data.table(cbn_name + vrbl_choice ~ version, value.var = "max_ll") %>%
    .[, `:=`(d63 = v62- v63,
             d64 = v62- v64)] 


dt_llcpr_long2 <- dt_llcpr_wide %>% 
    melt(id.vars = c("cbn_name", "vrbl_choice"), measure.vars = c("d63", "d64"))

## plot the differences from v63/v64 to v62
dt_llcpr_long2 %>%
    ggplot(aes(x=value, color = variable)) +
    geom_density()
    

dt_llcpr_long2[, .(mean_diff = mean(value, na.rm = T)), variable]
## d63           7.33
## d64           2.86



## ** evaluate model convergence consistency

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    group_by(regcmd, cbn_name, lag_spec) %>%
    summarize(nbr_mdls = len(mdl_id), nbr_unq_gof = n_distinct(gof_value)) %>%
    pull(nbr_unq_gof) %>% table()

## ** evaluate possible savings of better model caching 

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    ## mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>% 
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


    



    



