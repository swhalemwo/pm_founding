## * one out analysis

modfy_regspec_ou <- function(reg_spec, vrblset_oud, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' modify reg_spec for one-out calculations:
    #' one-out one/multiple variable(s) (oud = one-out-ed),
    #' usually just drop one, but also allow option to drop multiple for interactions/squared variable handling
    
    ## reg_spec
    
    ## vrbl_oud <- "hnwi_nbr_1M_lag1"
    
    ou_set_title <- names(vrblset_oud)
    vrbls_oud <- vrblset_oud[[ou_set_title]]
    

    reg_spec_mod <- reg_spec
    
    reg_spec_mod$mdl_vars <- setdiff(reg_spec_mod$mdl_vars, vrbls_oud)

    ## add ou set title and original ID to cfg
    reg_spec_mod$cfg <- c(reg_spec_mod$cfg,
                          list(ou_set_title = ou_set_title))
                               
    
    id_updtd <- gen_mdl_id(reg_spec_mod, vvs)
    ## id_updtd$mdl_id
    ## nchar(id_updtd$mdl_id)

    ## add ID of original reg_spec (all variables): nonou = non-one-out

    other_cfgs_w_nonou_id <- rbind(id_updtd$other_cfgs,
                                   data.frame(variable = "nonou_id",
                                              value = reg_spec$mdl_id,
                                              cfg_id = id_updtd$other_cfgs$cfg_id[1],
                                              lag_spec = id_updtd$other_cfgs$lag_spec[1],
                                              mdl_id = id_updtd$other_cfgs$mdl_id[1]))

    reg_spec_mod$other_cfgs <- other_cfgs_w_nonou_id
    reg_spec_mod$mdl_id <- id_updtd$mdl_id
    reg_spec_mod$df_idx <- id_updtd$df_idx

    return(reg_spec_mod)
   
}

## modfy_regspec_ou(reg_specx, vrbl_oud = "hnwi_nbr_1M_lag4", vvs)

gen_vrbl_oud <- function(vrbl, vrbls) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' generate the variables to exclude

    vrbl_unlag <- gsub("_lag[1-5]", "", vrbl)

    ## if vrbl is about tax incentives, also exclude the interaction
    if (vrbl == "Ind.tax.incentives" | vrbl_unlag == "tmitr_approx_linear20step") {
        oud <- c(vrbl, keep(vrbls, ~grepl("ti_tmitr_interact", .x)))
    }

    ## matching for squared variables
    ## if there's a version with the squared information, drop that too 
    else if (any(grepl(paste0(vrbl_unlag, "_sqrd"), vrbls))) {
        oud <- c(vrbl, keep(vrbls, ~grepl(paste0(vrbl_unlag, "_sqrd"), .x)))
    }
    else {
        oud <- vrbl
    }
    
    return(oud)
}

gen_regspecs_ou <- function(reg_spec, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' generate the modified reg_specs from one reg_spec (drop each variable once)

    vrbls_to_ou <- setdiff(reg_spec$mdl_vars, vvs$base_vars)
    
    ## generate one-out sets: yeet squared/interaction variables
    ou_sets <- lapply(vrbls_to_ou, \(x) gen_vrbl_oud(x, vrbls_to_ou))
    names(ou_sets) <- vrbls_to_ou

    ## add some manual sets of variables to exclude: 
    ## - all density
    ## - all density + closing
    ou_sets2 <- c(ou_sets,
                  list(all_dens = keep(vrbls_to_ou, ~grepl("density", .x)),
                       all_dens_close = c(keep(vrbls_to_ou, ~grepl("density", .x)),
                                          keep(vrbls_to_ou, ~grepl("nbr_closed_cum", .x)))))

    ## modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2["all_dens"], vvs)
    
    ## map(ou_sets2, ~modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2["pm_density_global_sqrd_lag2"], vvs)
    ## for some reason imap doesn't eval .y in list
    ## also imap doesn't have more options.. 
    ## imap(ou_sets2, ~modfy_regspec_ou(reg_spec, vrblset_oud = list(.y = .x), vvs))
    
    ## just use conventional query over keys
    regspecs_ou <- map(names(ou_sets2), ~modfy_regspec_ou(reg_spec, vrblset_oud = ou_sets2[.x], vvs))

    return(regspecs_ou)

    
}

one_out_setup <- function(batch_nbr) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ## reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))
    reg_res <- gen_reg_res(setup_regression_folders_and_files(batch_nbr))

    ## get the best fitting models
    ## mdl_idx <- reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
    ## .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
    ##   .SDcols = "mdl_id"] %>%
    ## .[5, mdl_id]

    ## get data for the comparision: mdl_id for joining, LL and df for LLRT
    mdl_id_dt_prep <- reg_res$reg_res_objs$gof_df_cbn %>% adt() %>%
        .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
          .SDcols = c("mdl_id", "gof_value")] %>%
        .[, setnames(.SD, "gof_value", "log_likelihood")]

    mdl_id_dt <- reg_res$reg_res_objs$gof_df_cbn %>% adt() %>%
        .[gof_names == "df", .(mdl_id, df = gof_value)] %>% 
        .[mdl_id_dt_prep, on = "mdl_id"]


    ## reconstruct folder info to read the reg_spec of model 

    fldr_info <- setup_regression_folders_and_files(batch_nbr)
    ## set up ou (one-out) folder for new regression results
    ## first get /ou/ folder in v64
    ou_cmd <- paste0("mkdir /home/johannes/reg_res/", batch_nbr, "/ou/") %>% system()
    ## then setup new folders there
    ## fldr_info_ou <- setup_regression_folders_and_files("v64ou", batch_dir_addgn = "v64/ou/")
    fldr_info_ou <- setup_regression_folders_and_files(batch_version = paste0(batch_nbr, "ou"),
                                                       batch_dir_addgn = paste0(batch_nbr, "/ou/"))

    ## get original reg_spec to modify 
    reg_specs_orig <- mclapply(mdl_id_dt[, mdl_id],
                               \(x) get_reg_spec_from_id(x, fldr_info = fldr_info), mc.cores = 4)

    ## modify original regspecs to make them work with one-out
    regspecs_ou <- mclapply(reg_specs_orig, \(x) gen_regspecs_ou(x, vvs), mc.cores = 4) %>% flatten()
    len(regspecs_ou)
    
    ## run modified regspecs
    mclapply(regspecs_ou, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou), mc.cores = 4)

    ## read-in ou results
    reg_res_ou <- gen_reg_res(fldr_info_ou)

    ## compare with previous results
    ## first get the df and LL for all the OU models (df for LLRT)
    dt_ou_base <- reg_res_ou$reg_res_objs$gof_df_cbn %>% adt() %>%
        .[gof_names %in% c("log_likelihood", "df"),
          .(ou_set_title, gof_name = gof_names, gof_value_ou = gof_value, mdl_id_ou = mdl_id,  nonou_id)] %>%
        .[, gof_name := paste0(gof_name, "_ou")] %>% 
        dcast.data.table(ou_set_title + mdl_id_ou + nonou_id ~ gof_name, value.var = "gof_value_ou")

    ## check that there are no duplicate names 
    check_df_name_unqns(list(dt_ou_base, mdl_id_dt), skip_var_names = c())
    

    ou_anls <- copy(mdl_id_dt)[dt_ou_base, on = .(mdl_id = nonou_id)] %>%
        .[, ou_set_title_unlag := gsub("_lag[1-5]", "", ou_set_title)] %>% # remove lag info
        .[, `:=`(log_likelihood_diff = log_likelihood - log_likelihood_ou,
                 df_diff = df - df_ou)] %>% # calc diffs in LL and df 
        .[, llrt_vlu := -2 * (log_likelihood_ou - log_likelihood)] %>% # start with LLRT
        .[llrt_vlu > 0] %>% # FIXME: due to changed data some LLRT_vlus are negative
        .[, llrt_p := pchisq(llrt_vlu, df = df_diff, lower.tail = F)] %>% # LLRT p-value
        .[, `:=`(sig = llrt_p < 0.05,
                 z = qnorm(llrt_p/2, lower.tail = F))] # just plug chi2-based p-values into normal dist
        ## ggplot(aes(x=llrt_p)) +
        ## geom_density()
        
    
    ## check that all models are complex enough 
    ## if (ou_anls[, min(df_diff)] == 0) {stop("some one-out models are not simpler")}
    
    ## add hypothesis coding
    ou_anls2 <- vvs$hyp_mep_dt %>% copy() %>%
        ## .[copy(ou_anls)[, ou_set_title_unlag2 := ou_set_title_unlag], # preserve to have keep it later 
        .[copy(ou_anls), on = .(vrbl = ou_set_title_unlag)]%>%
        .[, ou_set_title_unlag := vrbl] # other way to keep ou_set_title_unlag: recreate from vrbl
    
        
        
    ## violin plot
    ou_anls %>%
        ggplot(aes(x=log_likelihood_diff, y = ou_set_title_unlag)) +
        geom_violin() + 
        facet_grid(~cbn_name, scales = "free", space = "free")    

    
    reg_res$plts$plt_best_models_condensed

    ## point plot
    ou_anls2 %>%
        ggplot(aes(x=z, y = ou_set_title_unlag, shape = sig, color = sig)) +
        geom_jitter(size = 2, height = 0.2, width = 0) + 
        facet_grid(hyp~cbn_name, scales = "free", space = "free") +
        scale_shape_manual(values = c(1,4)) +
        geom_vline(xintercept = 1.96, linetype = "dashed")
        
        
    ou_anls %>%
        ggplot(aes(x=log_likelihood_diff, y = ou_set_title_unlag, shape = cbn_name, color = cbn_name)) +
        ## geom_point() +
        geom_boxplot() 
        ## facet_grid(ou_set_title_unlag ~ ., scales = "free", space = "free")
        
    ou_anls %>% copy() %>%
        .[, .(mean_gof_diff = mean(log_likelihood_diff)), by = .(ou_set_title_unlag, cbn_name)] %>%
        ggplot(aes(x=mean_gof_diff, y = ou_set_title_unlag, shape = cbn_name, color = cbn_name)) +
        geom_point(size =3)
        
    

}




stop("functions are done")

one_out_setup("v62")



## ** garage





## ------------------------
## ** debugging mysterious case of model improving after dropping variable
## can go back into one_out_setup when results are there
mys_dt <- ou_anls[ou_set_title_unlag == "hnwi_nbr_1M" & gof_diff < -3, ][1]
mys_id <- mys_dt$mdl_id_ou
mys_id_orig <- mys_dt$mdl_id

regspec_mys <- get_reg_spec_from_id(mys_id, fldr_info = fldr_info_ou)
regspec_mys_orig <- get_reg_spec_from_id(mys_id_orig, fldr_info = fldr_info)

## regspec_mys$cfg$cbn_name
## regspec_mys_orig$cfg$cbn_name

r_mys <- run_vrbl_mdl_vars(regspec_mys, vvs, fldr_info_ou, return_objs = "res_parsed",  wtf = F)
r_mys_orig <- run_vrbl_mdl_vars(regspec_mys_orig, vvs, fldr_info_ou, return_objs = "res_parsed", wtf = F)

r_mys$res_parsed
r_mys_orig$res_parsed

r_mys$res_parsed$gof_df %>% adt() %>% .[gof_names == "log_likelihood", gof_value] - 
    r_mys_orig$res_parsed$gof_df %>% adt() %>% .[gof_names == "log_likelihood", gof_value]

sapply(regspecs_ou, \(x) x$mdl_id) %>% unique() %>% len()

read_reg_res(mys_id, fldr_info_ou)
r62_old <- read_reg_res(mys_id_orig, fldr_info)$gof_df %>% adt() %>% .[, mdl_id := NULL] %>%
                                              .[, .(gof_names, gof_value_old = gof_value)]

r_mys_orig$res_parsed$gof_df %>% adt() %>%
    .[r62_old, on = "gof_names"] %>% 
    .[, gof_diff := gof_value - gof_value_old]


logLik(list(log_lik = -30, df = 4))

    
