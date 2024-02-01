args <- commandArgs(trailingOnly = T)
options(width = 115)


## * postestimation funcs 



gen_preds_given_mdfd_vrbls <- function(idx, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' general prediction of DV under changed IVs
    #' for now only has each IV being constant overall

    ## example, wrap that into own function later
    regspecx <- get_reg_spec_from_id(idx, fldr_info)
    
    dfx <- chuck(cbn_df_dict, "rates", chuck(regspecx, "cfg", "cbn_name"))
    iv_vars <- keep(setdiff(regspecx$mdl_vars, vvs$base_vars), ~!grepl("^SP\\.POP\\.TOTLm", .x))

    ## generate formula
    fx <- gen_r_f("rates", iv_vars)
    

    ## generate model
    
    rx <- glmmTMB(fx, dfx, family = nbinom1)
    
    ## fit model by country
    ## ehh idk this kinda assumes every slope is random
    ## rx_deu <- glmmTMB(fx, adt(dfx)[iso3c == "DNK"], family = nbinom2)

    ## fx2 <- sprintf("nbr_opened ~ %s + offset(log(SP_POP_TOTLm_lag0_uscld))",
    ##                paste0(iv_vars, collapse = " + ")) %>% as.formula()
    ## rx2 <- glmmTMB(fx2, dfx, family = nbinom2)

    ## compare_models(rx,rx2)
    ## compare_performance(rx,rx2)

    ## generate the variables that are to be modifed: filter with vvs: need to remove lag first.. 
    iv_vars_unlag <- gsub("_lag[1-5]", "", iv_vars) %>%
        setdiff(vvs$density_vars) %>%
        setdiff(vvs$crscn_vars) %>%
        keep(~!grepl("_sqrd", .x) & !grepl("_interact", .x))
        
    ## recover lag
    vrbls_lagged <- adt(regspecx$lngtd_vrbls)[data.table(vrbl = iv_vars_unlag), on = "vrbl"] %>%
        .[, paste0(vrbl, "_lag", lag)]

    
    ## ## vrblx <- "hnwi_nbr_1M_lag4"
    ## vrblx <- "hnwi_nbr_30M_lag2"
    ## ## vrblx <- "smorc_dollar_fxm_lag3"
    ## vrblx <- "sptinc992j_p90p100_lag3"
    ## ## vrblx <- "shweal992j_p90p100_lag4"
    ## ## vrblx <- "tmitr_approx_linear20step_lag5"
    ## ## sd(dfx$sptinc992j_p90p100_lag3)
    

    ## collect all values
    l_predres_cons <- map(vrbls_lagged, ~pred_given_const_vrbl(.x, rx, dfx, iv_vars))

    ## collect summary stats
    dt_predres_cons <- map(l_predres_cons, ~chuck(.x, "dtx_vlus")) %>% rbindlist() %>%
        .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]

    dt_predres_wse <- map(l_predres_cons, ~chuck(.x, "dtx_wse")) %>% rbindlist() %>%
        .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]

    

    ## dt_predres_cons %>% copy() %>%
    ##     .[dt_id %!in% c("minusone", "all")] %>% 
    ##     .[, diff := nbr_opened - pred] %>% 
    ##     ggplot(aes(x=diff, y=vrbl, color = dt_id)) + geom_point()

    ## map(l_predres_cons, ~chuck(.x, "dtx_crydiff")) %>% rbindlist() %>%
    ##     ## .[iso3c %in% c("KOR", "USA", "DEU")] %>%
    ##     .[abs(diff) > 3] %>% 
    ##     ## .[order(diff)] %>% .[, iso3c := factor(iso3c)] %>% 
    ##     ggplot(aes(x=diff, y=vrblx, color = iso3c, label = iso3c)) + geom_point() + geom_text_repel()



    ## ## actually run the predictions
    ## l_predres <- list(
    ##     dt_predres_cons = map(vrbls_lagged, ~pred_given_const_vrbl(.x, rx, dfx, iv_vars) %>%
    ##                                             chuck("l_pred_vlus")) %>% rbindlist() %>%
    ##         .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]
    ##     ## dt_predres_sdrange = map(vrbls_lagged, ~pred_sdrange(.x, rx, dfx, iv_vars)) %>% rbindlist() %>%
    ##     ##     .[, `:=`(mdl_id = idx, cbn_name = chuck(regspecx, "cfg", "cbn_name"))]
    ## )

    
    ## l_predres$dt_predres_sdrange %>%
    ##     ggplot(aes(x=updown, y=pred, color = vrblx)) + geom_line()

    ## margins(rx)
    ## x <- marginal_effects(rx) %>% adt()

    ## x[, map(.SD, mean)] %>% melt()


    ## return(l_predres$dt_predres_cons)
    return(list(
        dt_predres_cons = dt_predres_cons,
        dt_predres_wse = dt_predres_wse))
    
    

}


gen_cntrfctl <- function(gof_df_cbn, fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## get best models
    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)

    idx <- mdl_id_dt$mdl_id[5]
    t1 <- Sys.time()
    jj <- gen_preds_given_mdfd_vrbls(idx, fldr_info)
    t2 <- Sys.time()
    t2-t1
    
    l_cntrfctl_res <- mclapply(mdl_id_dt$mdl_id, \(x) gen_preds_given_mdfd_vrbls(x, fldr_info),
                               mc.cores = NBR_THREADS)

    dt_cntrfctl_cons <- map(l_cntrfctl_res, ~chuck(.x, "dt_predres_cons")) %>% rbindlist()

    ## collect model-country-year specific predictions + SEs
    dt_cntrfctl_wse <-  map(l_cntrfctl_res, ~chuck(.x, "dt_predres_wse")) %>% rbindlist()
     
    fwrite(dt_cntrfctl_cons, paste0(fldr_info$BATCH_DIR, "cntrfctl_cons.csv"))

    fwrite(dt_cntrfctl_wse, paste0(fldr_info$BATCH_DIR, "cntrfctl_wse.csv"))

    ## dt_cntrfctl_res <- fread(paste0(fldr_info$BATCH_DIR, "cntrfctl_res.csv"))
    ## dt_cntrfctl_res[mdl_id == idx]

    

}

## ** one out
one_out_setup_and_run <- function(batch_version, gof_df_cbn) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    ## reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))
    fldr_info <- setup_regression_folders_and_files(REG_MONKEY_DIR = REG_MONKEY_DIR,
                                                    batch_version = batch_version)

    
    # reg_res_files <- read_reg_res_files(fldr_info)

    ## get the best fitting models
    ## mdl_idx <- reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
    ## .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
    ##   .SDcols = "mdl_id"] %>%
    ## .[5, mdl_id]
    mdl_id_dt <- gen_mdl_id_dt(gof_df_cbn)

    ## mdl_id_dt[, .N, cbn_name]

    ## reconstruct folder info to read the reg_spec of model 
    
    ## set up ou (one-out) folder for new regression results
    ## first get /ou/ folder in v64
    ## ou_cmd <- paste0("mkdir /home/johannes/reg_res/", batch_version, "/ou/") %>% system()
    ou_cmd <- sprintf("mkdir %s%s/ou/", REG_MONKEY_DIR, batch_version) %>% system()
    ## then setup new folders there
    ## fldr_info_ou <- setup_regression_folders_and_files("v64ou", batch_dir_addgn = "v64/ou/")
    fldr_info_ou <- setup_regression_folders_and_files(
        REG_MONKEY_DIR = REG_MONKEY_DIR, 
        batch_version = paste0(batch_version, "ou"),
        batch_dir_addgn = paste0(batch_version, "/ou/"))

    ## get original reg_spec to modify 
    reg_specs_orig <- mclapply(mdl_id_dt[, mdl_id],
                               \(x) get_reg_spec_from_id(x, fldr_info = fldr_info), mc.cores = NBR_THREADS)

    
    ## modify original regspecs to make them work with one-out
        
    regspecs_ou <- mclapply(reg_specs_orig, \(x) gen_regspecs_ou(x, vvs), mc.cores = NBR_THREADS) %>% flatten()
    print(paste0("number of regspecs_ou: ", len(regspecs_ou)))

    ## map_chr(regspecs_ou, ~chuck(.x, "cfg", "cbn_name")) %>% table()

    

    ## gen_regspecs_ou(reg_specs_orig[[3]], vvs)
    ## dx <- data.table(ou_set_title = map_chr(regspecs_ou, ~.x$cfg$ou_set_title))
    ## dx[, .N, ou_set_title][grepl("SP.POP", ou_set_title)]
                                 


    ## dx <- Reduce(rbind, list(
    ##     data.table(idx = map_chr(reg_specs_orig, ~.x$mdl_id), source = "id_orig"),
    ##     data.table(idx = map_chr(regspecs_ou, ~.x$mdl_id), source = "id_ou_regspec"),
    ##     data.table(idx = map_chr(regspecs_ou, ~.x$other_cfgs$mdl_id[1]), source = "id_other_cfgs"),
    ##     data.table(idx = list.files(fldr_info_ou$REG_RES_DIR), source = "ou_dir")))

    ## dx[, max(nchar(idx)), source]

    ## regspecs_ou[[3]]

    ## ## mys_lag_specs <- c("X5XXX3XX3X111151545521", "XXX23XXXX5111151545521")
    ## mys_lag_specs <- c("1XXX3XX3XX551151511521", "1XXX3XXX3X111141545511", "X5XXX3XX3X111151545521")

    ## penl_mys <- keep(regspecs_ou, ~.x$base_lag_spec %in% mys_lag_specs[2:3])
    ## len(penl_mys)
    ## ## penl_mys[[3]]

    ## lapply(penl_mys, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou))

    

    ## ou_debug$df_reg_anls_cfgs_wide %>% adt() %>%
    ##     .[, max(nchar(mdl_id))]

    
    
    ## len(unique(map(reg_specs_orig, ~.x$mdl_id)))
    
    ou_ids <- map_chr(regspecs_ou, ~.x[["mdl_id"]])
    if (len(ou_ids) != len(unique(ou_ids))) {stop("duplicate ou-IDs")}

    ou_files <- list.files(fldr_info_ou$REG_SPEC_DIR)
    
    if (len(setdiff(ou_ids, ou_files)) != 0) {
        ## run modified regspecs
        mclapply(regspecs_ou, \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou), mc.cores = NBR_THREADS)
    }

    cbn_splitted_files(grep_pattern = "_cfgs.csv", fldr_info = fldr_info_ou) 

    ## ou_debug <- read_reg_res_files(fldr_info_ou)
    ## ou_debug$df_reg_anls_cfgs_wide %>% adt() %>% .[, .(mean(as.numeric(t_diff)), SD(as.numeric(t_diff)))]

    ## mclapply(regspecs_ou[1:20], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info_ou), mc.cores = 4)

    ## running commands to combine PID-specific files
    
}





## ** combined 


postestimation <- function(fldr_info) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    
    print("reading files")
    reg_res_files <- read_reg_res_files(fldr_info)    
    
    gof_df_cbn <- reg_res_files$gof_df_cbn
    df_reg_anls_cfgs_wide <- reg_res_files$df_reg_anls_cfgs_wide
    
    ## gen_plt_cvrgnc(gof_df_cbn)
    print("generating counterfactual")

    ## compare_r_stata(gof_df_cbn)
    
    gen_cntrfctl(gof_df_cbn, fldr_info)
    ## gen_cntrfctl(reg_anls_base$gof_df_cbn, fldr_info)

    ## gen_cryexmpls(top_coefs)

    print("generating development info")
    ## generate data for yearly developments
    gen_res_velps(cbn_dfs_rates, fldr_info)
  
    

    print("gen VIFs")
    
    ## generate top coefs, use it for VIF data
    df_anls_base <- add_coef_sig(reg_res_files$coef_df, reg_res_files$df_reg_anls_cfgs_wide)
    top_coefs <- gen_top_coefs(df_anls_base, gof_df_cbn)
    gen_VIF_allres(top_coefs, fldr_info)

    print("generate one-out tests")

    one_out_setup_and_run(fldr_info$batch_version, gof_df_cbn)
    ## test_dharma()


}



if (identical(args, character(0))) {
    stop("functions are done")
}

if (is.null(args[[1]])) {
    stop("functions are DONE")
}


## * main

## PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
PROJECT_DIR <- args[1]
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_postestimation.R"))

print("combining files")

cbn_splitted_files("_cfgs.csv[0-9]", fldr_info_optmz)

print("files have been combined, now saving files")


print("run postestimation")

## run the one-out analysis


postestimation(fldr_info_optmz)

print("postestimation is done")
