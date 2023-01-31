## * one out analysis


reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))

## get the best fitting models, for now just select random one  
mdl_idx <- reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
    .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
      .SDcols = "mdl_id"] %>%
    .[5, mdl_id]

## reconstruct folder info to read the reg_spec of model 
fldr_info_v64 <- setup_regression_folders_and_files("v64")

## get reg_spec to modify 
reg_specx <- get_reg_spec_from_id(mdl_idx, fldr_info_v64)

## set up ou (one-out) folder for new regression results
## first get /ou/ folder in v64
ou_cmd <- paste0("mkdir /home/johannes/reg_res/", "v64", "/ou/") %>% system()
## then setup new folders there
fldr_info_v64_ou <- setup_regression_folders_and_files("v64ou", batch_dir_addgn = "v64/ou/")


## run_vrbl_mdl_vars(reg_specx, vvs, fldr_info_v64_ou)
## seems to work

modfy_regspec_ou <- function(reg_spec, vrbl_oud, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' modify reg_spec for one-out calculations:
    #' one-out a variable (oud = one-out-ed)

    ## reg_spec

    vrbl_oud <- "hnwi_nbr_1M_lag1"
    
    reg_spec_mod <- reg_spec
    
    reg_spec_mod$mdl_vars <- setdiff(reg_spec_mod$mdl_vars, vrbl_oud)

    reg_spec_mod$cfg <- c(reg_spec_mod$cfg, list(vrbl_oud = vrbl_oud))
    
    id_updtd <- gen_mdl_id(reg_spec_mod, vvs)

    reg_spec_mod$other_cfgs <- id_updtd$other_cfgs
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
    

    ou_sets <- lapply(vrbls_to_ou, \(x) gen_vrbl_oud(x, vrbls_to_ou))
    names(ou_sets) <- vrbls_to_ou

    ## add some manual sets of variables to exclude: 
    ## - all density
    ## - all density + closing
    ou_sets2 <- c(ou_sets,
                  list(all_dens = keep(vrbls_to_ou, ~grepl("density", .x)),
                       all_dens_close = c(keep(vrbls_to_ou, ~grepl("density", .x)),
                                          keep(vrbls_to_ou, ~grepl("nbr_closed_cum", .x)))))
    

    

    
}

gen_regspecs_ou(reg_specx)

run_vrbl_mdl_vars(reg_spec_mod, vvs, fldr_info_v64_ou)





