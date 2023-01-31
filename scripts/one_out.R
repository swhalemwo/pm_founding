
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

run_vrbl_mdl_vars(reg_spec_mod, vvs, fldr_info_v64_ou)

modfy_regspec_ou(reg_specx, vrbl_oud = vrbl_oud, vvs)



