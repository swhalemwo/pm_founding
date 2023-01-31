


## get the best fitting 

mdl_idx <- reg_res62$reg_res_objs$gof_df_cbn %>% adt() %>%
    .[gof_names == "log_likelihood", .SD[which.max(gof_value)], by = .(cbn_name, vrbl_choice),
      .SDcols = "mdl_id"] %>%
    .[5, mdl_id]

fldr_info_v62 <- setup_regression_folders_and_files("v62")

reg_specx <- get_reg_spec_from_id(mdl_idx, fldr_info_v62)




