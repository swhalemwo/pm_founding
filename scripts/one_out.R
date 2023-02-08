




stop("functions are done")


oneout_anls("v66")


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

    
