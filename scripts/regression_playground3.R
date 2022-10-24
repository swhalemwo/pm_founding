## * regression playground for essential stuff, so that buffer can be C-c C-b-ed


## ** running old

cluster_sumry_addgns <- c(
    "hnwi_nbr_30M_pure" = "HNWI with net worth 30M USD (count)",
    "nbr_opened_pure" = "private museums openings (count)",
    "NY.GDP.PCAP.CD" = "country-average GDP per capita (2021 constant USD)",
    "NY.GDP.TTL" = "individual-average GDP per capita (2021 constant USD)",
    "SP.POP.TOTL_pure" = "population")

    

#' overall regression wrapping 

## *** end of functions


reg_settings <- list(
    nbr_specs = 1,
    batch_nbr = "v20",
    vary_vrbl_lag = T,
    cbns_to_include = names(cbn_dfs),
    mdls_to_include = c("full")
)

fldr_info <- setup_regression_folders_and_files(reg_settings$batch_nbr)

## generating 20k models costs around 5 secs


reg_spec_mdls <- gen_batch_reg_specs(reg_settings, vvs, vrbl_thld_choices)
names(reg_spec_mdls) <- lapply(reg_spec_mdls, \(x) x$mdl_id)

## check how unique the model cfgs are 
table(table(names(reg_spec_mdls)))

## run_vrbl_mdl_vars(reg_spec_mdls[[2]])
## gen_mdl_id(reg_spec_mdls[[2]])

cvrgns <- mclapply(reg_spec_mdls[1:12], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info, c("log_likelihood")), mc.cores = 6) %>% unlist()

lapply(reg_spec_mdls[1:30], \(x) run_vrbl_mdl_vars(x, vvs, fldr_info, c("converged"))) %>% unlist()

## run_vrbl_mdl_vars(reg_spec_mdls[[1]], vvs, fldr_info)
NULL






## **** convergence testing

vrbl_thld_choices_cvrg <- slice_sample(vrbl_thld_choices, n=1)


reg_settings_cvrg <- list(
    nbr_specs_per_thld = 2,
    batch_nbr = "v40",
    vary_vrbl_lag = F,
    cbns_to_include = "cbn_all",
    mdls_to_include = c("full"),
    technique_strs = c("nr", "dfp", "bfgs", "nr 5 dfp 5 bfgs 5"),
    difficulty_switches = c(T,F)
)
    
reg_spec_mdls_cvrg <- gen_batch_reg_specs(reg_settings_cvrg, vvs, vrbl_thld_choices_cvrg)

fldr_info_cvrg <- setup_regression_folders_and_files(reg_settings_cvrg$batch_nbr)

              
mclapply(reg_spec_mdls_cvrg, \(x) optmz_reg_spec(x, fldr_info_cvrg, reg_settings_cvrg),
         mc.cores = 6)

optmz_reg_spec(reg_spec_mdls_cvrg[[1]], fldr_info = fldr_info_cvrg, reg_settings = reg_settings_cvrg)

## ** scrap: debugging, re-running

## mdl_ids_tbl <- tibble(mdl_id = unlist(mdl_ids)) 
## mdl_ids_tbl$x <- 1

## mdls_to_check_ids <- merge(mdl_ids_tbl,
##       df_reg_anls_cfgs_wide %>% select(mdl_id) %>% mutate(y=2),
##       all.x = T) %>% atb() %>%
##     filter(is.na(y)) %>% pull(mdl_id)

## mdls_to_check_locs <- which(mdl_ids %in% mdls_to_check_ids)

## ## reg_spec_mdls[mdls_to_check_specs]
## mclapply(reg_spec_mdls[mdls_to_check_locs], run_vrbl_mdl_vars, mc.cores = 6) 





## ** standardization of cbn_dfs

cbn_dfs$cbn_all

adt(cbn_dfs$cbn_all)[, lapply(.SD, sd)] %>% melt() %>% adf()
adt(cbn_dfs$cbn_no_cult_spending)[, lapply(.SD, sd)] %>% melt() %>% adf()
adt(cbn_dfs$cbn_no_cult_spending_and_mitr)[, lapply(.SD, sd)] %>% melt() %>% adf()


## ** debug time

dirx <- "/home/johannes/reg_res/v49/reg_res/"

v49_files <- paste0(dirx, list.files(dirx))
v49_files[[1]]

v49_file_infos <- lapply(v49_files, file.info)

v49_time_infos <- lapply(v49_file_infos, \(x) format(as.POSIXct(x$ctime), format = "%Y-%m-%d %H:%M"))
v49_time_infos[[1000]]
as.POSIXct(v49_time_infos[[1000]])

format(v49_time_infos[[1000]], format = "%Y-%m-%d %H:%M")

v49_time_infos[[1000]]

t_dtx <- data.table(x=1, timex = unlist(v49_time_infos)) %>%
    .[, timex2 := as.POSIXct(timex)]

t_dtx %>% copy() %>% .[, ten_min := ceiling_date(timex2, '10 min')] %>%
    .[, .N, ten_min] %>%
    ggplot(aes(x=ten_min, y=N)) +
    geom_line()


ggplot(t_dtx, aes(x=timex2)) +
    geom_histogram(bins = 180)
    


