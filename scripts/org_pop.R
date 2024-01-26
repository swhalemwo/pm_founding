args <- commandArgs(trailingOnly = T)
options(width = 115)

## * source other files
PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")



PMDB_FILE <- "Private museum database2.xlsx"
PMDB_FILE <- "Private museum database3.xlsx"
PMDB_FILE <- "Private museum database4.xlsx"
PMDB_FILE <- "Private museum database5.xlsx"
PMDB_FILE <- "Private museum database6.xlsx"
PMDB_FILE <- "Private museum database7.xlsx"
PMDB_FILE <- "Private museum database9.xlsx"
PMDB_FILE <- "Private museum database11.xlsx"
PMDB_FILE <- "Private museum database12.xlsx"
## PMDB_FILE <- "Private museum database13.xlsx" ## just use to check if merging went well 
PMDB_FILE <- "Private museum database15.xlsx"
PMDB_FILE <- "Private museum database16.xlsx"
PMDB_FILE <- "Private museum database25.xlsx"
fstd <- ""

source(paste0(SCRIPT_DIR, "startup_org_pop.R")) ## startup: libraries, global vars
source(paste0(SCRIPT_DIR, "startup_static.R"))
source(paste0(SCRIPT_DIR, "cultural_spending.R"))
source(paste0(SCRIPT_DIR, "custom_funcs.R")) # random utils
cur_df <- gen_cur_df(WID_VX)
source(paste0(SCRIPT_DIR, "wb_api.R")) ## World Bank data, has to be run before sourcing base_df_creation since it provides the country-year structure
source(paste0(SCRIPT_DIR, "base_df_creation.R")) # function to read in excel data
source(paste0(SCRIPT_DIR, "WID_setup_and_checks.R"))
source(paste0(SCRIPT_DIR, "viz_opngs.R"))
source(paste0(SCRIPT_DIR, "oecd_api.R"))
source(paste0(SCRIPT_DIR, "mow.R"))
source(paste0(SCRIPT_DIR, "tax_incentives.R"))
source(paste0(SCRIPT_DIR, "artnews_to_pmdb.R"))
source(paste0(SCRIPT_DIR, "artnews.R"))
source(paste0(SCRIPT_DIR, "cbn_dfs.R"))
source(paste0(SCRIPT_DIR, "marginal_tax_rates.R"))
source(paste0(SCRIPT_DIR, "hdi.R"))
source(paste0(SCRIPT_DIR, "gen_rates.R"))
source(paste0(SCRIPT_DIR, "gen_cbn_df_dict.R"))


PMDATA_LOCS <- gc_pmdata_locs()

df_excl <- create_excel_df(PMDB_FILE, only_pms = F)
## df_excl <- create_excel_df(PMDB_FILE, only_pms = T)
df_open <- aggregate_openings(df_excl, impute_closing_year = T)
## df_wb <- get_WB_data(c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "NY.GDP.MKTP.CN", "NY.GDP.PCAP.KD.ZG"), refresh_all = T)
df_wb <- get_WB_data(c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "NY.GDP.MKTP.CN", "NY.GDP.PCAP.KD.ZG"), refresh_all = F)
df_anls <- create_anls_df(df_wb, df_open)
df_reg_pre_impt <- get_df_reg(df_anls)
df_reg <- impute_df_reg_vrbls(df_reg_pre_impt)
df_reg_rts <- gen_df_reg_rts(df_reg)



## generate data objects needed for regression 

vvs <- gen_vrbl_vectors()
vrbl_cbns <- gen_cbns(vvs$all_rel_vars, vvs$base_vars)
vrbl_thld_choices <- gen_vrbl_thld_choices(vvs$hnwi_vars, vvs$inc_ineq_vars, vvs$weal_ineq_vars)

select <- dplyr::select
lag <- dplyr::lag
cbn_dfs_counts_uscld <- gen_cbn_dfs(df_reg, vvs$lngtd_vars, vvs$crscn_vars, vrbl_cbns, vvs$base_vars)
cbn_dfs_counts <- scale_cbn_dfs(cbn_dfs_counts_uscld, vvs$base_vars, df_reg)
cbn_dfs_rates_uscld <- gen_cbn_dfs(df_reg_rts, vvs$lngtd_vars, vvs$crscn_vars, vrbl_cbns, vvs$base_vars)
cbn_dfs_rates <- scale_cbn_dfs(cbn_dfs_rates_uscld, vvs$base_vars, df_reg_rts)

cbn_df_dict <- list(counts = cbn_dfs_counts,
                    rates = cbn_dfs_rates)


## source(paste0(SCRIPT_DIR, "descriptives.R"))

## source(paste0(SCRIPT_DIR, "regression.R"))


print("writing objects to file")

walk(OBJS_TO_RDS, ~saveRDS(get(.x), file = paste0(RDS_DIR, .x, ".rds")))


