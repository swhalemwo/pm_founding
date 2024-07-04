## * setting all kinds of general stuff

## this is where I clone it to, i.e. its own location
## PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/" 
## PROJECT_DIR <- "/home/johannes/remtest/code/org_pop/"

## code for regression results
## REG_MONKEY_DIR <- "/home/johannes/remtest/regres/"
## REG_MONKEY_DIR <- "/home/jaengenhey/surftest/regres/"


## set environmental variables depending on whether analysis runs on my machine or in cloud
if (grepl("jaengenhey", PROJECT_DIR)) { # this is at the cloud 
    REG_MONKEY_DIR <- "/data/volume_2/surftest/regres/"
    NBR_THREADS <- 14
} else {
    ## this is at home 
    REG_MONKEY_DIR <- "/home/johannes/surfres/"
    NBR_THREADS <- 5
}



PMDB_DIR   <- paste0(PROJECT_DIR, "data/pmdb/") # DIR for private museum database (currently excel import)
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")
RDS_DIR    <- paste0(PROJECT_DIR, "data/RDS/")



source(paste0(SCRIPT_DIR, "rprofile.R"))

PROC_DATA_DIR <- paste0(PROJECT_DIR, "data/processed/")

STARTING_YEAR <- 1985
ENDING_YEAR <- 2020

MAX_GAP_SIZE = 4



colors_manual_light <- c("#a4e3a5","#f197c1","#98fff4","#f29a83","#2bcef0","#ffefa5","#75bfff","#c2bc71","#d6d3ff","#52bcae","#ffc9c2","#85b4b6","#f3ffd5","#b0ad84")

## colors_manual <- c("#c78ab5","#74b648","#6f4aca","#c7994f","#cb4fc2","#516833","#cd4872","#5cb099","#d25133","#6289c0","#814135","#613a76")

colors_manual <- c("#01867b","#e2183d","#44ddb8","#891ba6","#93d93d","#ff3c8a","#006e00","#d2b1ff","#ff9e30","#009cf4","#ff6830","#86375a","#ae8400","#967649")

colors_manual2 <- c("#6299ff", "#f89700", "#7d5aff", "#2c8700", "#d7016e", "#b8d15f", "#002564", "#ff3c65", "#018a67", "#ff7ecb", "#4b4e00", "#fdb694")

colors_manual3 <- c("#ae002b", "#01ca89", "#ae69ff", "#a4d55b", "#0059d9", "#ffb91b", "#900089", "#ef0015", "#59b1ff", "#001945", "#fd99ff", "#534a7a")
## i like this the most so far

colors_manual4 <- c("#ffa68b", "#001d5b", "#f4be4a", "#79007e", "#538500", "#bf9eff", "#f2001c", "#00a0ef", "#9cd679", "#7c003c", "#01834c", "#ff768f")



options(ggrepel.max.overlaps = 100) # set some option for ggrepel


## set static objects
## These which get written to file after data wrangling data, and get read in for regression part


OBJS_TO_RDS_WRNGL <- .c(
    ## regression
    df_reg,
    df_reg_rts, 
    vvs,
    vrbl_cbns,
    vrbl_thld_choices,
    cbn_df_dict,
    ## for reg_anls (nbrs)
    cbn_dfs_rates,
    cbn_dfs_counts_uscld,
    cbn_dfs_rates_uscld,
    ## data for nbrs
    df_excl,
    df_open,
    dt_shweal,
    dt_diversity
)

reg_settings_optmz <- list(
    ## relevant parameters
    ## uncomment only one of these
    ## on laptop
    ## ---------------
    ## batch_version       = "v33",   
    ## nbr_specs_per_thld  =  1,      
    ## lags                =  1:3,    
    ## cbns_to_include     =  "cbn1", 
    ## n_vrbl_thld_choices =  4,      
    ## -------------- for surf
    batch_version       = "v34",         
    nbr_specs_per_thld  = 4,                 
    lags                = 1:5,               
    cbns_to_include     = paste0("cbn", 1:3),
    n_vrbl_thld_choices = 36,
    ## ---------------- no longer varying
    dvfmts              = c("rates"), 
    vary_vrbl_lag       = F,
    technique_strs      = c("nr"),
    difficulty_switches = T,
    regcmds             = c("glmmTMB"),
    mdls_to_include     = c("full"),
    wtf                 = T,
    max_loop_nbr        = 100    
)


## output dirs: now moved to reg_res folder for better zipping

## FIG_DIR        <- paste0(PROJECT_DIR, "figures/")
## TABLE_DIR      <- paste0(PROJECT_DIR, "tables/")
FIG_DIR        <- paste0(REG_MONKEY_DIR, reg_settings_optmz$batch_version, "/figures/")
TABLE_DIR      <- paste0(REG_MONKEY_DIR, reg_settings_optmz$batch_version, "/tables/")

if (!dir.exists(FIG_DIR))   {dir.create(FIG_DIR,   recursive = T)}
if (!dir.exists(TABLE_DIR)) {dir.create(TABLE_DIR, recursive = T)}




setup_regression_folders_and_files <- function(REG_MONKEY_DIR, batch_version, batch_dir_addgn = "") {
    #' setup folders and files for running regressions
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    

    ## batch_version <- "v20"
    ## REG_MONKEY_DIR <- "/home/johannes/reg_res/"
    BATCH_DIR <- paste0(REG_MONKEY_DIR, batch_dir_addgn, batch_version, "/")

    REG_RES_DIR <- paste0(BATCH_DIR, "reg_res/")
    REG_RES_FILE_LAGS <- paste0(BATCH_DIR, batch_version, "_lags.csv")
    REG_RES_FILE_CFGS <- paste0(BATCH_DIR, batch_version, "_cfgs.csv")
    REG_SPEC_DIR <- paste0(BATCH_DIR, "specs/")
    MDL_START_FILE <- paste0(BATCH_DIR, batch_version, "_start.csv")
    MDL_END_FILE <- paste0(BATCH_DIR, batch_version, "_end.csv")
    ## generate existing dirs: have to normalizePath (remove //), and paste additional "/" at the end reeee
    existing_batch_dirs <- paste0(normalizePath(list.dirs(REG_MONKEY_DIR, recursive = F)), "/")
    
    if (BATCH_DIR %!in% existing_batch_dirs) { system(paste0("mkdir ", BATCH_DIR))}
    existing_dirs <- paste0(normalizePath(list.dirs(BATCH_DIR, recursive = F)), "/")

    if (REG_RES_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_RES_DIR))}
    if (REG_SPEC_DIR %!in% existing_dirs){ system(paste0("mkdir ", REG_SPEC_DIR))}
    PID_DIR <- "/home/johannes/pid_dir/"

    return(list(
        batch_version = batch_version,
        BATCH_DIR = BATCH_DIR,
        REG_MONKEY_DIR = REG_MONKEY_DIR,
        REG_RES_DIR = REG_RES_DIR,
        REG_RES_FILE_LAGS = REG_RES_FILE_LAGS,
        REG_RES_FILE_CFGS = REG_RES_FILE_CFGS,
        REG_SPEC_DIR = REG_SPEC_DIR,
        MDL_START_FILE = MDL_START_FILE,
        MDL_END_FILE = MDL_END_FILE,
        PID_DIR = PID_DIR
    )
    )
}

