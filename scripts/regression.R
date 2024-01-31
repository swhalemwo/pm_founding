## * regression

args <- commandArgs(trailingOnly = T)
options(width = 115)


PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_reg.R"))



vrbl_thld_choices_optmz <- slice_sample(vrbl_thld_choices, n=36)

reg_settings_optmz <- list(
    nbr_specs_per_thld = 2,
    dvfmts = c("rates"), # should also be counts, but multiple dvfmts not yet supported by reg_anls
    batch_version = "v19",
    lags = 1:5,
    vary_vrbl_lag = F,
    technique_strs = c("nr"),
    difficulty_switches = T,
    regcmds = c("glmmTMB"),
    cbns_to_include = names(cbn_df_dict$counts)[1],
    mdls_to_include = c("full"),
    wtf = T,
    max_loop_nbr = 100
)



reg_spec_mdls_optmz <- gen_batch_reg_specs(reg_settings_optmz, vvs, vrbl_thld_choices_optmz)
print(len(reg_spec_mdls_optmz))


fldr_info_optmz <- setup_regression_folders_and_files(reg_settings_optmz$batch_version)

setup_db_mdlcache(fldr_info_optmz)


mclapply(reg_spec_mdls_optmz, \(x) optmz_reg_spec(x, fldr_info_optmz, reg_settings_optmz),
         mc.cores = NBR_THREADS, mc.preschedule = F)


print("models have been run, now saving files")

walk(OBJS_TO_RDS_REG, ~saveRDS(get(.x), file = paste0(RDS_DIR, .x, ".rds")))

## this stop should never be commented out 
stop("regression is DONE")



## ** garage for inspection

## *** basic test
regspec_x <- reg_spec_mdls_optmz[[2]]

reg_settings_garage <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value = F)
optmz_reg_spec(reg_spec_mdls_optmz[[3]], fldr_info_optmz, reg_settings_garage)




## *** some other test
regspec_x <- get_reg_spec_from_id("XXX5XX3X5X553335511111115--cbn1--full--nr--TRUE--glmmTMB--rates--XXX3XX2X1X222231543344221--hn200iigwi99--3--XXX5XX3X5X553335511111115--14--NY.GDP.PCAP.CDk", fldr_info_optmz)

optmz_reg_spec(regspec_x, fldr_info_optmz, reg_settings_garage)

regspec_x <- get_reg_spec_from_id("XX5X4XXXX3443311213344112--cbn1--full--nr--TRUE--glmmTMB--rates--XX5X4XXXX3443311213344442--hn30ii90wig--4--XX5X4XXXX3443311213344112--0--pm_density_global", fldr_info_optmz)





## *** pool test

reg_settings_garage <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value = T)
reg_settings_garage <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value = F)

regspec_x <- get_reg_spec_from_id("XX5X4XXXX3443311213344112--cbn1--full--nr--TRUE--glmmTMB--rates--XX5X4XXXX3443311213344442--hn30ii90wig--4--XX5X4XXXX3443311213344112--0--pm_density_global", fldr_info_optmz)

optmz_reg_spec(regspec_x, fldr_info_optmz, reg_settings_garage)

## *** wb_tryagain test

reg_settings_garage <- copy(reg_settings_optmz) %>% `pluck<-`("wtf", value = F)
conx <- dbConnect(RSQLite::SQLite(), "/home/johannes/reg_res/v16/mdl_cache.sqlite")
dbGetQuery(conx, "select * from mdl_cache limit 10")

optmz_reg_spec(regspec_x, fldr_info_optmz, reg_settings_garage)


## ** look at growth rate numbers

cbn_dfs_rates$cbn2 %>% adt %>% .[, reg6 := rcd_iso3c_reg6(iso3c)] %>% atb %>%
    viz_lines(y="NY.GDP.PCAP.KD.ZG_lag5", facets = "reg6", max_lines = 6)


## ** test different optimization settings for speed






## resx2 <- copy(resx)

## ** testing callgraph, but still too buggy
## library(jtls)

## c_dirs <- gc_dirs(dir_proj = "/home/johannes/Dropbox/phd/papers/org_pop/")



## testf <- function() {
##     test_obj <- 10
##     attr(test_obj, "gnrtdby") <- as.character(match.call()[[1]])
##     return(test_obj)
## }


## testobj <- testf()
## testobj2 <- testf()
## jtls::gwd_clgrph()
## gl_clgr_objs()


## ** debugging lack of convergence

## *** v85
## termlog inspection
dt_termlog <- fread(paste0(fldr_info_optmz$BATCH_DIR, "termlog.txt"), header = F, col.names = c("garbage", "msg"))

dt_termlog %>%
    .[!grepl("already there", msg)] %>% 
    .[msg %!in% names(vvs$vrbl_lbls)]

## *** v78

## run ~150 unoptimized models, but all converge
ii <- 12
while (T) {
    x <- reg_spec_mdls_optmz[[ii]]

    x$cfg$regcmd <- "glmmTMB"
    resx <- run_vrbl_mdl_vars(x, vvs, fldr_info_optmz, verbose = F, wtf = F)

    print(ii)
    if (!resx$converged) {break}
    ii <- ii+1
    
}

## try running single model in detail; don't find failure there neither
optmz_reg_spec(reg_spec_mdls_optmz[[2]], fldr_info_optmz, reg_settings_optmz)

## search the results
mdl_ids_fail <- reg_anls_base$df_reg_anls_cfgs_wide %>% adt() %>% #.[, .N, cvrgd]
    .[cvrgd == 0, mdl_id]

regspec_fail <- get_reg_spec_from_id(mdl_id_fail, fldr_info)

run_vrbl_mdl_vars(regspec_fail, vvs, fldr_info_optmz, verbose = F, wtf = F)

cvrg_res <- map(mdl_ids_fail[1:30], ~get_reg_spec_from_id(.x, fldr_info) %>%
                      run_vrbl_mdl_vars(., vvs, fldr_info_optmz, verbose = F, wtf = F))


## *** v38

## non_cvrgd_spec <- get_reg_spec_from_id(
##     "XX5XXX2XX3111135211--cbn_no_cult_spending_and_mitr--full--XX2XXX4XX4555532345--1--hnwi_nbr_30M",
##     fldr_info_optmz)


## non_cvrgd_spec$cfg$difficulty <- T
## non_cvrgd_spec$cfg$technique_str <- "nr"
## non_cvrgd_spec$regcmd <- "xtnbreg"


## run_vrbl_mdl_vars(non_cvrgd_spec, vvs, fldr_info_optmz, verbose = T)

## *** v41

## dt_cfgs <- fread("/home/johannes/reg_res/v41/v41_cfgs.csv")
## dt_cfgs[V6 == 0] %>% adf()

non_cvrgd_spec <- get_reg_spec_from_id(
    "XX4XX3X3XX111125211--cbn_no_cult_spending_and_mitr--full--nr--TRUE--XX3XX2X3XX443221213--1--NY.GDP.PCAP.CDk",
    fldr_info_optmz)

non_cvrgd_spec$regcmd <- "xtnbreg"

t1 = Sys.time()
run_vrbl_mdl_vars(non_cvrgd_spec, vvs, fldr_info_optmz, verbose = F)
t2 = Sys.time()

print(t2-t1)

## *** v44

glmm_na_spec <- get_reg_spec_from_id(
    "XX5X3XX3XX441155232--cbn_no_cult_spending--full--nr--TRUE--glmmTMB--XX5X2XX5XX114113144--2--hnwi_nbr_30M",
    fldr_info_optmz)

run_vrbl_mdl_vars(glmm_na_spec, vvs, fldr_info_optmz, verbose = T)

optmz_reg_spec(glmm_na_spec, fldr_info_optmz, reg_settings_optmz)

## *** v47

## debug errors in v47: some models still throw errors -> FIND them

started <- fread(paste0(fldr_info_optmz$MDL_START_FILE), col.names = "mdl_id", header = F)
finished <- fread(paste0(fldr_info_optmz$MDL_END_FILE), col.names = "mdl_id", header = F)

setdiff(started$mdl_id, finished$mdl_id)
setdiff(finished$mdl_id, started$mdl_id)

error_spec <- get_reg_spec_from_id(
    started[!finished, on = "mdl_id"][c(3)], fldr_info_optmz)

run_vrbl_mdl_vars(error_spec, vvs, fldr_info_optmz, verbose = T)

## ** test mc.preschedule = F

testx <- function() {
    print(Sys.getpid())
    Sys.sleep(runif(1, 0, 5))}

nbr_cores <- 3
t1 = Sys.time()
mclapply(seq(1, nbr_cores* 5), \(x) testx(), mc.cores = nbr_cores)
t2 = Sys.time()
mclapply(seq(1, nbr_cores* 5), \(x) testx(), mc.cores = nbr_cores, mc.preschedule = F)
t3 = Sys.time()

print(t3-t2)
print(t2-t1)
