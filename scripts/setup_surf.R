# * setup script for surf

## ** install system packages necessary for R packages


dir_rlib <- "/home/jaengenhey/R/x86_64-pc-linux-gnu-library/4.1"
if (!dir.exists(dir_rlib)) {dir.create(dir_rlib)}

dir_regres <- "/data/volume_2/surftest/regres/"
if (!dir.exists(dir_regres)) {dir.create(dir_regres)}
    

syspkgs <- c("cmake", "libxml2-dev", "libssl-dev", "libcurl4-openssl-dev", "libfontconfig1-dev")

cmd_syspkgs <- sprintf("sudo apt-get install %s", paste0(syspkgs, collapse = " "))
system(cmd_syspkgs)

## ** install R packages

rlibs <- c("data.table", "dplyr", "purrr", "glmmTMB", "ggplot2", "parallel",
           "docstring", "DBI", "rsdmx", "collapse", "modelsummary", "RSQLite",
           "xtable", "Hmisc", "furrr", "performance", "tidyr", "dineq",
           "stringr", "ggbeeswarm", "patchwork", "ggridges", "ggrepel",
           "texreg", "countrycode")

## check which packages are installed already
rlib_status <- sapply(rlibs, \(x) require(x, character.only = T))

## install those that aren't installed yet 
install.packages(rlibs[!rlib_status],
                 lib = dir_rlib)


## check that packages have been installed
rlib_status2 <- sapply(rlibs, \(x) require(x, character.only = T))
if (any(!rlib_status2)) {
    stop(sprintf("packages %s not installed",
                 paste0(rlibs[!rlib_status2], collapse = ", ")))}

## pckgs_test <- c("data.table", "kapparino", "dplyr")
## lapply(pckgs_test, \(x) library(x, character.only = T))






                     
