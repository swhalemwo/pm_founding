

syspkgs <- c("cmake", "libxml2-dev", "libssl-dev", "libcurl4-openssl-dev", "libfontconfig1-dev")

rlibs <- c("data.table", "dplyr", "purrr", "glmmTMB", "ggplot2", "parallel",
           "docstring", "DBI", "rsdmx", "collapse", "modelsummary", "RSQLite",
           "xtable", "Hmisc", "furrr", "performance", "tidyr", "dineq",
           "stringr", "ggbeeswarm", "patchwork", "ggridges", "ggrepel",
           "texreg", "countrycode")

cmd_syspkgs <- sprintf("sudo apt-get install %s", paste0(syspkgs, collapse = " "))
system(cmd_syspkgs)

install.packages(rlibs)


                     
