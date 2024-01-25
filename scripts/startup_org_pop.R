## ** startup
## *** libraries
library("readxl")
library(tibble)
library(reshape2)
library(dplyr)
library(lme4)
library(texreg)
library(ggplot2)
library(countrycode)
library(stargazer)
library(gridExtra)
library(parallel)
library(RClickhouse)
library(docstring)
library(DBI)
library(TTR)
library(ggpubr)
library(xtable)
library(OECD)
library(rsdmx)
library(data.table)
library(wbstats)
library(ggrepel)
library(PerformanceAnalytics)
library(factoextra)
library(tidyr)
library(pglm)
## library(R.utils)
library(pmdplyr)
library(rlist)
library(readr)
library(DescTools)
library(testthat)
library(stringr)
library(pbmcapply)
library(purrr)
library(Hmisc, include.only = "latexTranslate")
library(furrr) # needed for VIF checks (run with future_map_dfr
library(glmmTMB) # needed for VIF checks (VIF models run with glmmTMB)
library(performance) # needed for VIF calculations (check_collinearity)
library(jtls) # custom tools, moved some functionality there
library(pmdata) # functions for importing PMDATA from centralized repo
library(collapse) # fast data transformation functions

## library(ggh4x, include.only = "geom_pointpath")

filter <- dplyr::filter
melt <- data.table::melt


ds <- docstring

options(show.error.messages = TRUE)
options(show.error.locations = TRUE)



## *** set static vars



con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

