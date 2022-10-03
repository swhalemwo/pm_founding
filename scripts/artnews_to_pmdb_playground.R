
## ** merging test

## 81 mismatches??




## ** printing debug
install.packages("devtools")

require(devtools)


install.packages("tibble")
install_version("tibble", version = "2.1.3", repos = "http://cran.us.r-project.org")
install_version("tibble", version = "3.0.0", repos = "http://cran.us.r-project.org")
install_version("tibble", version = "3.1.0", repos = "http://cran.us.r-project.org")
install_version("tibble", version = "3.0.5", repos = "http://cran.us.r-project.org")
install_version("tibble", version = "3.0.3", repos = "http://cran.us.r-project.org")
## install_version("tibble", version = "3.0.4", repos = "http://cran.us.r-project.org")

library(tibble)
as_tibble(mtcars)

tibble_mwe = tibble(column1_long_long_long = c(" long string long string long string long string long string",
                                               "long string long string long string long string long string"),
                    column2_long_long_long = c("long string long string long string long string long string",
                                               "long string long string long string long string long string "),
                    column3 = c(1,2))

ARTNEWS_RES_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names_UTF.csv"
x <- as_tibble(read.csv(ARTNEWS_RES_FILE, sep = ";"))
x$notes_long_long <- "aaaaaaaaaa jjjjjj"
x$notes_long_long2 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long3 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long3 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long4 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long5 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long6 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long7 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long8 <- "aaaaaaaaaa jjjjjj"
x$notes_long_long9 <- "aaaaaaaaaa jjjjjj"

x

as.data.table(x)

options("pillar.max_footer_lines" = 1)
options("pillar.subtle" = F)
options("pillar.max_extra_cols" = 0)
options("pillar.bidi" = T)
x


## MWE doesn't properly render, only own data does

options(datatable.print.trunc.cols = F)
options(datatable.print.class = T)


## ** separate_rows attempts in data table

dt <- data.table("title"=c("First Title", "Second Title", "Third Title", "Fourth Title"), 
                 "sha"=c("12345", "2345; 66543; 33423", "22222; 12345678;", "666662345; 444"))

dt <- data.table("title"=c("First Title", "Second Title", "Third Title", "Fourth Title"), 
                 "sha"=c("12345", "2345; 66543; 33423", "22222; 12345678;", ""))


dt[, .(sha = unlist(tstrsplit(sha, ";"))), by = "title"]
dt[, .(sha = unlist(ctstrsplit(sha, ";"))), by = "title"]


## if such a dataframe has more columns than title/id that I also want to have duplicated, I can pass them to by
## shouldn't really happen a lot tho me thinks

dt[, cnt:= seq(10, 40, 10)]

dt %>% copy() %>%
    .[, .(sha = trimws(unlist(ctstrsplit(sha, ";")))), by = c("title", "cnt")]

## not specifying title fucks it up 
dt[, .(sha = unlist(ctstrsplit(sha, ";"))), by = "title"]



ctstrsplit("asdf;jjj", ";")
ctstrsplit("", ";")
tstrsplit("mmm", ";")
ctstrsplit("mmm", ";")


## rowsplitting
artnews_name_check_dt[, .(collector_name_artnews = unlist(ctstrsplit(collector_name_artnews, ";", type.convert = T))), by = "founder_name_pmdb"]

## artnews_name_check_dt2[, .(collector_name_artnews = strsplit(collector_name_artnews, ";")), by = "founder_name_pmdb"]

## dt[, lapply(.SD, \(x) unlist(tstrsplit(x, "; ?"))),
##    .SDcols = "sha", by = .(title)]

## artnews_name_check_dt2[, lapply(.SD, \(x) unlist(tstrsplit(x, "; ?"))),
##    .SDcols = "collector_name_artnews", by = founder_name_pmdb]

## *** some other SO attempt 

## https://stackoverflow.com/questions/13773770/split-comma-separated-strings-in-a-column-into-separate-rows/31514711#31514711

artnews_name_check_dt %>% copy() %>%
    .[,on=conversion_df, collector_name_artnews := i.to] %>%
    .[,lapply(.SD, \(x) unlist(ctstrsplit(x, ";")))] %>% unique()
## doesn't even work properly when having multiple columns with commas
    



## different ways of debugging the count
## {.[, .N, founder_name_pmdb][, .N, N]}    
## pull(founder_name_pmdb) %>% table() %>% table()

v <- data.frame(director = c("Aaron Blaise,Bob Walker", "Akira Kurosawa", 
                        "Alan J. Pakula", "Alan Parker", "Alejandro Amenabar", "Alejandro Gonzalez Inarritu", 
                        "Alejandro Gonzalez Inarritu,Benicio Del Toro", "Alejandro González Iñárritu", 
                        "Alex Proyas", "Alexander Hall", "Alfonso Cuaron", "Alfred Hitchcock", 
                        "Anatole Litvak", "Andrew Adamson,Marilyn Fox", "Andrew Dominik", 
                        "Andrew Stanton", "Andrew Stanton,Lee Unkrich", "Angelina Jolie,John Stevenson", 
                        "Anne Fontaine", "Anthony Harvey"), AB = c('A', 'B', 'A', 'A', 'B', 'B', 'B', 'A', 'B', 'A', 'B', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B', 'A'))

setDT(v)[, lapply(.SD, function(x) unlist(tstrsplit(x, ",", fixed=TRUE))), by = AB
         ][!is.na(director)]
## splitting every column LMAO what could go wrong???


## ** recoding, generally chaining/piping dt operations
## https://stackoverflow.com/questions/44590935/recode-a-variable-using-data-tabel
## update assignment in by-part, amazing and not a footgun at all
## artnews_name_check_dt[conversion_df]
## error: conversion_df in i-part (filtering) isn't even part that works on its own
## probably should read dt-join vignette:

## update by reference, AVOID!!
## artnews_name_check_dt[conversion_df, on = .(collector_name_artnews), collector_name_artnews := i.to]


## use copying to maintain original object 
artnews_name_check_dt %>% copy() %>%
    .[conversion_df,  on = .(collector_name_artnews), collector_name_artnews := i.to]


artnews_name_check_dt[scramblematch("Schaufler", collector_name_artnews)]


## see if I can modify conversion_df 
artnews_name_check_dt %>% copy() %>%
    .[conversion_df,  on = .(collector_name_artnews), collector_name_artnews := i.to]


## try chaining with piping, throws error
artnews_name_check_dt %>% copy() %>%
    .[conversion_df,  on = .(collector_name_artnews), collector_name_artnews := i.to][
        scramblematch("Schaufler", collector_name_artnews)]

## solution is to wrap RHS in curly braces
artnews_name_check_dt %>% copy() %>% {
    .[conversion_df,  on = .(collector_name_artnews), collector_name_artnews := i.to][
        scramblematch("en", collector_name_artnews)]}
    
## for now use copying     

## the fact that only the update assingment expands the df is fucking weird:
## is because only passing dt in X[Y] is effectively filtering, and by update by referencing does meaning change from subset to update 

## ## just joining (filtering)
## artnews_name_check_dt %>% copy() %>%
##     .[conversion_df, on = .(collector_name_artnews)]

## ## renaming collector_name_artnews
## artnews_name_check_dt %>% copy() %>%
##     .[conversion_df, on = .(collector_name_artnews), collector_name_artnews := i.to] %>% 
##     .[scramblematch("Schaufler", collector_name_artnews)]

## ## can reorder
## artnews_name_check_dt %>% copy() %>%
##     .[conversion_df, collector_name_artnews := i.to, on = .(collector_name_artnews)] %>%
##     .[scramblematch("Schaufler", collector_name_artnews)]

## ## see if I can get natural joins working: yup
## artnews_name_check_dt %>% copy() %>%
##     .[,on=conversion_df, collector_name_artnews := i.to] # have to keep i free in [], on=Y uses Y tho LUL

## *** some other attempts at recoding, seem pretty useless
## weird order due to default RHS table first and then outer join??


## trying to do the recoding the other way around (starting with conversion df), don't think it works:
## there's no way to easily refer to conversion_df$collector_name artnews with assignment (only update by reference, which then only refers to rows in conversion df, but I want entire table
## also nomatch=NULL not really working

## conversion_df[artnews_name_check_dt, on = .(collector_name_artnews)]
## conversion_df %>% copy() %>%
##     .[,on=artnews_name_check_dt, collector_name_artnews := collector_name_artnews]

## conversion_df %>% copy() %>%
##     .[,on=artnews_name_check_dt, i.collector_name_artnews := x.collector_name_artnews, nomatch=NULL]

## conversion_df %>% copy() %>%
##     .[,on=artnews_name_check_dt, collector_name_artnews = x.collector_name_artnews]



## recode within dt: 
## y <- artnews_name_check_dt[, .(collector_name_artnews = recode(collector_name_artnews, !!!conversions_list))]
## am smarter now 

