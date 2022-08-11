
df_excl <- create_excel_df(PMDB_FILE, only_pms = F)

ARTNEWS_RES_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names_res.csv"
ARTNEWS_RES_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names_UTF.csv"

check_for_ids <- function(df_excl) {
    #' check if every museum has an ID

    if (any(is.na(df_excl$ID))) {
        stop("some museums don't have IDs")
    }
}

ARTNEWS_ORIG_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names.csv"
artnews_orig_dt <- fread(ARTNEWS_ORIG_FILE)


## replacing some weird pattern: ¬† which is supposed to be space
artnews_name_check_dt <- fread(ARTNEWS_RES_FILE, sep = ";")
artnews_name_check_dt[, collector_name_artnews := gsub("¬†", " ", collector_name_artnews)]


artnews_all_res <- readin_artnews_all()

## some replacing of weird non-breaking space that crept already into python coding
artnews_time_df <- artnews_all_res$artnews_time_df %>%
    mutate(clctr_name = gsub(" ", " ", clctr_name))

## the resulting artnews collectors who are not in the original dataframe
setdiff(artnews_name_check_dt$collector_name_artnews, artnews_time_df$clctr_name)

## first fix artnews name
## think they need manual treatment, also since there are some cases of multiple founders

conversions_list = list(
    "Ida and Achille Maramotti; Maramotti Family" = "Ida and Achille Maramotti;Maramotti Family", 
    "Anette and Udo Brandhorst; Udo Brandhorst" ="Anette and Udo Brandhorst;Udo Brandhorst",              
    "Bernard Arnault; Hélène and Bernard Arnault" = "Bernard Arnault;Hélène and Bernard Arnault",             
    "Christiane Schaufler-M√ºnch and Peter Schaufler" = "Christiane Schaufler-Münch and Peter Schaufler",
    "Dieter Bührle; Hortense Anda-Bührle" = "Dieter Bührle;Hortense Anda-Bührle",
    "Emily Rauh Pulitzer; Joseph Pulitzer, Jr." = "Emily Rauh Pulitzer;Joseph Pulitzer, Jr.",
    "Eugenio L√≥pez Alonso" = "Eugenio López Alonso", 
    "Mania and Bernhard Hahnloser; Margrit and Paul Hahnloser" = "Mania and Bernhard Hahnloser;Margrit and Paul Hahnloser", 
    "Gra_yna Kulczyk" = "Grażyna Kulczyk")

conversion_df <- data.table(collector_name_artnews = names(conversions_list),
                            to = unlist(conversions_list))





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


## combining with splitting long
artnews_name_check_dt2 <- artnews_name_check_dt %>% copy() %>%
    .[,on=conversion_df, collector_name_artnews := i.to] %>% 
    .[, .(collector_name_artnews = unlist(ctstrsplit(collector_name_artnews, ";"))), by = "founder_name_pmdb"]


## different ways of debugging the count
## {.[, .N, founder_name_pmdb][, .N, N]}    
## pull(founder_name_pmdb) %>% table() %>% table()



## artnews_name_check_dt[scramblematch("Schaufler", collector_name_artnews)]
    

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




## this is what I have to replicate in data.table 
artnews_name_check_tbl <- artnews_name_check_dt %>%
    mutate(collector_name_artnews = recode(collector_name_artnews, !!!conversions_list)) %>%
    separate_rows(collector_name_artnews, sep= ";")

## 
if (len(setdiff(artnews_name_check_dt2$collector_name_artnews, artnews_time_df$clctr_name)) > 0) {
    stop("match incomplete")}



adt(artnews_name_check_dt2)[scramblematch("Kulczyk", collector_name_artnews)]
adt(artnews_name_check_dt2)[scramblematch(";", collector_name_artnews)]




## also need some expansion of rows with ; to multiple rows




## converting back to pm founder name 
dt_founder_crpn <- cbind(founder_res=artnews_name_check_dt$founder_name_pmdb,
      founder_orig = artnews_orig_dt$founder_name_pmdb) %>% adt()

## check manually whether mismatches are just formatting bs 
dt_founder_crpn[founder_orig != founder_res]
## seems to be the case -> can just work with founder_orig




## check, wait for proper encoding

setdiff(df_excl$`Founder name`, artnews_name_check_dt$founder_name_pmdb)

## artnews_name_check_dt[scramblematch("carlos slim", founder_name_pmdb),]

## merge museum information to artnews time_df

merge(
    artnews_name_check_dt[,.(clctr_name = collector_name_artnews, founder_name_pmdb)],
    adt(artnews_time_df), all.x = T)

getOption("datatable.print.trunc.cols")
getOption("datatable.*")
    




artnews_name_check_dt[notes != "", ]



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


ctstrsplit <- function(x, ...) {
    #' custom version of tstrsplit that works with empty strings 
    res <- tstrsplit(x, ...)
    
    ## if nothing gets returned, return original string
    if (len(res) == 0) {
        res <- x
    }        
     
    return(res)
}

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

## ** recoding, generally chaining/piping dt operations
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

