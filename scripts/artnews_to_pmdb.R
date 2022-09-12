## * calculate artnews-top200 collector based stats for pmdb

ARTNEWS_ORIG_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names.csv"
ARTNEWS_RES_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names_UTF.csv"



check_for_ids <- function(df_excl) {
    #' check if every museum has an ID

    if (any(is.na(df_excl$ID))) {
        stop("some museums don't have IDs")
    }
}

ctstrsplit <- function(x, ...) {
    #' custom version of tstrsplit that works with empty strings 
    res <- tstrsplit(x, ...)
    
    ## if nothing gets returned, return original string
    if (len(res) == 0) {
        res <- x
    }        
     
    return(res)
}


gen_artnews_names_fixed <- function(artnews_name_check_dt, antdf){
    #' generate the artnews_name_check_dt: artnews collector names now correspond to artnews_dfs
        
    an_clctrs_nomatch <- artnews_name_check_dt[!antdf, on=.(collector_name_artnews = clctr_name)] %>% 
        .[!is.na(collector_name_artnews)]

    ## artnews_name_check_dt[findt("brandhorst", collector_name_artnews)]
    ## has semicolon, 
    ## antdf[findt("brandhorst", clctr_name)]
    ## none have semicolons
    ## the brandhorst row in artnews_name_check_dt shouldn't be removed
    ## hmm it probably isn't

    ## i think because there's some ugly empty string somewhere
    ## hmm nope: antdf doesn't have empty string, so it doesn't remove them from artnews_name_check_dt

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
        "Daniel and Judith Terra; Daniel Terra" = "Daniel and Judith Terra;Daniel Terra",
        "Gra_yna Kulczyk" = "Grażyna Kulczyk")

    conversion_df <- data.table(collector_name_artnews = names(conversions_list),
                                to = unlist(conversions_list))

    if (nrow(an_clctrs_nomatch[!conversion_df, on=.NATURAL]) > 0) {
        print(an_clctrs_nomatch[!conversion_df, on=.NATURAL])
        stop("not all artnews names converted")
    } 


    ## combining with splitting long
    artnews_name_check_dt2 <- artnews_name_check_dt %>% copy() %>%
        .[,on=conversion_df, collector_name_artnews := i.to] %>%
        .[, .(collector_name_artnews = unlist(ctstrsplit(collector_name_artnews, ";"))), by = "founder_name_pmdb"]

    ## artnews_name_check_dt[scramblematch("Schaufler", collector_name_artnews)]

    ## this is what I have to replicate in data.table 
    ## artnews_name_check_tbl <- artnews_name_check_dt %>%
    ##     mutate(collector_name_artnews = recode(collector_name_artnews, !!!conversions_list)) %>%
    ##     separate_rows(collector_name_artnews, sep= ";")

    ## NA is allowed as match, will always be there -> >1 should be good catch of errors
    if (len(setdiff(artnews_name_check_dt2$collector_name_artnews, antdf$clctr_name)) > 1) {
        stop("match of artnews collectors incomplete")}
    
    return(artnews_name_check_dt2)
}

gen_pm_founder_names_fixed <- function(artnews_name_check_dt2, artnews_orig_dt, df_excl) {
    ## fix the mismatches between res and orig: some are due to encoding weirdness, one (family) due to name changes
    
    dt_founder_crpn <- data.table(founder_res=unique(artnews_name_check_dt2$founder_name_pmdb),
                                  founder_orig = artnews_orig_dt$founder_name_pmdb)

    ## check manually whether mismatches are just formatting BS 
    founder_name_mismatches <- dt_founder_crpn[founder_orig != founder_res]
    ## seems to be the case -> can just work with founder_orig -> can just recode

    ## some manual adjustments

    ## not sure if Kemper is escape error or something else
    ## is probably stuff being read in incorrectly from orig_dt: fread throws some warning
    founder_name_mismatches[scramblematch("crosby kemper", founder_res),
                            founder_orig := "R. Crosby Kemper Jr. and Mary \"Bebe\" Hunt"]

    ## Family changed to Copelouzos Family after df_orig was created
    founder_name_mismatches <- rbind(founder_name_mismatches,
                                     data.table(founder_res = "Copelouzos Family",
                                                founder_orig = "Copelouzos Family"))

    ## actually do the recoding with update join 
    artnews_name_check_dt3 <- artnews_name_check_dt2 %>% copy() %>%
        .[founder_name_mismatches, founder_name_pmdb := i.founder_orig, on=.(founder_name_pmdb = founder_res)]

    ## artnews_name_check_dt3[founder_name_pmdb == "Copelouzos Family"]
    ## df_excl %>% filter(`Founder name` %in% "Cope")

    

    ## some test, idk what it does
    ## checks whether there are founders in artnews that are not in pmdb???
    ## ofc there are... 
    if (len(setdiff(artnews_name_check_dt3$founder_name_pmdb, df_excl$`Founder name`)) > 1) {
        print(setdiff(artnews_name_check_dt3$founder_name_pmdb, df_excl$`Founder name`))
        stop("match of museum founders incomplete")}

    return(artnews_name_check_dt3)
}




gen_merge_res <- function(df_excl) {
    #' generate the merge res: artnews_time with fitting founder name 
    

    check_for_ids(df_excl)

    artnews_orig_dt <- fread(ARTNEWS_ORIG_FILE, sep = ";")

    ## replacing some weird pattern: ¬† which is supposed to be non-breaking space
    artnews_name_check_dt <- fread(ARTNEWS_RES_FILE, sep = ";", na.strings = "") %>%
        .[, collector_name_artnews := gsub("¬†", " ", collector_name_artnews)]

    ## some replacing of weird non-breaking space that crept already into python coding
    artnews_all_res <- readin_artnews_all()

    ## antdf: artnews time df
    antdf <- artnews_all_res$artnews_time_df %>% adt() %>%
        .[, clctr_name := gsub(" ", " ", clctr_name)]

    ## fix the artnews names
    artnews_name_check_dt2 <- gen_artnews_names_fixed(artnews_name_check_dt, antdf)

    artnews_name_check_dt3 <- gen_pm_founder_names_fixed(artnews_name_check_dt2, artnews_orig_dt, df_excl)


    ## confirm with merge() (dt and tbl), dt[] seems to be robust 
    ## merge(adt(artnews_time_df), artnews_name_check_dt3, all.x = T, by.x = "clctr_name", by.y = "collector_name_artnews", allow.cartesian = T)

    ## merge(artnews_time_df, atb(artnews_name_check_dt3), all.x = T, by.x = "clctr_name", by.y = "collector_name_artnews") %>% atb()

    ## matching to pmdb
    ## converting back to pm founder name
    ## first get original names back

    ## artnews_name_check_dt3[collector_name_artnews != "" & founder_name_pmdb != collector_name_artnews]

    merge_res <- artnews_name_check_dt3[antdf, on=.(collector_name_artnews = clctr_name)]

    return(merge_res)
}



## merge_res[scramblematch("alonso", founder_name_pmdb)]

## table(merge_res$founder_name_pmdb) %>% adf() %>% adt() %>% .[order(-Freq)]

## aggregate res now works without having to specify collector_name_artnews != "",

export_an_data_to_gdocs <- function() {
    #' wrapper function for exporting the artnews data to google docs db 

    df_excl <- create_excel_df(PMDB_FILE, only_pms = F)

    merge_res <- gen_merge_res(df_excl)

    an_agg_res <- merge_res[!is.na(founder_name_pmdb),
                            .(artnews_first_year = min(year),
                              artnews_last_year = max(year),
                              artnews_nbr_years = len(year),
                              artnews_nbr_clctrs = len(unique(collector_name_artnews)))
                           ,by = founder_name_pmdb]



    ## an_agg_res[artnews_nbr_years == 1]

    ## mtcars %>%
    ##     atb() %>%
    ##     group_by(gear) %>% 
    ##     filter(cyl == 4)



    setdiff(an_agg_res$founder_name_pmdb, df_excl$`Founder name`)


    an_dt_gdocs <- an_agg_res[adt(df_excl)[, .(`Founder name`, ID = as.numeric(ID))],
                              on=.(founder_name_pmdb = `Founder name`)]

    an_dt_gdocs[artnews_nbr_clctrs >1]

    print(an_dt_gdocs[order(-artnews_nbr_years)], n=150)



    GDOCS_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/gdocs_res.csv"

    write.table(an_dt_gdocs[order(ID)], file = GDOCS_FILE, row.names = F)

    ## check some things, idk 
    ## adt(df_excl)[`Founder name` != founder_name_pmdb, .(`Founder name`, founder_name_pmdb)]
    ## adt(df_excl)[as.numeric(`ID...18`) != `ID...120`, .(`Founder name`, founder_name_pmdb, `ID...18`, `ID...120`)]

}


## an_agg_res[order(-nbr_an_clctrs)]



## check, wait for proper encoding

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

