## * calculate artnews-top200 collector based stats for pmdb

ARTNEWS_ORIG_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names.csv"
ARTNEWS_RES_FILE <- "/home/johannes/Dropbox/phd/papers/org_pop/scripts/artnews_cleaning/pm_founder_names_UTF.csv"



check_for_ids <- function(df_excl) {
    #' check if every museum has an ID

    if (any(is.na(df_excl$ID))) {
        stop("some museums don't have IDs")
    }
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

