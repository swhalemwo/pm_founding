## * artnews collector ranking


ARTNEWS_DIR <- paste0(PROJECT_DIR, "data/artnews/")

## ** some helper functions to check uniqueness


generate_unq_nbrs <- function(dfx, group, vlu) {
    #' generates the df to check change over time 
                              
    dfx_unq_cntd <- dfx %>%
        group_by_at(group) %>%
        mutate(nbr_unq = n_distinct(get(vlu)))

    return (dfx_unq_cntd)
}

generate_unq_nbrs_long <- function(dfx, group, vlu, aux_group) {
    #' generates the count of unique values for long variables,
    #' aux_group needed to specify among which variable(s) vlus should be compared

    ## filter(dfx_unq_cntd, clctr_name == "Karl-Heinrich Müller") %>%
    ##     select_at(c(group, vlu, "nbr_unq")) %>%
    ##     as.data.frame()

    # value for vlu can't be "vlu" for some reason

    ## arrange sorts the vlues within group and aux group
    vlus_in_list <- dfx %>%
        group_by_at(c(group, aux_group)) %>%
        arrange(get(vlu)) %>%
        summarize(unq_vlus = list(unique(get(vlu))))

    dfx_unq_cntd <- vlus_in_list %>%
        group_by_at(group) %>%
        mutate(nbr_unq = n_distinct(unq_vlus))
    
    
    dfx_unq_cntd[,c(vlu)] <- list(dfx_unq_cntd$unq_vlus)

    return(dfx_unq_cntd)
}



identical_checker <- function(dfx, vlu, group, aux_group=F) {
    #' checks whether values of vlu are identical for every entry of group, returns TRUE if they are identical
    #' multi-value variables in long format can be passed by specifying aux_group
    
    if (aux_group != FALSE) {
        dfx_unq_cntd <- generate_unq_nbrs_long(dfx, group, vlu, aux_group)
    } else {
        dfx_unq_cntd <- generate_unq_nbrs(dfx, group, vlu)
    }
        
    return_value <- FALSE

    if (max(dfx_unq_cntd$nbr_unq)==1){
        return_value <- TRUE
    }

    return(return_value)
}



get_changed_values <- function(dfx, vlu, group, verbose=F, aux_group = F) {
    #' report the non-unique values per group
    #' also statistics would be nice

    
    if (aux_group != FALSE) {
        dfx_unq_cntd <- generate_unq_nbrs_long(dfx, group, vlu, aux_group)
    } else {
        dfx_unq_cntd <- generate_unq_nbrs(dfx, group, vlu)
    }

    prop_calc_df <- dfx_unq_cntd %>%
        group_by_at(group) %>%
        summarize(group=sample(group,1), nbr_unq=sample(nbr_unq,1))
    
    prop_ones <- len(which(prop_calc_df$nbr_unq==1))/nrow(prop_calc_df)
    cnt_non_ones <- len(which(prop_calc_df$nbr_unq != 1))

    print(paste0(cnt_non_ones, " groups have different values, ", round(100* prop_ones,3),"% are identical"))

    if (verbose) {

        non_identical_values <- filter(dfx_unq_cntd, nbr_unq > 1) %>%
            select_at(c(group,vlu)) %>%
            unique()
                
        ## print(non_identical_values)
    }
    

    return_obj <- list(
        prop_ones = prop_ones,
        cnt_non_ones = cnt_non_ones)
    
    ## add the non-identical stuff if verbose is required
    if (verbose) {
        return_obj$non_identical_values = non_identical_values
    }
        
    return(return_obj)

}
           

## *** testing
## check that

testing <- FALSE

if (testing) {
generate_unq_nbrs(artnews_sep, group = "clctr_name", vlu="collection_focus")


## check that both nbr_unq and vlu column (genre) are properly generated for variable in long format
generate_unq_nbrs_long(artnews_df_genre, group = "clctr_name", vlu="genre", aux_group = "year")

## compare long and short generate_unq_numbers
generate_unq_nbrs(artnews_df_genre, group = "clctr_name", vlu="collection_focus")

## generating test df, testing that nbr_unq and vlu column (vlux) are properly generated
test_df <- data.frame(idx=c(1,1,1,1,2,2,2,2),timex = c(1,1,2,2,1,1,2,2), vlux= c("b","a", "b", "b", "a", "b", "a", "b"))
generate_unq_nbrs_long(test_df, group="idx", vlu="vlux", aux_group = "timex")


## collection_focus string has some changes, is not identical for each collector
generate_unq_nbrs(artnews_sep, group = "clctr_name", vlu="collection_focus")
identical_checker(artnews_sep, vlu="collection_focus", group="clctr_name") 
get_changed_values(artnews_sep, vlu="collection_focus", group="clctr_name", verbose = T)

## collection focus is identical for every collector-year tho (duh, since only one observatin per collector year)
identical_checker(artnews_sep, vlu="collection_focus", group=c("clctr_name", "year"))

## test_df non-identical values are correctly recognized
get_changed_values(test_df, vlu="vlux", group="idx", aux_group = "timex", verbose = T)

## values for genre are the same for a collector for all years
identical_checker(artnews_df_genre, vlu="genre", group="clctr_name", aux_group = "year")
get_changed_values(artnews_df_genre, vlu="genre", group="clctr_name", aux_group = "year", verbose = T)


## location values also don't change over time 
get_changed_values(artnews_sep, vlu="location", group="clctr_name", aux_group = "year", verbose = T)
}


## ** actually artnews

readin_artnews_all <- function() {
    #' just read in the artnews df, fix the country coding
    #' returns
    #' - time_df: clctr_name, year,
    #' - location_df: clctr_name, location(s)
    #' - maybe focus_df: clctr_name, collection_focus
    #' -> if i want to combine different dfs, I have to join stuff together

    artnews_df_all <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "ranking.csv")))

    ## can use countrycode, multiple matches return NA -> will be manually coded
    artnews_df_all$country1 <- countrycode(artnews_df_all$location, "country.name", "iso3c")

    ## manually countrycoding the rest like a fucking LOSER
    artnews_country_dict <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/git_files/artnews_loc_table.csv")))

    ## reading the manually coded countrycodes in
    artnews_cbn <- as_tibble(merge(artnews_df_all, artnews_country_dict, all.x = T))

    artnews_cbn$country3 <- ifelse(!is.na(artnews_cbn$country1), artnews_cbn$country1, artnews_cbn$country2)

    ## filter(artnews_cbn, is.na(country1) & is.na(country2))$location
    ## 14 people have no location
    ## filter(artnews_cbn, clctr_name == "[]")
    ## 5 have no name, could check them out since I have their position
    ## filter(artnews_cbn, collection_focus == "[]")
    ## 207 have no collection focus

    artnews_sep <- separate_rows(artnews_cbn, country3, sep=";")

    artnews_time_df <- artnews_sep %>%
        select(clctr_name, year) %>%
        unique()

    ## locations don't change over time -> fine to not store location with time
    identical_checker(artnews_sep, vlu="country3", group="clctr_name", aux_group = "year")
    
    artnews_loc_df <- artnews_sep %>%
        select(clctr_name, country=country3) %>%
        unique()

    ## collection focus: some minor renmaing
    identical_checker(artnews_sep, vlu="collection_focus", group="clctr_name", aux_group = "year")
    get_changed_values(artnews_sep, vlu="collection_focus", group="clctr_name", verbose = T) 
    ## only inconsistency is "Postwar and contemporary art" and "Contemporary art; Postwar art" for "Stefan T. Edlis and Gael Neeson" -> fine to recode
    artnews_sep$collection_focus[which(artnews_sep$clctr_name == "Stefan T. Edlis and Gael Neeson" & 
                                       artnews_sep$collection_focus == "Postwar and contemporary art")] <- "Contemporary art; Postwar art"
    
    artnews_collection_df <- artnews_sep %>%
        select(clctr_name, collection_focus) %>%
        unique()

    return(list(
        artnews_time_df=artnews_time_df,
        artnews_loc_df=artnews_loc_df,
        artnews_collection_df=artnews_collection_df))
}



    

readin_artnews_genre <- function() {
    #' read in genre-classified artnews ranking
    #' just use it to generate the genre df
    
    artnews_df_genre <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "ranking_genre.csv")))

    ## genres don't change over time
    identical_checker(artnews_df_genre, group = "clctr_name", vlu="genre", aux_group = "year")

    artnews_genre_df <- artnews_df_genre %>%
        select(clctr_name, genre) %>%
        unique()
    
    return (artnews_genre_df)
        
}


## ** genre combination

get_cpaer_clctrs <- function(artnews_genre_df, artnews_collection_df) {
    #' combine artnews_genre_df and artnews_collection_df, melt them and match collectors associated with modern/contemporary art

    genre_cbn <- as_tibble(merge(artnews_collection_df, artnews_genre_df, all = T))

    ## melting? idk if best way, but has some brutal simplicity
    genre_melt <- as_tibble(melt(genre_cbn, id=c("clctr_name")))

    rel_art_str <- "modern|contempor|minimalism|conceptual|pop|postwar|expressionis|20th|abstract|last 20|surrealism|1990|1945|Salvador Dalí|cubis|1980s"

    genre_melt$cpaer <- grepl(rel_art_str, genre_melt$value, ignore.case = T)

    ## checking non-matched values, think by now I got them all
    filter(genre_melt, cpaer == FALSE)$value %>%
                                     unique()

    cpaer_clctrs <- filter(genre_melt, cpaer == T) %>%
        select(clctr_name) %>%
        unique()
    ## 728 collectors when combining

    ## ## see if it was worth it
    ## artnews_collection_df$cpaer <- grepl(rel_art_str, artnews_collection_df$collection_focus, ignore.case = T)
    ## filter(artnews_collection_df, cpaer == T) %>%
    ##     select(clctr_name) %>%
    ##     unique()
    ## ## 599 when only looking at collection focus

    ## artnews_genre_df$cpaer <- grepl(rel_art_str, artnews_genre_df$genre, ignore.case = T)
    ## filter(artnews_genre_df, cpaer == T) %>%
    ##     select(clctr_name) %>%
    ##     unique()
    ## ## 459 when only using the areas on the website
    ## ## -> effort was worth it :)))

    return(cpaer_clctrs)
}

generate_artnews_data <- function() {

    artnews_all_res <- readin_artnews_all()
    artnews_time_df <- artnews_all_res$artnews_time_df
    artnews_loc_df <- artnews_all_res$artnews_loc_df
    artnews_collection_df <- artnews_all_res$artnews_collection_df
    ## table(artnews_time_df$year)

    artnews_genre_df <- readin_artnews_genre()

    ## getting the relevant (contemporary/modern art collectors from all and genre dfs)
    rel_clctrs <- get_cpaer_clctrs(artnews_genre_df, artnews_collection_df)
    ## joining with location 
    rel_clctrs_loc <- as_tibble(merge(rel_clctrs, artnews_loc_df))

    ## joining with time 
    rel_clctrs_time <- as_tibble(merge(rel_clctrs_loc, artnews_time_df))

    ## also generating count for all artcollectors, not just modern/contemporary
    all_clctrs <- na.omit(as_tibble(merge(artnews_time_df, artnews_loc_df)))

    ## generate count of contemporary
    cnt_cpaer <- rel_clctrs_time %>%
        group_by(country, year) %>%
        summarize(cnt_cpaer = len(clctr_name))

    ## generate count of all 
    cnt_all <- all_clctrs %>%
        group_by(country, year) %>%
        summarize(cnt_all = len(clctr_name))

    ## merge
    cnt_clctrs <- as_tibble(merge(cnt_cpaer, cnt_all, all = T))
    ## need to fill up missing values with 0, idk if best here or elsewhere

    fill_up_cols <- c("cnt_cpaer", "cnt_all")
    cnt_clctrs[fill_up_cols][is.na(cnt_clctrs[fill_up_cols])] <- 0

    cnt_clctrs$clctr_cpaer_all_diff <- cnt_clctrs$cnt_all - cnt_clctrs$cnt_cpaer

    cnt_clctrs <- cnt_clctrs %>%
        rename(iso3c=country)

    return (select(cnt_clctrs,
                   iso3c, year, 
                   clctr_cnt_cpaer = cnt_cpaer,
                   clctr_cnt_all = cnt_all,
                   clctr_cpaer_all_diff = clctr_cpaer_all_diff))
}



artnews_descriptives <- function(cnt_clctrs) {
    #' generates descriptive plots for artnews ranking

    cnt_clctrs$region <- countrycode(cnt_clctrs$iso3c, "iso3c", "un.region.name")


    pdf(paste0(FIG_DIR, "artnews_ranking_diff.pdf"), width = 17, height = 10)
    viz_lines(na.omit(cnt_clctrs), x="year", y="clctr_cpaer_all_diff", grp = "iso3c", facets = "region", time_level = "ra", duration = 2, fill_up = T, max_lines = 6)
    dev.off()

    pdf(paste0(FIG_DIR, "artnews_ranking_all.pdf"), width = 17, height = 10)
    viz_lines(na.omit(cnt_clctrs), x="year", y="cnt_all", grp = "iso3c", facets = "region", time_level = "ra", duration = 2, fill_up = T, max_lines = 6)
    dev.off()

    pdf(paste0(FIG_DIR, "artnews_ranking_cpaer.pdf"), width = 17, height = 10)
    viz_lines(na.omit(cnt_clctrs), x="year", y="cnt_cpaer", grp = "iso3c", facets = "region", time_level = "ra", duration = 2, fill_up = T, max_lines = 6)
    dev.off()
    
}




