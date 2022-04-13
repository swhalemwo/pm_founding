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
    artnews_country_dict <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "artnews_loc_table.csv")))

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



artnews_all_res <- readin_artnews_all()
artnews_time_df <- artnews_all_res$artnews_time_df
artnews_loc_df <- artnews_all_res$artnews_loc_df
artnews_collection_df <- artnews_all_res$artnews_collection_df



table(artnews_time_df$year)
    

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

artnews_genre_df <- readin_artnews_genre()

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

rel_clctrs <- get_cpaer_clctrs(artnews_genre_df, artnews_collection_df)
    





## ** contemporary


artnews_sep$cpaer <-
    table(grepl("modern|contempor|minimalism|conceptual|pop|postwar|expressionism|20th|abstract|last 20|", artnews_sep$collection_focus, ignore.case = T))

unique(artnews_sep$collection_focus)

artnews_sum <- artnews_sep %>%
    select(country3, year, X) %>%
    na.omit() %>%
    group_by(iso3c=country3, year) %>%
    summarize(cnt=len(X))

artnews_sum$region <- countrycode(artnews_sum$iso3c, "iso3c", "un.region.name")

pdf(paste0(FIG_DIR, "artnews_ranking.pdf"), width = 14, height = 10)
viz_lines(na.omit(artnews_sum), x="year", y="cnt", grp="iso3c", facets = "region", time_level = "ra", duration = 2, fill_up = T, max_lines = 6)
dev.off()


## ** trying to code location automatically, fails due to lack of standardization, and coding only a few hundred entries manually is fine
## library(maps)

## data(world.cities)
## world_cities <- as.tibble(world.cities)

## world_cities[match(c("New York", "London", "Edinburgh"), world_cities$name),]

## cities <- as.data.frame(c("New York", "London", "Edinburgh"))
## names(cities) <- "name"
## cities$xx <- 1

## merge_test <- as_tibble(merge(world_cities, cities, all.x = T))

## filter(merge_test, !is.na(xx)) %>%
##     group_by(name) %>%
##     top_n(1, pop)

## ## use separate_rows, then merge with world_cities

## sep_rows <- filter(artnews_df, is.na(country1)) %>%
##     separate_rows(location, convert = T, sep=',|;') %>%
##     select(name=location, year, clctr_name)

## ## filter(sep_rows, clctr_name=="Aaron I. Fleischman")
## filter(sep_rows, name=="Beirut")

## sep_rows$name <- trimws(sep_rows$name)

## ## dropping rows that are in the same country, but different cities
## artnews_city_infer <- as_tibble(merge(
##     select(world_cities, name, country.etc, pop),
##     sep_rows))

## filter(artnews_city_infer, name=="Beirut")

## %>%
##     select(country2=country.etc, year, clctr_name) %>% 
##     unique()


## ## i need it in long format because collectors can be in same country
## ## at the end group_by(year, len(unique(clctr_name)) has to be 200 for all years


## artnews_cbn <- as_tibble(merge(artnews_df, artnews_city_infer, all= T))

## artnews_cbn %>%
##     select(clctr_name, year, location, country1, country2) %>%



      
    

## can probably grep the rest


## ** use grepl for modern/contemporary 

        
