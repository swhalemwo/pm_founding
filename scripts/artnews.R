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

    ## sort   
    vlus_in_list <- dfx %>%
        group_by_at(c(group, aux_group)) %>%
        arrange(get(vlu)) %>%
        summarize(unq_vlus = list(unique(get(vlu))))

    dfx_unq_cntd <- vlus_in_list %>%
        group_by_at(group) %>%
        summarize(nbr_unq = n_distinct(unq_vlus))
    
    return(dfx_unq_cntd)
}


generate_unq_nbrs_long(artnews_df_genre, group = "clctr_name", vlu="genre", aux_group = "year")

## test_df <- data.frame(idx=c(1,1,1,2,2),vlu= c("b","a", "b", "b", "a"))

## test_df %>%
##     group_by(idx) %>%
##     arrange(vlu) %>%
##     summarize(unq_vlus = list(unique(vlu))) %>%
##     as.data.frame()



identical_checker <- function(dfx, vlu, group) {
    #' checks whether values of vlu are identical for every entry of group, returns TRUE if they are identical
    
    dfx_unq_cntd <- generate_unq_nbrs(dfx, group, vlu)
        
    return_value <- FALSE

    if (max(dfx_unq_cntd$nbr_unq)==1){
        return_value <- TRUE
    }

    return(return_value)
}


get_changed_values <- function(dfx, vlu, group, verbose=F) {
    #' report the non-unique values per group
    #' also statistics would be nice
    

    dfx_unq_cntd <- generate_unq_nbrs(dfx, group, vlu)
    
    

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
                
            print(non_identical_values)
    }

    if (verbose == F) {

        return_obj <- list(
            prop_ones = prop_ones,
            cnt_non_ones = cnt_non_ones)
    } else {
        return_obj <- list(
            prop_ones = prop_ones,
            cnt_non_ones = cnt_non_ones,
            non_identical_values = non_identical_values)
        
    }
    return(return_obj)

}
           


identical_checker(artnews_sep, vlu="collection_focus", group="clctr_name")
identical_checker(artnews_sep, vlu="collection_focus", group=c("clctr_name", "year"))

get_changed_values(artnews_sep, vlu="collection_focus", group="clctr_name", verbose = T)

## ** actually artnews

readin_artnews <- function() {
    #' just read in the artnews df, fix the country coding
    
    artnews_df <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "ranking.csv")))


    unique(artnews_df$location)

    ## can use countrycode
    ## multiple matches return NA -> will be manually coded
    artnews_df$country1 <- countrycode(artnews_df$location, "country.name", "iso3c")

    ## manually countrycoding the rest like a fucking LOSER
    artnews_country_dict <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "artnews_loc_table.csv")))

    ## reading the manually coded countrycodes in
    artnews_cbn <- as_tibble(merge(artnews_df, artnews_country_dict, all.x = T))

    artnews_cbn$country3 <- ifelse(!is.na(artnews_cbn$country1), artnews_cbn$country1, artnews_cbn$country2)

    ## filter(artnews_cbn, is.na(country1) & is.na(country2))$location
    ## 14 people have no location
    ## filter(artnews_cbn, clctr_name == "[]")
    ## 5 have no name, could check them out since I have their position
    ## filter(artnews_cbn, collection_focus == "[]")
    ## 207 have no collection focus


    artnews_sep <- separate_rows(artnews_cbn, country3, sep=";")
    ## table(artnews_sep$country3) %>% sort()
    ## countrycode(unique(artnews_sep$country3), "iso3c", "country.name")    

    return (select(artnews_sep, location, clctr_name, collection_focus, industry, year, country=country3))
}

artnews_sep <- readin_artnews()
## 14 

readin_artnews_genre <- function() {
    #' read in genre-classified artnews ranking

    
    artnews_df <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "ranking_genre.csv")))

    ## artnews_df %>%
    ##     group_by(year) %>%
    ##     summarize(clctr_nbr = len(unique(clctr_name))) %>%
    ##     as.data.frame() %>%
    ##     plot(type='l')

    select(artnews_df, clctr_name, year, collection_focus, genre)

        
}


## number of genres doesn't change for collector over year, content could still change 
    d1 <- artnews_df %>%
        group_by(clctr_name, year) %>%
        summarize(nbr_genres_year = len(unique(genre))) %>%
        group_by(clctr_name) %>%
        summarize(nbr_genres_clctr_min = min(unique(nbr_genres_year)),
                  nbr_genres_clctr_max = max(unique(nbr_genres_year))
                  )
    ## max and min are the same
    d1$nbr_genres_clctr_min - d1$nbr_genres_clctr_max


    d2 <- artnews_df %>%
        group_by(clctr_name) %>%
        summarize(nbr_genres = len(unique(genre)))
        
    d3 <- as_tibble(merge(d1,d2))
    d3$diff <- d3$nbr_genres_clctr_max - d3$nbr_genres
    hist(d3$diff)

readin_artnews_genre()

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

        
