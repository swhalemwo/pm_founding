## * artnews collector ranking

ARTNEWS_DIR <- paste0(PROJECT_DIR, "data/artnews/")

artnews_df <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "ranking.csv")))

https://www.artnews.com/art-collectors/top-200-profiles/?filter_collectingarea=contemporary-art&year=1990

https://www.artnews.com/art-collectors/top-200-profiles/?filter_top200year=1995&filter_collectingarea=contemporary-art&page=1

unique(artnews_df$location)

## can use countrycode
## multiple matches return NA -> will be manually coded
artnews_df$country1 <- countrycode(artnews_df$location, "country.name", "iso3c")

## manually countrycoding coding the rest like a fucking LOSER
artnews_country_dict <- as_tibble(read.csv(paste0(ARTNEWS_DIR, "artnews_loc_table.csv")))

artnews_cbn <- as_tibble(merge(artnews_df, artnews_country_dict, all.x = T))

## filter(artnews_cbn, is.na(country1) & is.na(country2))$location
      

artnews_cbn$country3 <- ifelse(!is.na(artnews_cbn$country1), artnews_cbn$country1, artnews_cbn$country2)

artnews_sep <- separate_rows(artnews_cbn, country3, sep=";")
table(artnews_sep$country3) %>% sort()
countrycode(unique(artnews_sep$country3), "iso3c", "country.name")    

artnews_sep$cpaer <-
    table(grepl("modern|contempor|minimalism|conceptual|pop|postwar|expressionism|20th|abstract|last 20|", artnews_sep$collection_focus, ignore.case = T))

unique(artnews_sep$collection_focus)

artnews_sum <- artnews_sep %>%
    select(country3, year, X) %>%
    na.omit() %>%
    group_by(iso3c=country3, year) %>%
    summarize(cnt=len(X))

artnews_sum$region <- countrycode(artnews_sum$iso3c, "iso3c", "un.region.name")

viz_lines(na.omit(artnews_sum), x="year", y="cnt", grp="iso3c", facets = "region", time_level = "ra", duration = 2, fill_up = T, max_lines = 6)


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

        
