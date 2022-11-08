## * base df creation

create_excel_df <- function(db_file, only_pms=T) {
    
    #' read the excel sheet into R

    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database.xlsx")
    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database2.xlsx")

    ## 2: use more recent version
    
    df <- read_excel(paste0(PMDB_DIR, db_file))
    
    ## removing header stuff 
    nrows <- nrow(df)
    df <- df[2:nrows,]


    df$country <- df$"Country where museum is located"

    df$name <- df[1] # reeeee-name because column names get changed
    df$year_opened_str <- df$"Opening year"
    df$year_closed <- as.numeric(df$"Closing year / year it changed structure")
    df$ID <- as.numeric(df$ID)
    df$museum_status <- df$`Museum status`
    ## df$museum_closed <- df$"Museum closed"

    ## tbl <- table(df$country)
    ## tbl2 <- tbl[rev(order(tbl))]
    ## tbl2

    ## df[which(df$country == "England"),]$country <- "United Kingdom"


    df$countrycode <- recode(df$country, "United Kingdom" = "GBR", "Spain" = "ESP", "United States" = "USA", "Switzerland" = "CHE" , "India" = "IND", "Greece" = "GRC", "Lebanon" = "LBN", "France" = "FRA", "Estonia" = "EST", "Azerbaijan" = "AZE", "Latvia" = "LVA", "Madagascar" = "MDG", "Indonesia" = "IDN", "Slovakia" = "SVK", "Romania" = "ROU","Argentina" = "ARG","South Korea" = "KOR", "Japan" = "JPN", "Benin" = "BEN", "Bangladesh" = "BGD", "Australia" = "AUS", "Norway" = "NOR", "New Zealand" = "NZL", "Poland" = "POL", "Nigeria" = "NGA", "Portugal" = "PRT", "Serbia" = "SRB","Czech Republic" = "CZE","Senegal" = "SEN", "Puerto Rico" = "PRI", "Taiwan" = "TWN", "Israel" = "ISR", "England" = "GBR", "China" = "CHN", "Germany" = "DEU", "Netherlands" = "NLD", "Italy" = "ITA", "Russia" = "RUS", "Canada" = "CAN", "Hungary" = "HUN", "Belgium" = "BEL", "Sweden" = "SWE", "Finland" = "FIN","Malaysia" = "MYS","Philippines" = "PHL", "Turkey" = "TUR", "Austria" = "AUT", "South Africa" = "ZAF","Thailand" = "THA", "Denmark" = "DNK",  "Mexico" = "MEX", "United Arab Emirates" = "ARE","Brazil" = "BRA", "Hong Kong" = "HKG", "Ukraine" = "UKR", "Kuwait" = "KWT",  "Cyprus" = "CYP", "Monaco" = "MCO", "Iceland" = "ISL", "Kenya" = "KEN", "Singapore" = "SGP", "Iran" = "IRN", "Lithuania" = "LTU", "Liechtenstein" = "LIE", "Morocco" = "MAR", "Jordan" = "JOR", .default= NA_character_)

    ## debugging unclear/missing countries
    ## filter(df, is.na(countrycode)) %>% select(country, name, museum_status)

    df$year_opened_int <- as.integer(lapply(df$year_opened_str, function(x)(substring(x, 0,4))))
    ## as.data.frame(filter(df, is.na(year_opened_int))[,c("year_opened_str", "year_opened_int")])

    ## make ugly conditional filtering
    if (only_pms) {
        df_only_pms <- filter(df, museum_status %in% c("private museum", "no longer a private museum", "closed"))
        
    } else {
        df_only_pms <- df
    }


    return(df_only_pms)
}



create_excel_df_diagnose <- function(df, verbose = 0){
    #' check status of base df
    ## debugging unclear/missing countries: atm 12 cases

    country_test <- filter(df, is.na(countrycode))[,c("country", "name")]
    print(paste0("non-perfect countries: ", nrow(country_test)))
    if (verbose == 1){
        print(as.data.frame(country_test))
    }

    year_opened_test <- filter(df, is.na(year_opened_int))[,c("year_opened_str", "year_opened_int")]
    print(paste0("non-perfect opening years: ", nrow(year_opened_test)))
    if (verbose == 1){
        print(as.data.frame(year_opened_test))
    }
    ## could functionalize this diagnosis properly with mapping variables to sort, with additional variables to be selected
    ## but don't think I need it as this point, don't update data that much
}

## dfx <- create_excel_df(PMDB_FILE)
## create_excel_df_diagnose(dfx, verbose =0)




aggregate_openings <- function(df_excl) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' aggregate excel df into country-year openings

    df_excl2 <- select(df_excl, name, country, countrycode, year_opened_int) %>% 
        na.omit()
    ## excel-based df

    ## apply(df_excl2, 2, \(x) sum(is.na(x)))
    
    ## generate count of closed 
    df_clsd <- select(df_excl, name, iso3c = countrycode, year = year_closed) %>% na.omit() %>%
        group_by(iso3c, year) %>% 
        summarize(nbr_closed = n())
    

    df_excl2$ctr <- 1
    df_open_cnt <- as_tibble(aggregate(ctr ~ countrycode + year_opened_int, df_excl2, FUN = sum))
    names(df_open_cnt) <- c('iso3c', 'year', 'nbr_opened')

    df_open_names <- df_excl2 %>% group_by(countrycode, year_opened_int) %>% summarise(name = list(name))
    names(df_open_names) <- c("iso3c", "year", "name")

    ## df_open <- as_tibble(merge(df_open_cnt, df_open_names))

    df_open <- Reduce(\(x,y) full_join(x,y), list(df_open_cnt, df_open_names, df_clsd))

    
    ## test in dt: make sure that previous version and dt produce same results
    dt_open <- adt(df_excl)[, .(name, iso3c = countrycode, year_opened_int, year_closed)] %>%
        melt(id.vars = c("name", "iso3c"), value.name = "year") %>% 
        ## .[!complete.cases(.), .N, variable] # 9 PMs have no opening year, 491 no closing year
        na.omit() %>% 
        .[, .(vlu_proc = .N), by = c("iso3c", "variable", "year")] %>%
        dcast.data.table(iso3c + year ~ variable, value.var = "vlu_proc")
    
    
    nbr_row_diff <- rbind(
        adt(df_open) %>% .[, .(iso3c, year, nbr_opened, nbr_closed)] %>% .[, source := "old"] %>%
        melt(id.vars = c("iso3c", "year", "source")),
        dt_open[, .(iso3c,year,nbr_opened=year_opened_int,nbr_closed=year_closed)] %>% .[,source:="dt"] %>%
        melt(id.vars = c("iso3c", "year", "source"))) %>%
        dcast.data.table(iso3c + year + variable ~ source) %>%
        .[dt != old] %>% nrow()
        
    if (nbr_row_diff != 0) {stop("there is an error in `aggregate_openings`")}
    
    
    return(df_open)
}



## need some df_wb for the complete country-year structure


create_anls_df <- function(df_wb, df_open) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' merge the aggregated excel df to the WB df (as complete country-year structure)
    df_anls <- as_tibble(merge(df_wb, df_open,
                               by=c("iso3c", "year"),
                               all.x = TRUE))

    df_anls2 <- df_anls %>% mutate(
                    region = countrycode(iso3c, "iso3c", "un.region.name"),
                    nbr_opened = ifelse(is.na(nbr_opened), 0, nbr_opened),
                    nbr_closed = ifelse(is.na(nbr_closed), 0, nbr_closed),
                    nbr_opened_cum = ave(nbr_opened, iso3c, FUN= cumsum),
                    nbr_closed_cum = ave(nbr_closed, iso3c, FUN= cumsum),
                    pm_density = nbr_opened_cum - nbr_closed_cum,
                    pm_density_sqrd = pm_density^2) %>%
        group_by(year) %>%
        mutate(nbr_opened_cum_global = sum(nbr_opened_cum),
               nbr_closed_cum_global = sum(nbr_closed_cum),
               pm_density_global = nbr_opened_cum_global - nbr_closed_cum_global,
               pm_density_global_sqrd = pm_density_global^2)

    ## df_anls2 %>% select(pm_closed_cum_glbl) %>% print(n=40)
    


    

    ## filter(df_anls,  iso3c == "DEU") %>% select(year, nbr_opened, nbr_closed, nbr_opened_cum, nbr_closed_cum,
    ##                                             pm_density) %>% print(n=40)
    
    ## df_anls$region <- countrycode(df_anls$iso3c, "iso3c", "un.region.name")

    ## maybe move these things somewhere proper 
    ## df_anls$wv <- 0
    ## df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$iso3c, FUN = cumsum)

    return(df_anls2)
}


## df_excl <- create_excel_df(PMDB_FILE)
## df_open <- aggregate_openings(df_excl)
## df_wb <- get_WB_data("NY.GDP.PCAP.CD")
## df_anls <- create_anls_df(df_wb, df_open)
