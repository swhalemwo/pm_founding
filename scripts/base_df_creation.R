## * base df creation

vrbl_fndr <- function(df, penl_vrbls) {
    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1 | len(vrbl) == 0 ) {
        stop("variable unclear: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else {
        vrbl
    }
}


create_excel_df <- function(db_file, only_pms=T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' read the excel sheet into R

    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database.xlsx")
    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database2.xlsx")

    ## 2: use more recent version
    
    df <- read_excel(paste0(PMDB_DIR, db_file))
    
    ## removing header stuff 
    nrows <- nrow(df)
    df <- df[2:nrows,]

    ## more flexible renaming 
   
    df$country <- pull(df, vrbl_fndr(df, c("Country where museum is located", "Museum_country")))
    df$name <- pull(df, vrbl_fndr(df, c("Museum_name", "Name of museum")))

    ## df$name <- df[1] # reeeee-name because column names get changed
    ## df$year_opened_str <- df$"Opening year"
    df$year_opened_str <- pull(df, vrbl_fndr(df, c("Opening year", "Museum_opening year")))

    df$year_closed <- as.integer(pull(df, vrbl_fndr(df, c("Closing year", "Museum_closing year"))))
    
    df$ID <- as.integer(df$ID)
    df$museum_status <- pull(df, vrbl_fndr(df, c("Museum status", "Museum_status")))
    
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



create_excel_df_diagnose <- function(df, verbose = F){
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' check status of base df
    ## debugging unclear/missing countries: atm 12 cases

    country_test <- filter(df, is.na(countrycode)) %>% select(ID, name, countrycode, museum_status)


    print(paste0("non-perfect countries: ", nrow(country_test)))
    if (verbose){
        print(as.data.frame(country_test))
    }

    year_opened_test <- filter(df, is.na(year_opened_int)) %>%
        select(ID, name, year_opened_str, year_opened_int, museum_status)
    
    print(paste0("non-perfect opening years: ", nrow(year_opened_test)))
    if (verbose) {
        print(as.data.frame(year_opened_test))
    }
    ## could functionalize this diagnosis properly with mapping variables to sort, with additional variables to be selected
    ## but don't think I need it as this point, don't update data that much
}

## dfx <- create_excel_df(PMDB_FILE)
## create_excel_df_diagnose(dfx, verbose =0)

## create_excel_df(PMDB_FILE) %>% create_excel_df_diagnose(verbose = F)


aggregate_openings <- function(df_excl, yeet_early_closed = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' aggregate excel df into country-year openings

    df_excl2 <- select(df_excl, name, country, countrycode, year_opened_int, year_closed) %>%
        ## na.omit() 
        filter(!is.na(year_opened_int))
    ## excel-based df

    ## if yeeting early closed: yeet the museums closed before 2000: atm  4 in US, 1 in NLD
    ## hopefully creates better closing measurements:
    ## otherwise huge percentages of PM population closed in those countries 
    if (yeet_early_closed) {
        df_excl2 <-  filter(df_excl2, year_closed >= 2000 | is.na(year_closed))
    }

    ## select(df_excl, name, country, countrycode, year_opened_int, year_closed) %>% adt() %>% 
        ## .[!complete.cases(.)]
        ## .[is.na(year_opened_int)]
        
    ## generate count of closed 
    df_clsd <- select(df_excl2, name, iso3c = countrycode, year = year_closed) %>% na.omit() %>%
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
               ## prop_closed = nbr_closed_cum/nbr_opened_cum)

    ## df_anls2 %>% group_by(iso3c) %>%
    ##     mutate(any_closed = sum(nbr_closed) > 0) %>%
    ##     filter(any_closed) %>%
    ##     ## viz_lines(y="prop_closed", facets = "region", max_lines = 6)
    ##     ## viz_lines(y="prop_closed")
    ##     viz_lines(y="nbr_closed", duration = 8)

    
    ## viz_lines(filter(df_anls2, iso3c %in% c("ARE", "AUT", "ISL", "NLD", "USA", "SWE", "DEU")), y = "prop_closed", duration = 1)
    ## viz_lines(df_anls2, y = "nbr_closed", duration = 8 )

    ## viz_lines(df_anls2, y="nbr_closed_cum", extra = "rates", div = "SP.POP.TOTL")

    ## ## look at opened per capita again, also skewed but fine
    ## df_anls2 %>%
    ##     mutate(opened_pop_prop = nbr_opened_cum/SP.POP.TOTL) %>%
    ##     filter(iso3c %!in% c("MCO", "LIE", "ISL")) %>% 
    ##     viz_lines(y = "opened_pop_prop")

    
    ## ## look again at closed per capita, idk why, i know CHE and ISL are outliers 
    ## df_anls2 %>%
    ##     mutate(closed_pop_prop = nbr_closed_cum/SP.POP.TOTL) %>%
    ##     filter(iso3c %!in% c("ISL", "CHE")) %>% 
    ##     viz_lines(y = "closed_pop_prop")
    


    ## viz_lines(df_anls2, y="nbr_opened_cum", extra = "rates", div = "SP.POP.TOTL")

    ## cbn_dfs_counts_uscld$cbn_all %>% filter(year == 2010) %>% 
    ##     ggplot(aes(x=log(SP.POP.TOTLm_lag0), y=smorc_dollar_fxm_lag0)) +
    ##     geom_point()


    ## viz_lines(df_anls2, y = "nbr_closed", duration = 1)

    ## viz_lines(cbn_dfs_counts$cbn_no_cult_spending_and_mitr, y="nbr_opened", duration = 12)
    ## viz_lines(cbn_dfs_counts$cbn_all, y="nbr_opened", div = "SP_POP_TOTLm_lag0_uscld", extra = "rates")

    ## filter(cbn_dfs_counts_uscld$cbn_all, year < 2000, nbr_closed_cum_lag0 > 0) %>% adt()
        
    ## filter(df_excl, year_closed < 2000) %>% select(countrycode, year_opened_int, year_closed)
    ## filter(df_excl, countrycode == "USA", year_opened_int < 2000) %>% select(year_opened_int, year_closed)

    ## adt(df_excl)[year_closed < 2000, .(...1, year_opened_int, year_closed, countrycode)]
    ## adt(df_excl)[year_opened_int < 2000, .(...1, year_opened_int, year_closed, countrycode)]

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
