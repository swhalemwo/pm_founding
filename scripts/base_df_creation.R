## * base df creation

vrbl_fndr <- function(df, penl_vrbls) {
    #' find the variable referred by by potential variables (penl_vrbls)

    vrbl <- intersect(names(df), penl_vrbls)
    if (len(vrbl) > 1)  {
        stop("multiple variables found: referred to by ", paste0(penl_vrbls, collapse = " - "))
    } else if (len(vrbl) == 0 ) {
        stop("no variable found: referred to by ", paste0(penl_vrbls, collapse = " - "))
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


    df$countrycode <- recode(df$country, "United Kingdom" = "GBR", "Spain" = "ESP", "United States" = "USA", "Switzerland" = "CHE" , "India" = "IND", "Greece" = "GRC", "Lebanon" = "LBN", "France" = "FRA", "Estonia" = "EST", "Azerbaijan" = "AZE", "Latvia" = "LVA", "Madagascar" = "MDG", "Indonesia" = "IDN", "Slovakia" = "SVK", "Romania" = "ROU","Argentina" = "ARG","South Korea" = "KOR", "Japan" = "JPN", "Benin" = "BEN", "Bangladesh" = "BGD", "Australia" = "AUS", "Norway" = "NOR", "New Zealand" = "NZL", "Poland" = "POL", "Nigeria" = "NGA", "Portugal" = "PRT", "Serbia" = "SRB","Czech Republic" = "CZE","Senegal" = "SEN", "Puerto Rico" = "PRI", "Taiwan" = "TWN", "Israel" = "ISR", "England" = "GBR", "China" = "CHN", "Germany" = "DEU", "Netherlands" = "NLD", "Italy" = "ITA", "Russia" = "RUS", "Canada" = "CAN", "Hungary" = "HUN", "Belgium" = "BEL", "Sweden" = "SWE", "Finland" = "FIN","Malaysia" = "MYS","Philippines" = "PHL", "Turkey" = "TUR", "Austria" = "AUT", "South Africa" = "ZAF","Thailand" = "THA", "Denmark" = "DNK",  "Mexico" = "MEX", "United Arab Emirates" = "ARE","Brazil" = "BRA", "Hong Kong" = "HKG", "Ukraine" = "UKR", "Kuwait" = "KWT",  "Cyprus" = "CYP", "Monaco" = "MCO", "Iceland" = "ISL", "Kenya" = "KEN", "Singapore" = "SGP", "Iran" = "IRN", "Lithuania" = "LTU", "Liechtenstein" = "LIE", "Morocco" = "MAR", "Jordan" = "JOR", "Uruguay" = "URY", .default= NA_character_)

    ## debugging unclear/missing countries
    ## filter(df, is.na(countrycode)) %>% select(country, name, museum_status)

    df$year_opened_int <- as.integer(lapply(df$year_opened_str, function(x)(substring(x, 0,4))))
    ## as.data.frame(filter(df, is.na(year_opened_int))[,c("year_opened_str", "year_opened_int")])

    ## make ugly conditional filtering
    if (only_pms) {
        df_only_pms <- filter(df, museum_status %in% c("private museum", "closed"))
    } else {
        df_only_pms <- filter(df, museum_status %in% c("private museum", "closed", "no longer a private museum"))
    }


    return(df_only_pms)
}



create_excel_df_diagnose <- function(df, verbose = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' check status of base df
    ## debugging unclear/missing countries: atm 12 cases

    country_test <- filter(df, is.na(countrycode)) %>% select(ID, name, countrycode, museum_status)


    print(paste0("non-perfect countries: ", nrow(country_test)))
    if (verbose) {
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

gen_closing_year_imputed <- function(dfx) {
    #' impute closing year for museums that closed (based on opening year)
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    
    dt_pred_closing <- adt(dfx)[!is.na(year_opened_int) & !is.na(year_closed)] %>%
        .[, years_open := year_closed - year_opened_int]


    lm_pred <- lm(year_closed ~ year_opened_int, dt_pred_closing)

    ## lm(years_open ~ year_opened_int, dt_pred_closing) %>% summary()

    df_imptd <- adt(dfx) %>%
        .[is.na(year_closed) & !is.na(year_opened_int) & museum_status != "private museum",
          `:=`(
              imputed = T,
              year_closed = as.integer(predict(lm_pred, .SD)))] %>% atb()

    if ((adt(df_imptd)[, max(year_closed, na.rm =T)] > 2022)) { stop("year_closed too big")}

    ## check that now all PMs that are not currently open have closing year
    ## adt(dfx)[, .N, is.na(year_closed)]
    ## adt(df_imptd)[, .N, by = .(year_closed_there = !is.na(year_closed), museum_status)]

    ## rbind(
    ##     x[museum_status != "private museum", .N, year_closed][,source := "imptd"],
    ##     adt(filter(df_imptd, !is.na(year_closed)))[, .N, year_closed][,source := "orig"]) %>%
    ##     ggplot(aes(x=year_closed, y = N, fill = source)) +
    ##     geom_col(position = "dodge2")

    return(select(df_imptd, -imputed))
    
}




aggregate_openings <- function(df_excl, yeet_early_closed = F, impute_closing_year) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' aggregate excel df into country-year openings

    ## country,
    df_excl2 <- select(df_excl, name,  countrycode, year_opened_int, year_closed,
                       museum_status = `Museum_status`) %>%
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
    
    if (impute_closing_year) {

        df_excl3 <- gen_closing_year_imputed(df_excl2)
    } else {
        df_excl3 <- df_excl2
    }

    
    ## generate count of closed 
    df_clsd <- select(df_excl3, name, iso3c = countrycode, year = year_closed) %>%
        na.omit() %>%
        group_by(iso3c, year) %>% 
        summarize(nbr_closed = n())
    
    
    df_excl3$ctr <- 1
    df_open_cnt <- as_tibble(aggregate(ctr ~ countrycode + year_opened_int, df_excl3, FUN = sum))
    names(df_open_cnt) <- c('iso3c', 'year', 'nbr_opened')

    df_open_names <- df_excl3 %>% group_by(countrycode, year_opened_int) %>% summarise(name = list(name))
    names(df_open_names) <- c("iso3c", "year", "name")

    ## df_open <- as_tibble(merge(df_open_cnt, df_open_names))
    
    ## df_open <- Reduce(\(x,y) full_join(x,y), list(df_open_cnt, df_open_names, df_clsd))

    df_open <- Reduce(full_join, list(df_open_cnt, df_open_names, df_clsd))

    ## test suite: test in dt: make sure that previous version and dt produce same results
    dt_open <- adt(df_excl3)[, .(name, iso3c = countrycode, year_opened_int, year_closed)] %>%
        melt(id.vars = c("name", "iso3c"), value.name = "year") %>% 
        ## .[!complete.cases(.), .N, variable] # 9 PMs have no opening year, 491 no closing year
        na.omit() %>% 
        .[, .(vlu_proc = .N), by = c("iso3c", "variable", "year")] %>%
        dcast.data.table(iso3c + year ~ variable, value.var = "vlu_proc", drop = c(T,F))
    
    opng_cprn <- rbind(
        adt(df_open) %>% .[, .(iso3c, year, nbr_opened, nbr_closed)] %>% .[, source := "old"] %>%
        melt(id.vars = c("iso3c", "year", "source")),
        dt_open[, .(iso3c,year,nbr_opened=year_opened_int,nbr_closed=year_closed)] %>% .[,source:="dt"] %>%
        melt(id.vars = c("iso3c", "year", "source"))) %>%
        dcast.data.table(iso3c + year + variable ~ source)

    ## test that no unallowed NAs sneak in
    if (opng_cprn[, any(is.na(iso3c)) | any(is.na(year)) | any(is.na(variable))]) {stop("unallowed NAs")}

    ## need some stupid NA recoding to have proper comparison (otherwise NA comparisons don't work
    opng_cprn[is.na(opng_cprn)] <- 9999

    nbr_row_diff <- nrow(opng_cprn[dt != old])

    if (nbr_row_diff != 0) {stop("there is an error in `aggregate_openings`")}
    
    
    return(df_open)
}



## need some df_wb for the complete country-year structure

gd_dens_neib <- function(df_wb, df_open) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    #' generate data.table of pmdensity in neighboring countries, per capita
    #' neighboring countries are: for countries with land neighbors: their land connections
    #' for island countries: the countries in the same UN subregion
    
    ## join df_wb and df_open to get density and population
    dt_denspop <- adt(df_open)[, .(iso3c, year, nbr_opened, nbr_closed)][adt(df_wb), on = .(iso3c, year)] %>%
        .[, `:=`(nbr_opened = replace_NA(nbr_opened), nbr_closed = replace_NA(nbr_closed))] %>% 
        .[, pm_dens := cumsum(nbr_opened) - cumsum(nbr_closed), iso3c] %>%
        .[, .(iso3c, year, pm_dens, pop = SP.POP.TOTL)]

    ## dt_denspop[iso3c == "ITA"] %>% print(n=50)
    

    ## dt_boundaries <- fread("/home/johannes/Dropbox/phd/papers/org_pop/data/boundaries/geodatasource_land_boundaries.csv")

    dt_crybndrs <- gd_crybndrs()

    ## set which conditions countries fall in: have land neighbors or not
    dt_bndry_cases <- dt_crybndrs[dt_denspop[, .(iso3c = unique(iso3c))], on = "iso3c"] %>%
        .[, bndrycase := fifelse(all(is.na(iso3c_border)), "island", "land_neighbors"), iso3c]

    ## prepare subregions
    dt_subreg <- dt_denspop[, .(iso3c = unique(iso3c))] %>%
        .[, subregion := countrycode(iso3c, "iso3c", "un.regionsub.name")]

    ## get sub-region members for Island countries
    dt_island_subreg_members <- dt_subreg %>%
        .[., on = "subregion", allow.cartesian = T] %>% # self-join on subregion
        .[dt_bndry_cases[bndrycase == "island"], on = "iso3c"] %>% # filter down to islands
        .[, .(iso3c, iso3c_border = i.iso3c, bndrycase)] %>% # renaming
        .[iso3c != iso3c_border] # filter out self-joins
    
    ## check that not only islands are in sub-region set
    if (funique(dt_bndry_cases[, .(iso3c, bndrycase)]) %>%
        .[dt_island_subreg_members[, .(iso3cxx = iso3c, iso3c_border)] , on = .(iso3c = iso3c_border)] %>%
        .[, all(bndrycase == "island")]) {stop("boundary cases not handled")}

    ## get single dt with country boundaries of land neighbors, and for island those in region
    dt_crybndrs_all <- rbind(dt_bndry_cases[bndrycase == "land_neighbors", .(iso3c, iso3c_border, bndrycase)],
                             dt_island_subreg_members)
        
    ## dt_crybndrs_all[, .N, .(iso3c, bndrycase)] %>% .[, .(mean_N = mean(N)), .(bndrycase)]
    ## islands have more neighbors, whatever

    ## dt_denspop[dt_crybndrs, on = "iso3c", allow.cartesian = T][, .(iso3c, year, SP.POP.TOTL, iso3c_border)]

    ## can i get the neighboring populatiosn with a single join?
    ## nope not really well with different grouping mechanisms
    ## just assume no changing boundaries -> join subregion countries to islands

    ## first expand to entity-neighbor-years, then join population info to the country-neighbors-year entries
    ## generates NA because the country-neighbors pairs are generated for the entire period,
    # but population data for some countries is available only later
    dt_neighinfo <- dt_crybndrs_all[dt_denspop[, .(iso3c, year)], on = "iso3c", allow.cartesian = T] %>% 
        join(dt_denspop[, .(iso3c, year, pop_border = pop, pm_dens_border = pm_dens)],
             on = c("iso3c_border" = "iso3c", "year")) %>%
        .[, pm_dens_border := replace_NA(pm_dens_border, 0)]

    ## dt_neighinfo[is.na(pop_border)]

    if (any(is.na(dt_neighinfo))) {warning("fix NAs you lazy bone, won't do it tho, na.omit is fine here")}

    ## dt_neighinfo[iso3c == "ABW" & year == 1990] %>% print(n=300)

    ## calculate in neighboring countries population, density, density per capita
    dt_neighdens <- dt_neighinfo %>% na.omit %>% 
        .[, .(neigh_popm = sum(pop_border)/1e6, neigh_pmdens = sum(pm_dens_border)), .(iso3c, year)] %>%
        .[, pmdens_neigh := neigh_pmdens/neigh_popm] # name in style with other variables

    ## dt_neighdens %>% copy() %>% .[, reg6 := rcd_iso3c_reg6(iso3c)] %>% na.omit %>% atb %>% 
    ##     viz_lines(y="neigh_pmdens_prop", facets = "reg6", max_lines = 8)

    ## adt(df_wb)[!dt_neighdens, on = .(iso3c, year)] %>% print(n=300)

    return(dt_neighdens)
        
        
        


}

## gd_dens_neib(df_wb, df_open) %>% .[iso3c == "CHE"] %>% print(n=200)


create_anls_df <- function(df_wb, df_open) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' merge the aggregated excel df to the WB df (as complete country-year structure)

    
    df_anls <- as_tibble(merge(df_wb, df_open,
                               by=c("iso3c", "year"),
                               all.x = TRUE))

    
    ## global density calculations
    df_glbl <- adt(df_open) %>%
        ## na.rm = T for yeeting CYs with some closed but no opened
        .[, .(nbr_opened_global = sum(nbr_opened, na.rm = T),
              nbr_closed_global = sum(nbr_closed, na.rm = T)), year] %>%
        .[CJ(year = min(year):max(year)), on = "year"] %>% 
        setnafill(fill = 0) %>% # fill up year gaps with 0 (probably not needed but still)
        .[order(year)] %>% 
        .[, `:=`(nbr_opened_cum_global = cumsum(nbr_opened_global),
                 nbr_closed_cum_global = cumsum(nbr_closed_global))] %>%
        .[, pm_density_global := nbr_opened_cum_global - nbr_closed_cum_global] %>%
        .[, .(year, pm_density_global, pm_density_global_sqrd = pm_density_global^2, nbr_closed_cum_global)]

    ## country level density calculations
    df_cry <- adt(df_open)[, .(iso3c, year, nbr_opened = nbr_opened, nbr_closed)] %>%
        .[CJ(year = min(year):max(year), iso3c = unique(iso3c)), on = .(year, iso3c)] %>% 
        setnafill(fill = 0, cols = c("nbr_opened", "nbr_closed")) %>%
        .[order(iso3c, year)] %>% 
        .[, `:=`(nbr_opened_cum = cumsum(nbr_opened), nbr_closed_cum = cumsum(nbr_closed)), iso3c] %>%
        .[, pm_density := nbr_opened_cum - nbr_closed_cum] %>%
        .[, .(iso3c, year, nbr_opened, pm_density, pm_density_sqrd = pm_density^2)]
            
    
    
    df_anls3 <- left_join(df_wb, atb(df_cry), by = c("iso3c", "year")) %>%
        left_join(atb(df_glbl), by = "year") %>% adt() %>%
        ## fill density variables here already
        setnafill(fill = 0, cols = c("nbr_opened", "pm_density", "pm_density_sqrd")) %>%
        atb() 
        ## .[, lapply(.SD, \(x) sum(is.na(x)))] %>% melt()
 
    dt_dens_neib <- gd_dens_neib(df_wb, df_open)[, .(iso3c, year, pmdens_neigh)]

    df_anls4 <- join(df_anls3, dt_dens_neib, on = c("iso3c", "year"), how = "left")
    


    ## df_anls2 <- df_anls %>% mutate(
    ##                 region = countrycode(iso3c, "iso3c", "un.region.name"),
    ##                 nbr_opened = ifelse(is.na(nbr_opened), 0, nbr_opened),
    ##                 nbr_closed = ifelse(is.na(nbr_closed), 0, nbr_closed),
    ##                 nbr_opened_cum = ave(nbr_opened, iso3c, FUN= cumsum),
    ##                 nbr_closed_cum = ave(nbr_closed, iso3c, FUN= cumsum),
    ##                 pm_density = nbr_opened_cum - nbr_closed_cum,
    ##                 pm_density_sqrd = pm_density^2) %>%
    ##     group_by(year) %>%
    ##     mutate(nbr_opened_cum_global = sum(nbr_opened_cum),
    ##            nbr_closed_cum_global = sum(nbr_closed_cum),
    ##            nbr_opened_global = sum(nbr_opened),
    ##            pm_density_global = nbr_opened_cum_global - nbr_closed_cum_global,
    ##            pm_density_global_sqrd = pm_density_global^2)
    ##            ## prop_closed = nbr_closed_cum/nbr_opened_cum)


    ## ## comparison of country between old and new
    ## ## old method garbage
    ## rbind(df_cry[, .(iso3c, year, nbr_opened, pm_density, source = "new")],
    ##       adt(df_anls2)[, .(iso3c, year, nbr_opened, pm_density, source = "old")]) %>%
    ##     melt(id.vars = c("year","iso3c", "source")) %>%
    ##     dcast.data.table(variable + year + iso3c ~ source) %>%
    ##     .[complete.cases(.)] %>%
    ##     .[, diff := old - new] %>%
    ##     ## .[, .N, .(diff, variable)] %>% print(n=1000)
    ##     .[diff != 0] %>% .[order(diff)] %>% print(n=700)
            

    ## ## comparison of global between old and new
    ## ## old method garbage
    ## rbind(
    ##     unique(adt(df_anls2)[, .(year, pm_density_global, nbr_closed_cum_global, source = "old")]),
    ##     df_glbl[year >= 1985, .(year, pm_density_global, nbr_closed_cum_global, source = "new")]) %>%
    ##     melt(id.vars = c("year", "source")) %>%
    ##     dcast.data.table(variable + year ~ source) %>% print(n=200)

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

    return(df_anls4)
}

## create_anls_df(df_wb, df_open)




## df_excl <- create_excel_df(PMDB_FILE)
## df_open <- aggregate_openings(df_excl)
## df_wb <- get_WB_data("NY.GDP.PCAP.CD")
## df_anls <- create_anls_df(df_wb, df_open)
