
create_base_df <- function() {

    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database.xlsx")
    ## df <- read_excel("/home/johannes/Dropbox/phd/papers/org_pop/data/Private museum database2.xlsx")

    ## 2: use more recent version
    df <- read_excel(paste0(PMDB_DIR, "Private museum database2.xlsx"))
    
    ## removing header stuff 
    nrows <- nrow(df)-1
    df <- df[2:nrows,]


    df$country <- df$"Country where museum is located"

    df$name <- df$Museumname
    df$year_opened_str <- df$"Opening year"
    df$year_closed <- df$"Closing year"
    df$museum_closed <- df$"Museum closed"

    ## tbl <- table(df$country)
    ## tbl2 <- tbl[rev(order(tbl))]
    ## tbl2

    df[which(df$country == "England"),]$country <- "United Kingdom"


    df$countrycode <- recode(df$country, "United Kingdom" = "GBR", "Spain" = "ESP", "United States" = "USA", "Switzerland" = "CHE" , "India" = "IND", "Greece" = "GRC", "Lebanon" = "LBN", "France" = "FRA", "Estonia" = "EST", "Azerbaijan" = "AZE", "Latvia" = "LVA", "Madagascar" = "MDG", "Indonesia" = "IDN", "Slovakia" = "SVK", "Romania" = "ROU","Argentina" = "ARG","South Korea" = "KOR", "Japan" = "JPN", "Benin" = "BEN", "Bangladesh" = "BGD", "Australia" = "AUS", "Norway" = "NOR", "New Zealand" = "NZL", "Poland" = "POL", "Nigeria" = "NGA", "Portugal" = "PRT", "Serbia" = "SRB","Czech Republic" = "CZE","Senegal" = "SEN", "Puerto Rico" = "PRI", "Taiwan" = "TWN", "Israel" = "ISR", "England" = "GBR", "China" = "CHN", "Germany" = "DEU", "Netherlands" = "NLD", "Italy" = "ITA", "Russia" = "RUS", "Canada" = "CAN", "Hungary" = "HUN", "Belgium" = "BEL", "Sweden" = "SWE", "Finland" = "FIN","Malaysia" = "MYS","Philippines" = "PHL", "Turkey" = "TUR", "Austria" = "AUT", "South Africa" = "ZAF","Thailand" = "THA", "Denmark" = "DNK",  "Mexico" = "MEX", "United Arab Emirates" = "ARE","Brazil" = "BRA", "Hong Kong" = "HKG", "Ukraine" = "UKR", "Kuwait" = "KWT",  "Cyprus" = "CYP", "Monaco" = "MCO", "Iceland" = "ISL", "Kenya" = "KEN", "Singapore" = "SGP", "Iran" = "IRN", "Lithuania" = "LTU", .default= NA_character_)

    ## debugging unclear/missing countries
    # filter(df, is.na(countrycode))[,c("country", "name")]

    df$year_opened_int <- as.integer(lapply(df$year_opened_str, function(x)(substring(x, 0,4))))
    ## as.data.frame(filter(df, is.na(year_opened_int))[,c("year_opened_str", "year_opened_int")])

    return(df)
}



create_base_df_diagnose <- function(df, verbose = 0){
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


## dfx <- create_base_df()
## create_base_df_diagnose(dfx)
    
