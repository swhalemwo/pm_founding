## ** duplicate detection


dupl_checker <- function(name, countrycode, opening_year = NA){
    #' manual checker
    #' filter down the mow df to country and year+- year_diff
    #' don't use opening year by default

    print(name)

        
    year_diff <- 1
    
    if (name %!in% mow_dupls$name) {

        ## print(year_seq)
        countrycode <- countrycode[[1]]
        print(countrycode)
        
        if (is.na(opening_year)) {
            opening_year <- as.numeric(opening_year)
            year_seq <- seq(opening_year-year_diff,opening_year+year_diff)
            
            ## table(filter(mow_art, iso3c == "USA")$founding_date1)

            mow_art_fltrd <- filter(mow_art, iso3c == countrycode &
                                             (founding_date1 %in% year_seq |
                                              founding_date2 %in% year_seq | 
                                              founding_date3 %in% year_seq |
                                              founding_date4 %in% year_seq ))
        } else {
            mow_art_fltrd <- filter(mow_art, iso3c == countrycode)
        }
        
        print(nrow(mow_art_fltrd))
        print(as.data.frame(mow_art_fltrd[,c("idx","name", "name_eng")]))

        print(name)

        duplicate <- readline(prompt = "duplicate?: ")
        ## duplicate <- TRUE

        ## write to file, append
        if (duplicate == "j") {
            idx <- readline(prompt = "id: ")
            write.table(t(c(name, 1, idx)), file=MOW_DUPL_FILE, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
        } else {
            write.table(t(c(name, 0, NA)), file=MOW_DUPL_FILE, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
        }
    }
    
}


dupl_checker("Rubin Museum of Art", "USA", 2004)
dupl_checker("Nerman Museum of Contemporary Art", "USA", 2007)
dupl_checker("Rivet", "USA", 2007)

mow_art <- as_tibble(merge(filter(df_mow[,c("idx", "name", "name_eng", "founding_date1", "founding_date2", "founding_date3", "founding_date4", "iso3c")], founding_date1 > 1900),
                           filter(mow_type, type == "Art Museum"),
                                  by=c("idx")))



MOW_DUPL_FILE <- paste0(PROJECT_DIR, "data/git_files/", "mow_dupl.csv")
mow_dupls <- read.csv(MOW_DUPL_FILE, header = FALSE)
names(mow_dupls) <- c("name", "dupl", "idx")


## check those that have opening date 
apply(na.omit(df_excl[,c("name", "countrycode", "year_opened_int")]), 1,
      function(x) {dupl_checker(x['name'], x['countrycode'], x['year_opened_int'])})

## check those without opening date
apply(df_excl[,c("name", "countrycode", "year_opened_int")], 1,
      function(x) {dupl_checker(x['name'], x['countrycode'])})


## analyzing the coverage of PMs in MOW
mow_dupl_anls <- as_tibble(merge(df_excl[,c("name", "year_opened_int","countrycode")], mow_dupls))
dupl_years <- as_tibble(aggregate(dupl ~ year_opened_int, mow_dupl_anls, sum))
## setting arbitrary group
dupl_years$s <- "s"
viz_lines(filter(dupl_years, year_opened_int > 1950), x="year_opened_int", y="dupl", time_level = "ra", duration=5, grp = "s", extra = FALSE)
 ## 2002 peak, but also some in 80s/90s

dupl_crys <- aggregate(dupl ~ countrycode, mow_dupl_anls, sum)
dupl_crys[order(dupl_crys$dupl),]
## Germany, USA, Switzerland covered most

    
