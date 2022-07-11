

## * MOW/IDA

## first do in python: mow.py

## ** csv



mow_tag_year_cnt <- function(mow_df, year, setx, sets) {
    #' get the counts of mow df until year given some set name
    #' don't pass list of tags directly because it's a mess to then get the label 

    tags <- sets[setx]

    print(paste0(year, setx))

    mow_df_fltrd <- filter(mow_df, founding_date1 <= year, tag %in% tags[[1]])
    mow_df_fltrd2 <- unique(mow_df_fltrd[,c('idx', 'name', 'iso3c')])
    mow_df_fltrd2$cnt <- 1
                                

    mow_agg <- as_tibble(aggregate(cnt ~ iso3c, mow_df_fltrd2, sum))

    col_name <- paste0('cnt_', setx, "_", year)
    names(mow_agg)[2] <- col_name

    mow_agg[paste0(col_name, "_squared")] <- mow_agg[col_name]^2

    return(mow_agg)
    
}

mow_cntns_cvrg <- function(mow_df, setx, sets) {
    #' construct the continuous measures (just to test them)

    tags <- sets[setx]

    mow_df_fltrd <- filter(mow_df, founding_date1 >= STARTING_YEAR & tag %in% tags[[1]])
    mow_df_fltrd2 <- unique(mow_df_fltrd[,c("idx", "name", "iso3c", "founding_date1")])
    mow_df_fltrd2$cnt <- 1

    mow_agg <- as_tibble(aggregate(cnt ~ iso3c + founding_date1, mow_df_fltrd2, sum))
    
    names(mow_agg) <- c("iso3c", "year", paste0("cnt_", setx))
    return(mow_agg)

}



get_mow_dfs <- function() {
    #' provide the MOW data
    
    df_mow <- as_tibble(read.csv(paste0(MOW_DIR, "mow.csv")))
    df_mow$iso3c <- countrycode(df_mow$country, "country.name", "iso3c")

    mow_type <- as_tibble(read.csv(paste0(MOW_DIR, "type.csv")))
    mow_clsfcn <- as_tibble(read.csv(paste0(MOW_DIR, "classification.csv")))
    names(mow_type) <- c("idx", "tag")
    names(mow_clsfcn) <- c("idx", "tag")

    mow_tag <- rbind(mow_type, mow_clsfcn)

    mow_tag <- as_tibble(merge(filter(df_mow[,c("idx", "name", "founding_date1", "iso3c")], founding_date1 > 1900), mow_tag, by=c("idx"), all=T))

    
    set_contemp <- c("Art, Modern and Contemporary")
    set_art <- c("Art Museum", "Art")
    ## set_all <-list("asdf", "jjj")
    set_all <- setdiff(unique(mow_tag$tag), c(set_contemp, set_art))
    
    
    sets <- list("contemp"=set_contemp, "art"=set_art, "all"=set_all)

    years <- c(1985, 1990, 1995, 2000, 2005, 2010,2020)
    set_year_cbns <- as_tibble(expand.grid(set=names(sets), year=years))
    
    res_mow_agg <- apply(set_year_cbns, 1, function(x) mow_tag_year_cnt(mow_df=mow_tag, year=x['year'], setx=x['set'], sets=sets))
    ## mow_tag_year_cnt(mow_tag, year=2000, setx="art")

    mow_res_wide <- as_tibble(Reduce(function(x,y,...) merge(x,y,by=c('iso3c'), all=TRUE), res_mow_agg))
    mow_res_wide[is.na(mow_res_wide)] <- 0


    ## res_mow_cnts <- mow_cntns_cvrg(mow_tag, setx="art")
    res_mow_cnts <- lapply(names(sets), function(x) mow_cntns_cvrg(mow_df = mow_tag, setx = x, sets=sets))
    res_mow_cnts_cbn <- as_tibble(Reduce(function(x,y,...) merge(x,y,by=c('iso3c', 'year'), all=TRUE), res_mow_cnts))

 
    return(list(
        mow_crssctn=mow_res_wide,
        mow_cntns = res_mow_cnts_cbn))
}

