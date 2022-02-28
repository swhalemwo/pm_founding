

## * MOW/IDA

## first do in python: mow.py

## ** csv



df_mow <- as_tibble(read.csv(paste0(MOW_DIR, "mow.csv")))

df_mow$iso3c <- countrycode(df_mow$country, "country.name", "iso3c")

mow_type <- as_tibble(read.csv(paste0(MOW_DIR, "type.csv")))
mow_clsfcn <- as_tibble(read.csv(paste0(MOW_DIR, "classification.csv")))

mow_fndgs <- as_tibble(merge(filter(df_mow[,c("idx", "name", "founding_date1", "iso3c")], founding_date1 > 1900), mow_type, by=c("idx")))

mow_fndgs$cnt <- 1

viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 8, grp = "type", extra = FALSE)





## check that viz_lines can also be used to aggregate dataframe
y <- viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 1, grp = "iso3c", extra = FALSE, fill_up = T)

y <- viz_lines(na.omit(mow_fndgs[,c("founding_date1", "cnt", "iso3c")]), x="founding_date1", y="cnt", time_level = "ra", duration = 1, grp = "iso3c", extra = FALSE, fill_up = T)


y2 <- fill_up2(na.omit(mow_fndgs[,c("founding_date1", "cnt", "iso3c")]), x="founding_date1", y="cnt", grp="iso3c")

y2 <- fill_up2(mow_fndgs, x="founding_date1", y="cnt", grp="iso3c")

head(filter(y, grp == "DEU"))

filter(mow_fndgs, is.na(iso3c))
       

viz_lines(filter(mow_fndgs, founding_date1 > 1985), x="founding_date1", y="cnt", time_level = "ra", duration = 3, grp = "iso3c", fill_up = T)

x <- as_tibble(aggregate(cnt ~ founding_date1 + iso3c, mow_fndgs, sum))
head(filter(x, iso3c == "DEU"),8)



mow_tag_year_cnt <- function(mow_df, year, setx, sets) {
    #' get the counts of mow df until year given some set name
    #' don't pass list of tags directly because it's a mess to then get the label 

    tags <- sets[setx]

    print(paste0(year, setx))

    mow_df_fltrd <- filter(mow_df, founding_date1 <= year, tag %in% tags[[1]])
    mow_df_fltrd2 <- unique(mow_df_fltrd[,c('idx', 'name', 'iso3c')])
    mow_df_fltrd2$cnt <- 1
                                

    mow_agg <- as_tibble(aggregate(cnt ~ iso3c, mow_df_fltrd2, sum))


    names(mow_agg)[2] <- paste0('cnt_', setx, "_", year)

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

    years <- c(1985, 2000,2020)
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

## ** more cleaning/completeness checks
## *** country


country_tbl <- table(df_mow$country)

country_tbl[rev(order(country_tbl))[1:10]]
## 63 museums have no country 
## -> looks good


## *** city
city_tbl <- table(df_mow$city)

city_tbl[rev(order(city_tbl))[1:10]]
## around 4k have no city 

## *** some manual check
as.data.frame(filter(mow_fndgs,
                     country == "Netherlands" & founding_date1 >= 1970 &
                     founding_date1 < 1980 & type == "Art Museum")[,c("name", "founding_date1")])


## *** some basic look at contemporary art museums

## around 2k museums, 1250 have founding date

mow_cpaer <- as_tibble(merge(filter(mow_clsfcn, clsfcn == "Art, Modern and Contemporary"), df_mow))
mow_cpaer$cnt <- 1

p1 <- viz_lines(filter(mow_cpaer, founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, return="plot")
p2 <- viz_lines(filter(mow_cpaer, founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, extra = "cum_rate", return="plot")
p3 <- viz_lines(filter(mow_cpaer[mow_cpaer$country %!in% c("United States of America", "Germany"),], founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, max_lines = 8, extra = "cum_rate", return="plot")
p4 <- viz_lines(filter(mow_cpaer[mow_cpaer$country %!in% c("United States of America", "Germany"),], founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, max_lines = 8, return = "plot")

pdf(paste0(FIG_DIR, "mow_contemporary.pdf"), width = 14, height = 8)
grid.arrange(p1,p4,p2,p3, ncol=2, nrow=2)
dev.off()


## hmm canada so early huh
## italy has actually a lot of founding going on until the end.. does it mean database is maybe less incomplete than I thought? 

## *** keyword co-occurence
kwds_w_cpaer <- as_tibble(merge(mow_clsfcn, mow_cpaer[,c("idx")]))
tbl_kwds <- table(kwds_w_cpaer$clsfcn)
as.data.frame(tbl_kwds[order(tbl_kwds)])

## Decorative and Applied Arts   23
##                Art, Russian   27
##                 Photography   28
##                   Sculpture   38
## History, Local and Regional   45
##                         Art   62
## Art, Modern and Contemporary 2035





