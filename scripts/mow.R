

## * MOW/IDA

## first do in python: mow.py

## ** csv



mow_tag_year_cnt <- function(mow_df, year, setx, sets) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}    
    #' get the counts of mow df until year given some set name
    #' don't pass list of tags directly because it's a mess to then get the label 

    tags <- sets[setx]

    print(paste0(year, setx))

    mow_df_fltrd <- filter(mow_df, founding_date1 <= year, tag %in% tags[[1]]) # [[1]] to to beyond name
    mow_df_fltrd2 <- unique(mow_df_fltrd[,c('idx', 'name', 'iso3c')])
    mow_df_fltrd2$cnt <- 1
    
    ## adt(mow_df_fltrd)[, n:=.N, by = c('idx', 'name', 'iso3c')][order(-n)]
    ## filter(mow_df, idx == "M65474")
    


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



proc_mow_dt <- function(df_mow2, sets, years) {
    #' split into subfunction the processing with data table 
        

    dt_tags <- data.table(setx = names(sets), sets)[, .(tag = unlist(sets)), setx]

    ## update join to set tags to set names (contemp, modern, all)
    dt_mow <- adt(df_mow2)[dt_tags, tag := setx, on = "tag"] %>% unique()
    
    ## create and combine different subsets of dt_mow depending on having opened up until year threshold
    year_splits_cbn <- map_dfr(years, ~dt_mow[founding_date1 <= .x][,founding_date1 := .x]) %>% na.omit()

    ## calculate counts/squared and cast into existing variable names
    dt_mow_proc <- year_splits_cbn %>% copy() %>%
        .[, .(cnt = .N, cnt_squared = as.integer(.N^2)), by = c("iso3c", "founding_date1", "tag")] %>%
        melt(id.vars = c("iso3c", "founding_date1", "tag")) %>% # melt into super long
        .[, vrbl_name := paste0("cnt_", tag, "_", founding_date1)] %>% # generate variable name
        .[variable == "cnt_squared", vrbl_name := paste0(vrbl_name, "_sqrd")] %>% # update "squared" vrbl name
        dcast.data.table(iso3c ~ vrbl_name, value.var = "value")

    ## fill up zeroes
    dt_mow_proc[is.na(dt_mow_proc)] <- 0

    return(atb(dt_mow_proc))
}


proc_mow_trad <- function(df_mow2, sets, years) {
    #' traditional approach to process threshold counts with functions/tibbles
    
    set_year_cbns <- as_tibble(expand.grid(set=names(sets), year=years))

    
    res_mow_agg <- apply(set_year_cbns, 1, \(x)
                         mow_tag_year_cnt(mow_df=df_mow2, year=x['year'], setx=x['set'], sets=sets))
    ## mow_tag_year_cnt(mow_tag, year=2000, setx="art")

    mow_res_wide <- as_tibble(Reduce(function(x,y,...) merge(x,y,by=c('iso3c'), all=TRUE), res_mow_agg))
    mow_res_wide[is.na(mow_res_wide)] <- 0

    return(mow_res_wide)
}


get_mow_dfs_old <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' provide the MOW data
    
    df_mow <- as_tibble(read.csv(paste0(MOW_DIR, "mow.csv")))
    df_mow$iso3c <- countrycode(df_mow$country, "country.name", "iso3c")

    mow_type <- as_tibble(read.csv(paste0(MOW_DIR, "type.csv")))
    mow_clsfcn <- as_tibble(read.csv(paste0(MOW_DIR, "classification.csv")))
    names(mow_type) <- c("idx", "tag")
    names(mow_clsfcn) <- c("idx", "tag")

    mow_tag <- rbind(mow_type, mow_clsfcn)

    mow_tag <- as_tibble(merge(df_mow[,c("idx", "name", "founding_date1", "iso3c")], mow_tag, by=c("idx"), all=T))

    ## filter(mow_tag, iso3c == "ISL", tag == "Art, Modern and Contemporary", founding_date1 <= 1990)

    
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


get_mow_dfs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' provide the MOW data
    
    df_mow <- as_tibble(read.csv(paste0(MOW_DIR, "mow.csv")))
    df_mow$iso3c <- countrycode(df_mow$country, "country.name", "iso3c")

    ## add the classification stuff
    mow_type <- as_tibble(read.csv(paste0(MOW_DIR, "type.csv")))
    mow_clsfcn <- as_tibble(read.csv(paste0(MOW_DIR, "classification.csv")))
    names(mow_type) <- c("idx", "tag")
    names(mow_clsfcn) <- c("idx", "tag")
    mow_tag <- rbind(mow_type, mow_clsfcn)

    ## assign tags
    mow_tag <- df_mow %>% select(idx, name, founding_date1, iso3c) %>% 
        inner_join(., mow_tag , by = "idx")

    ## yeet museums form MOW that are covered by PMDB
    mow_dupl <- read.csv(paste0(PROJECT_DIR, "data/git_files/mow_dupl.csv")) %>% atb()
    names(mow_dupl) <- c("muem_name", "dupl", "idx")
    ## do some awkward filtering/selecting because some museum ids have multiple names (amos rex, benesse)

    ## filter(mow_dupl, !is.na(idx)) %>% left_join(mow_tag) %>%
    ##     filter(iso3c == "USA", tag == "Art, Modern and Contemporary")
        


    df_mow2 <- filter(mow_dupl, dupl==1) %>% select(idx) %>% unique() %>% 
        anti_join(mow_tag, ., on = "idx") %>%
        unique()

        ## manual testing 
    ## merge(
    ##     adt(mow_tag)[founding_date1 <= 2020 & tag==set_contemp][, .(nfl = .N), iso3c],
    ##     adt(df_mow2)[founding_date1 <= 2020 & tag==set_contemp][, .(fl = .N), iso3c], on="iso3c") %>% 
    ##     .[, diff := nfl - fl]%>% .[diff != 0]
        
    ## full_join(
    ##     mow_tag_year_cnt(df_mow2, year = 2020, setx = "contemp", sets = sets),
    ##     adt(df_mow2)[founding_date1 <= 2020 & tag==set_contemp][, .(fl = .N), iso3c], on ="iso3c") %>%
    ##     mutate(diff = cnt_contemp_2020 - fl) %>%
    ##     inner_join(select(y$mow_crssctn, iso3c, y_cnt_contemp_2020 = cnt_contemp_2020)) %>% print(n=80)

    
    set_contemp <- c("Art, Modern and Contemporary")
    set_art <- c("Art Museum", "Art")
    set_all <- setdiff(unique(df_mow2$tag), c(set_contemp, set_art))
    sets <- list("contemp"=set_contemp, "art"=set_art, "all"=set_all)
    ## create tag df to update-join the tag column to set names
    years <- c(1985, 1990, 1995, 2000, 2005, 2010,2020)
    
    ## y <- proc_mow_dt(df_mow2, sets, years)
    ## x <- proc_mow_trad(df_mow2, sets, years)
    
    ## z <- rbind(mutate(x, source = "trad"),mutate(y, source="dt"))
    ## adt(z) %>% melt(id.vars = c("iso3c", "source")) %>%
    ##     dcast.data.table(iso3c + variable ~ source) %>%
    ##     .[dt != trad & !grepl("squared", variable)] %>%
    ##     .[, diff := dt - trad] %>% print(n=100)
        
    ## adt(df_mow2)
    
    mow_res_wide <- proc_mow_dt(df_mow2, sets, years)
    
    ## res_mow_cnts <- mow_cntns_cvrg(mow_tag, setx="art")
    res_mow_cnts <- lapply(names(sets), function(x) mow_cntns_cvrg(mow_df = df_mow2, setx = x, sets=sets))
    res_mow_cnts_cbn <- atb(Reduce(\(x,y,...) merge(x,y,by=c('iso3c', 'year'), all=TRUE), res_mow_cnts))

 
    return(list(
        mow_crssctn=mow_res_wide,
        mow_cntns = res_mow_cnts_cbn))
}


## x <- get_mow_dfs_old()
## y <- get_mow_dfs()



## full_join(
##     select(x$mow_crssctn, iso3c, cnt_old=cnt_contemp_1990),
##     select(y$mow_crssctn, iso3c, cnt_new=cnt_contemp_1990)) %>%
##     mutate(diff = cnt_old - cnt_new) %>% 
##     filter(diff != 0) %>% adf()
