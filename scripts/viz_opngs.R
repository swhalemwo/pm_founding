## * visualization


year_selector <- function(x)(
    # convert cuts back to years
    substring(x, 2,5))

set_geo_level <- function(df, geo_level) {
    #' set geo_level to either country or geo

    if (geo_level == "region") {
    
        df$geo_level <- countrycode(df$iso3c, "iso3c", "region")

    } else {
        df$geo_level <- df$iso3c
    }
    return(df)
}


set_time_level <- function(df, time_level, duration) {
    #' setting time level: either cut or year (for rolling mean) calculation

    if (time_level == "cut") {
        ## just population calculation: could go into separate function, if rates are requested
        
        df$cut <- cut(df$year, seq(min(df$year), max(df$year)+5, by = duration))
        df$time_level <- as.numeric(sapply(as.character(df$cut), year_selector))


    } else {
        ##  maybe I have to put in function application here, not sure if I can put it properly away
        ## try first tho
        df$time_level <- df$year
    }
    return(df)
}

prep_pop <- function(df_plt) {
    #' prepare the population variable: mean by time period, then sum by geo level (if it is region)
    
    ## founding rates
    ## first country mean per cut
    ## then region sum
    ## always need this one: first aggregate by country + time + geo with mean 
    df_pop_cry_mean <- as_tibble(aggregate(SP.POP.TOTL ~ iso3c + time_level + geo_level, df_plt, mean))

    ## df_viz_pop_agg$region <- countrycode(df_viz_pop_agg$iso3c, "iso3c", "region")

    ## countrycode(unique(filter(df_viz_pop1, region == "South Asia")$iso3c), "iso3c", "country.name")
    df_pop_reg_sum <- as_tibble(aggregate(SP.POP.TOTL ~ geo_level + time_level, df_pop_cry_mean, sum))
    ## ggplot(df_pop_reg_sum, aes(x=time_level, y=SP.POP.TOTL, group=geo_level, color=geo_level)) +
    ##     geom_line()

    return(df_pop_reg_sum)
    }


agg_opnd_cnts <- function(df_plt, time_level, duration) {
    #' aggregate the opening counts

    ## always sum by geo_level + time_level, is enough if cuts are provided as time_level
    df_plt_opnd <- as_tibble(aggregate(nbr_opened ~ geo_level + time_level, df_plt, sum))

    ## if time_level is ra, apply custom rollmean function (time_level is year in that case)
    if (time_level != "cut") {

        df_plt_opnd <- df_plt_opnd %>%
            group_by(geo_level) %>%
            mutate(nbr_opened = rollmean_custom(nbr_opened, win_len = duration))
    }
    return(df_plt_opnd)
}

process_extra <- function(df_plt, df_pop_reg_sum, extra) {
    #' process the additional things:
    #' pop rates: opening per 100 million
    #' cumulative count
    #' cumulative rate
    
    if (extra == "pop_rates") {
        
        
        df_plt <- as_tibble(merge(df_pop_reg_sum, df_plt))
        df_plt$nbr_opened <- df_plt$nbr_opened/(df_plt$SP.POP.TOTL/1e+8)
    }
    ## maybe there is some question in which population rates and cumulative stuff isn't mutually exclusive,
    ## but imo not atm 


    if (extra == "cum_count") {
        df_plt$nbr_opened <- ave(df_plt$nbr_opened, df_plt$geo_level, FUN = cumsum)
    }
    if (extra == "cum_rate") {
        df_plt$nbr_opened_cum <- ave(df_plt$nbr_opened, df_plt$geo_level, FUN = cumsum)
        
        df_plt <- df_plt %>%
            group_by(geo_level) %>%
            mutate(nbr_max = max(nbr_opened_cum))
        
        df_plt$nbr_opened <- df_plt$nbr_opened_cum/df_plt$nbr_max
    }
    return (df_plt)
}


viz_opngs <- function(df_plt, time_level, duration, geo_level, extra=FALSE, max_lines=12, return ="df") {
    #' visualize the openings of private museums
    #' 
    #' time_level: cut or rolling mean
    #' duration: length of cut/rolling mean
    #' geo_level: region or country
    #' extra: one of "pop_rates" (population rates), "cum" (cumulative counts) and "cum_rate" (cumulative rate)
    #' max_lines: if country
    #' return: whether to return dataframe (before filtering lines) or the plot 


    ## may at some point be generalized into plotting any time series

    df_plt <- set_geo_level(df_plt, geo_level)
    
    df_plt <- set_time_level(df_plt, time_level, duration)
    df_pop_reg_sum <- prep_pop(df_plt)    

    df_plt <- agg_opnd_cnts(df_plt, time_level, duration)
    
    df_plt$nbr_opened_bu <- df_plt$nbr_opened


    df_plt <- process_extra(df_plt, df_pop_reg_sum, extra)

    print(c(geo_level, time_level, duration))
    ## filter(df_plt2, nbr_opened > 400)

    df_return <- df_plt
    
    ## limit down to max_lines
    ## if exists("max_lines") {
    ## can i use sum, even for rates? guess so...
    
    geos_cnt <- aggregate(nbr_opened_bu ~ geo_level, df_plt, sum)
    max_geos <- geos_cnt[rev(order(geos_cnt$nbr_opened_bu))[1:min(max_lines, nrow(geos_cnt))],"geo_level"]
    
    
    df_plt <- filter(df_plt, geo_level %in% max_geos)
    ## }
    
    
    plt <- ggplot(df_plt, aes(x=time_level, y=nbr_opened, color = geo_level)) +
        scale_color_brewer(palette = "Paired") + 
        geom_line(size=1.5)
    print(plt)

    ## allow to return either df or plot 
    if  (return=="plot") {
        return(plt)
    } else {
        return(df_return)
    }
}


## x <- viz_opngs(df_anls, time_level = "rolling_mean", duration = 5, geo_level = "country", extra = "pop_rates", max_lines = 8, return="plot")

## plots <- lapply(seq(1,10), function(x) viz_opngs(df_anls, time_level = "cut", duration = x, geo_level = "country", extra = FALSE, max_lines = 5, return = "plot"))


## pdf(paste0(FIG_DIR, "opngs_cut.pdf"), height = 15, width = 20)
## do.call(grid.arrange, plots)
## dev.off()



