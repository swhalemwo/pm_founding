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

        df$time_level <- df$year
    }
    return(df)
}

set_time_level_gnrl <- function(df, x, time_level, duration) {
    #' setting time level: either cut or year (for rolling mean) calculation

    if (time_level == "cut") {
        ## just population calculation: could go into separate function, if rates are requested
        
        df$cut <- cut(df$x, seq(min(df$x), max(df$x)+5, by = duration))
        df$x <- as.numeric(sapply(as.character(df$cut), year_selector))
    }

    return(df)
}

prep_pop <- function(df_plt) {
    #' prepare the population variable: mean by time period, then sum by geo level (only does something if geo level is region)
    
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

prep_div <- function(df_plt, div, grp) {
    #' prepare the population variable: mean by time period, then sum by grp level 
    
    ## founding rates
    ## first country mean per cut
    ## then region sum
    ## always need this one: first aggregate by country + time + geo with mean 
    ## df_pop_cry_mean <- as_tibble(aggregate(SP.POP.TOTL ~ iso3c + time_level + geo_level, df_plt, mean))

    
    df_div_grp_mean <- as_tibble(aggregate(div ~ grp + x, df_plt, mean))

    ## df_viz_pop_agg$region <- countrycode(df_viz_pop_agg$iso3c, "iso3c", "region")

    ## countrycode(unique(filter(df_viz_pop1, region == "South Asia")$iso3c), "iso3c", "country.name")
    df_div_grp_sum <- as_tibble(aggregate(div ~ grp + x , df_div_grp_mean, sum))
    ## ggplot(df_pop_reg_sum, aes(x=time_level, y=SP.POP.TOTL, group=geo_level, color=geo_level)) +
    ##     geom_line()

    return(df_div_grp_sum)
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

agg_y <- function(df_plt, y, grp, time_level, duration) {
    #' aggregate the opening counts
    
    ## always sum by geo_level + time_level, is enough if cuts are provided as time_level
    ## df_plt$y <- df_plt[[y]]
    ## df_plt$grp <- df_plt[[grp]]

    ## grps1 <- names(which(table(df_plt$grp)==1))
    ## df_plt <- filter(df_plt, grp %!in% grps1)
    
    
    df_plt_opnd <- as_tibble(aggregate(y ~ grp + x, df_plt, sum))
    ## df_plt_opnd2 <- df_plt_opnd[order(df_plt_opnd$grp),]

    ## if time_level is ra, apply custom rollmean function (time_level is year in that case)
    if (time_level != "cut") {

        df_plt_opnd <- df_plt_opnd %>%
            group_by(grp) %>%
            mutate(y = rollmean_custom(y, win_len = duration))
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

process_extra_gnrl <- function(df_plt, df_div_grp_sum=FALSE, extra) {
    #' process the additional things:
    #' pop rates: opening per 100 million
    #' cumulative count
    #' cumulative rate
    

    if (extra == "rates") {
        
        df_plt <- as_tibble(merge(df_div_grp_sum, df_plt))
        df_plt$y <- df_plt$y/(df_plt$div)
    }
    ## maybe there is some question in which population rates and cumulative stuff isn't mutually exclusive,
    ## but imo not atm 


    if (extra == "cum_count") {
        df_plt$y <- ave(df_plt$y, df_plt$grp, FUN = cumsum)
    }
    if (extra == "cum_rate") {
        df_plt$y_cum <- ave(df_plt$y, df_plt$grp, FUN = cumsum)
        
        df_plt <- df_plt %>%
            group_by(grp) %>%
            mutate(y_max = max(y_cum))
        
        df_plt$y <- df_plt$y_cum/df_plt$y_max
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



actually_plot <- function(df_plt, max_lines) {
    #' filters the df for plotting, and plots it


    grp_cnt <- aggregate(y_bu ~ grp, df_plt, sum)
    max_grps <- grp_cnt[rev(order(grp_cnt$y_bu))[1:min(max_lines, nrow(grp_cnt))],"grp"]

    df_plt <- filter(df_plt, grp %in% max_grps)

    plt <- ggplot(df_plt, aes(x=x, y=y, color = grp)) +
        scale_color_brewer(palette = "Paired") + 
        geom_line(size=1.5)
    print(plt)
    
    return(plt)
}

fill_up <- function(df, x, y, grp) {
    #' create an empty df, merge it to the data

    ## df$x <- df[[x]]
    ## df$y <- df[[y]]
    ## df$grp <- df[[grp]]

    structure_df <- expand(df, x=min(df$x):max(df$x), grp)

    ## aggregate (sum) original data
    og_df_agg <- aggregate(y ~ x + grp, df, sum)

    df_merge <- as_tibble(merge(structure_df, og_df_agg, all.x = TRUE))
    df_merge$y[which(is.na(df_merge$y))] <- 0

    return(df_merge)
}

viz_lines <- function(dfx, x, y, time_level, duration, grp, extra =FALSE, div=FALSE, max_lines=12, return ="df",fill_up = FALSE)  {
    #' general vizualization function
    #' dfx: overall dataframe, containing at least columns for
    #' x: the time series that ends up on the x axis
    #' y: the (count) variable ending up on y
    #' grp: group
    #' time_level: cut for cuts, else rolling means
    #' division column optional for rates
    #' extra: one of "pop_rates" (population rates), "cum" (cumulative counts) and "cum_rate" (cumulative rate)
    #' fill_up: whether to impute missing x-grp observations, aggregates with sum (for now)

        
    dfx <- set_time_level_gnrl(dfx, x, time_level = time_level, duration)

    dfx$y <- dfx[[y]]
    dfx$x <- dfx[[x]]
    dfx$grp <- dfx[[grp]]

    ## rates are now conditional
    if (extra == "rates") {
        
        dfx$div <- dfx[[div]]
        df_div_grp_sum <- prep_div(dfx, div, grp)
        
    } else {
        df_div_grp_sum <- FALSE
    }
    
    if (fill_up) {
        dfx <- fill_up(dfx, x, y, grp)
    }
    

    df_plt <- agg_y(dfx, y, grp, time_level, duration)

    df_plt$y_bu <- df_plt$y

    df_plt <- process_extra_gnrl(df_plt, df_div_grp_sum, extra)

    ## ggplot(df_plt[df_plt$grp %in% c("DEU", "USA", "CHE", "CHN"),], aes(x=time_level, y=y, group=grp, color=grp)) +
    ##     geom_line()

    ## ggplot(df_plt2, aes(x=time_level, y=y, group=grp, color=grp)) +
    ##     geom_line()
    
    df_return <- df_plt

    plt <- actually_plot(df_plt, max_lines)

    if  (return=="plot") {
        return(plt)
    } else {
        return(df_return)
    }

}


## viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 5, grp = "type", extra = "cum_count")

## viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 5, grp = "type", extra = FALSE)

## viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 5, grp = "type", extra = "cum_rate", max_lines = 8)

## viz_lines(df_anls, x="year", y="nbr_opened", time_level = "ra", duration = 5, grp = "country", div = "SP.POP.TOTL", max_lines = 12)

## viz_lines(df_anls, x="year", y="nbr_opened", time_level = "ra", duration = 5, grp = "country", extra = "rates", div = "SP.POP.TOTL", max_lines = 12)

## viz_lines(df_anls, x="year", y="nbr_opened", time_level = "ra", duration = 5, grp = "country", extra = "cum_rate", div = "SP.POP.TOTL", max_lines = 12)


## viz_lines(df_anls, x="year", y="nbr_opened", time_level = "ra", duration = 3, grp = "country", extra = "cum_rate", div = "SP.POP.TOTL", max_lines = 12)

## df_excl$cnt <- 1
## viz_lines(filter(df_excl, year_opened_int > 1984 & year_opened_int < 2021), x="year_opened_int", y="cnt", time_level = "ra", duration = 3, grp = "country")

## viz_lines(filter(df_excl, year_opened_int > 1984 & year_opened_int < 2021), x="year_opened_int", y="cnt", time_level = "ra", duration = 6, grp = "country", fill_up = T)


## df_anls$region <- countrycode(df_anls$iso3c, "iso3c", "region")
## df_anls$region <- countrycode(df_anls$iso3c, "iso3c", "un.regionsub.name")
## viz_lines(df_anls, x="year", y="nbr_opened", time_level = "ra", duration = 5, grp = "region", extra = "cum_rate", div = "SP.POP.TOTL", max_lines = 12)



