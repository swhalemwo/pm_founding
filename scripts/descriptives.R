library(psych)

plt_to_pdf <- function(plt, width, height, fig_name) {
    if (missing(fig_name)) { 
        fig_name <- deparse(substitute(plt))
    }

    
    pdf(paste0(FIG_DIR, fig_name, ".pdf"), width = width, height = height)
    plot(plt)
    dev.off()
}


## * coverage visualizations


library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

gen_cbn_plots <- function(cbn_dfs, df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plots that describe the variable coverage 
      
    source_df <- lapply(names(cbn_dfs)[1:3], \(x) cbn_dfs[[x]] %>% select(iso3c, year) %>%
                                             group_by(iso3c) %>% summarize(cnt = len(year), source = x) %>%
                                             mutate(source = factor(source, levels = names(cbn_dfs)))) %>%
        Reduce(\(x,y) rbind(x,y), .)

    world <- ne_countries(scale = "small", returnclass = "sf") %>% atb() %>%
        select(iso3c = iso_a3, geometry)

    ## stacked plot: country only in color of most valuable 

    source_priority_vec <- names(cbn_dfs)

    source_priority_df <- lapply(seq_along(source_priority_vec), \(pos)
                                 sort_by_priority(source_df, source_priority_vec, pos)) %>%
        list.rbind()

    world_w_data <- merge(world, source_priority_df, all.x = T) %>% atb()
    plt_world_stacked <- ggplot(world_w_data) +
        geom_sf(aes(geometry = geometry, fill = source)) +
        coord_sf(ylim = c(-55, 83))
    
    plt_world_stacked_res <- list(plt = plt_world_stacked, width = 10, height = 3, name = "world_plot_stacked")

    ## faceted world
    ## not sure that works: differences are hard to spot between all cbns that aren't cbn_all
    world_w_data_facet <- lapply(names(cbn_dfs)[1:3], \(x) merge(world,
                                      filter(source_df, source == x), all.x = T) %>%
                                      ## mutate(source = ifelse(is.na(source), paste0(x, "na") ,source)) %>%
                                      mutate(source_facet = x) %>% 
                                atb()) %>%
        list.rbind() %>%
        mutate(source_facet = factor(source_facet, levels = names(cbn_dfs))) %>%
        mutate(any_source = !is.na(source))
    
    ## filter(world_w_data_facet, source == "cbn_all")
    ## merge(world, filter(source_df, source == "cbn_all"), all.x = T) %>% atb()

    
    plt_world_facet <- ggplot(world_w_data_facet) +
        geom_sf(aes(geometry = geometry, fill= any_source, alpha = cnt)) +
        coord_sf(ylim = c(-55, 83)) + 
        facet_wrap(~source_facet, ncol = 1) +
        scale_fill_manual(values = c("grey", "#0492c2")) + # 1520a6 1338BE 52b2bf 
        theme(axis.ticks = element_blank(), axis.text = element_blank(),
              panel.background = element_blank())
        
    
    plt_world_facet_res <- list(plt = plt_world_facet, width = 8, height = 8, name = "world_plot_faceted")


    ## line/raster plot
    source_df_lines <- lapply(names(cbn_dfs), \(x,y) cbn_dfs[[x]] %>% select(iso3c, year) %>%
                                 mutate(source = factor(x, levels = names(cbn_dfs)))) %>%
        Reduce(\(x,y) rbind(x,y), .)

    
    source_df_lines_priority <- lapply(seq_along(source_priority_vec), \(pos)
                                       sort_by_priority(source_df_lines, source_priority_vec, pos)) %>%
        list.rbind()

    ## sort the countries by the inclusion in the most complete combination
    source_df_lines_priority <- source_df_lines_priority %>%
        mutate(iso3c = factor(iso3c, levels = source_priority_df$iso3c)) %>%
        merge(select(source_priority_df, iso3c, facet_source = source)) %>% atb()
    
    ## try with splitting the tile plot into smaller chunks, but stop due to limited benefit (also complexity)
    ## source_df_lines_priority$iso3c <- as.character(source_df_lines_priority$iso3c)
    ## max_block_size <- source_priority_df %>% group_by(source) %>% summarize(cnt = len(iso3c)) %>% pull(cnt) %>% max()
    ## source_df_lines_priority %>% pivot_wider(names_from = facet_source, values_from = source) %>%
    ##     pivot_longer(cols = source_priority_vec, names_to = "facet_source", values_to = "source") %>%
    ##     group_by(facet_source, year) %>% 
    ##     arrange(source, iso3c, year) %>% 
    ##     slice(1 : max_block_size)
        
    tile_plt <-  ggplot(source_df_lines_priority) +
        geom_tile(aes(x = year, y=iso3c, fill = source), color = "black")
            ## facet_wrap(~facet_source, nrow = 1, scales = "free")

    tile_plt_res <- list(plt = tile_plt, width = 10, height = 25, name = "country_tile_plot")


    line_plt <- source_df_lines %>% group_by(source, year) %>%
        summarize(cnt = len(iso3c)) %>%
        ggplot(aes(x=year, y=cnt, color = source)) +
        geom_line()
    line_plt_res <- list(plt = line_plt, width=6, height = 3, name = "cbn_cnt_line_plot")


    ## original variable plot, used for manually constructing the 
    rel_lngtd_vars <- c("tmitr_approx_linear20step",
                        "hnwi_nbr_30M",
                        "gptinc992j",
                        "ghweal992j",
                        "smorc_dollar_fxm",
                        "NY.GDP.PCAP.CDk",
                        "SP.POP.TOTLm")


    cpltns_vrbl_plot <- df_reg %>% select(c("iso3c", "year", rel_lngtd_vars)) %>%
        pivot_longer(cols=rel_lngtd_vars) %>%
        na.omit() %>%
        group_by(year, name) %>%
        summarize(nbr_crys = len(iso3c)) %>%
        ggplot(aes(x=year, y=nbr_crys, color = name, group=name)) +
        geom_line(size = 1.5) +
        scale_color_manual(values = colors_manual2, labels = rel_lngtd_vars) +
        scale_linetype_manual(labels = rel_lngtd_vars)

    
    cpltns_vrbl_plot_res <- list(plt = cpltns_vrbl_plot, width = 6, height = 4, name = "cpltns_vrbl_plot")

    return(list(plt_world_facet_res, plt_world_stacked_res, tile_plt_res, line_plt_res, cpltns_vrbl_plot_res))
}

cbn_plots <- gen_cbn_plots(cbn_dfs_rates_uscld, df_reg)
## lapply(cbn_plots, \(x) plt_to_pdf(x$plt, width = x$width, height=x$height, fig_name = paste0(x$name, "_v2")))

## * xtsum based descriptives

## finding out the weird internal workings of xtsum
## x <- xtsum(df_reg, nbr_opened, iso3c)

## xtsum_dt <- data.table(id = c(1,1,2,2,3,3), score1 = c(70,70,60,80,90,50), score2 = c(70,70,70,90,90,30))
## xtsum(xtsum_dt, score1, id)
## xtsum(xtsum_dt, score2, id)

## ## calculate within mean 
## xtsum_dt[, xbar := mean(score2), by = id]
## xtsum_dt2 <- xtsum_dt[, .(id, score2, xbar, s_within_p1 = score2 - xbar)]

## xtsum_dt2[, sd(s_within_p1 + mean(score2))]
## xtsum_dt2[, .(s_within_p1, s_within_p1^2)] %>% .[, sqrt(sum(V2)/5)]



proc_xtsum <- function(dfx, vrbl, id_vrbl) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' run xtsum on vrbl with id_vrbl, get the overall, between, within sds

    sdx <- xtsum_res <- xtsum(dfx, get(vrbl), get(id_vrbl))$sd

    return(list(
        name = vrbl,
        overall = sdx[[1]],
        between = sdx[[2]],
        within = sdx [[3]]))
}



render_xtsum_prop_plt <- function(df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate plot of proportion of within and between country-variation
    
    

    xtsum_res <- mclapply(c(vvs$all_rel_vars, "nbr_opened", "hdi"), \(x)
                          proc_xtsum(df_reg, x, "iso3c"),
                          mc.cores = 6) %>% rbindlist()

    xtsum_res2 <- xtsum_res %>% copy() %>%
        .[, `:=`(between_prop = between/overall, within_prop = within/overall,
                 within_between_ratio = within/between)]

    ## convert variable names to labels
    lbl_dt <- data.table(vrbl = names(vvs$vrbl_lbls), vrbl_name = vvs$vrbl_lbls)
    xtsum_res2[lbl_dt, name := vrbl_name, on = .(name = vrbl)]

    ## yeet some squared variables
    xtsum_res2 <- xtsum_res2[!grepl("sqrd", name)]


    plt_xtsum <-  ggplot(xtsum_res2, aes(x=within_prop, y=between_prop, color = within_between_ratio)) +
        geom_point() +
        geom_text_repel(aes(label = name), force_pull = 0, force = 1, point.padding = 0) + 
                                ## geom_hline(yintercept = 0.5) +
        ## geom_vline(xintercept = 0.5) +
        scale_color_gradient2(low = "blue", mid = "darkgray", high = "red", midpoint = 1) +
        xlim(c(min(xtsum_res2$within_prop)-0.1,max(xtsum_res2$within_prop))) +
        ylim(c(min(xtsum_res2$between_prop), max(xtsum_res2$between_prop +0.03))) +
        theme(legend.position = c(0.2, 0.3))

    ## ggplot(xtsum_res2, aes(x=within_between_ratio, y = name)) +
    ##     geom_bar(stat="identity")
    pdf(paste0(FIG_DIR, "plt_xtsum.pdf"), width = 9, height = 6)
    plot(plt_xtsum)
    dev.off()
}


render_xtsum_prop_plt(df_reg_rts)

render_xtsum_plt2 <- function(cbn_dfsx, df_regx, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate variability plots for variables and samples
    1
    4
    5
    65
    
    
    ## get super long data 
    dt_splong <- gen_dt_splong(cbn_dfsx, df_regx) %>%
        .[!grepl("_global", variable) & variable %!in% vvs$crscn_vars] ## yeet global and cross-sectional vars

    ## xtsum(dt_splong[cbn_name == "cbn_all" & variable == "nbr_opened"], value, iso3c)
    ## proc_xtsum(dt_splong[cbn_name == "cbn_all" & variable == "nbr_opened"], "value", "iso3c")

    ## do it per variable and combination
    dt_xtsum <- dt_splong %>% .[iso3c != "ISL"] %>% 
        .[, proc_xtsum(.SD, "value", "iso3c"), by = .(cbn_name, variable)] %>%
        .[, within_overall_prop := within/overall]
    
    ## dt_xtsum$within_overall_prop

    dt_xtsum %>% 
        ## ggplot(aes(x=within_overall_prop, y = variable)) +
        ggplot(aes(x=within_overall_prop, y = cbn_name)) +
        geom_bar(stat = "identity") + 
        ## facet_grid(variable~cbn_name, scales = "free_y")
        facet_wrap(~variable)


    dt_xtsum %>% 
        ggplot(aes(x=within_overall_prop, y = variable)) +
        geom_bar(stat = "identity") +
        facet_wrap(~cbn_name)

    

    plt_xtsum <- dt_xtsum %>%
        .[, variable2 := factor(variable, levels =  rev(names(vvs$vrbl_lbls)))] %>% 
        ggplot(aes(x=within_overall_prop, y = variable2, fill = cbn_name)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme(legend.position = "bottom") +
        scale_y_discrete(labels = vvs$vrbl_lbls) +
        labs(x="within-country variance / overall variance", y = 'variable')


    plt_to_pdf(plt_xtsum, width = 7, height = 8, fig_name = "plt_xtsum")

}

render_xtsum_plt2(cbn_dfs_rates_uscld, df_reg_rts, vvs)

## * descriptives per combination

## ** summary table


nicely_fmt_number <- function(vlu) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' round number to nice looking number of decimal places
    #' depends on size


    nbr_digits_before_comma <- floor(log10(vlu))
    
    ## need to use convoluted if-block because switch only allows integer/string matching
    if (nbr_digits_before_comma < 0) {rnd <- 3
    } else if (nbr_digits_before_comma == 0) {rnd <- 2
    } else if (nbr_digits_before_comma == 1) {rnd <- 1
    } else if (nbr_digits_before_comma > 1) {rnd <- 0}

    ## add some special cases for 0 and 1: don't need decimal places
    if (vlu %!in% c(0,1)) {
        format(round(vlu, rnd), nsmall = rnd, big.mark = " ")
    } else {
        format(vlu, nsmall = 0)
    }
}


## nicely_fmt_number(28123.21)
## nicely_fmt_number(0.212123)

gen_dt_splong <- function(dfs_cbnsx, df_regx) {
    #' generate super long (quintuple) dataframe: iso3c, year, cbn_name, variable, value
    #' useful for summary descriptives across combinations
    
    dtx_cbn <- imap(dfs_cbnsx[1:3], ~adt(.x)[, cbn_name := .y]) %>% # cbn_dfds_rates_uscld go brrr
        map_dfr(~adt(select(df_regx, iso3c, year, nbr_opened))[.x, on =.(iso3c, year)]) %>% # add DV
        melt(id.vars = c("iso3c", "year", "cbn_name")) %>%
        ## yeet obs with lag!=0, or unlagged (crscn,dv)
        .[grepl("_lag0$", variable) | !grepl("_lag\\d$", variable)] %>% 
        .[, variable := gsub("_lag0", "", variable)] # yeet remaining lag indication

    return(dtx_cbn)
}


gen_render_sum_stats_rates <- function(df_regx, dfs_cbnsx, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' create and render summary stats

    dtx_cbn <- gen_dt_splong(dfs_cbnsx, df_regx)

    ## generate statistics separately
    func_names = c("mean", "sd", "min", "max")
    funcs <- lapply(func_names, get)
    sumry_sprt <- dtx_cbn[, lapply(funcs, \(x) x(value)), by = c("cbn_name", "variable")]
    setnames(sumry_sprt, c("cbn_name", "variable", func_names))

    ## generate combination names
    cbn_lbls_dt <- data.table(cbn_name = names(vvs$cbn_lbls), cbn_lbl = vvs$cbn_lbls)
    cbn_lbls_dt$n <- map_int(cbn_lbls_dt$cbn_name, ~nrow(cbn_dfs_rates[[.x]]))
    cbn_lbls_dt[, cbn_lbl := sprintf("%s (n=%s)", cbn_lbl, n)]
        
    ## nicely format; have to move to second value column to convert to character
    ## also need some ugly filtering in by to apply nicely_fmt_number row-wise: 
    sumry_sprt_mlt <- melt(sumry_sprt, id.vars = c("cbn_name", "variable"), variable.name = "stat") %>%
         .[!is.na(value)] %>% .[, rnbr := .I] %>% .[, value2 := nicely_fmt_number(value), by = rnbr]
 
    ## cast and reorder (matching with vvs)
    wide_tbl_sprt <- sumry_sprt_mlt %>%
        .[, .(cbn_name, variable, stat, value = value2)] %>% 
        dcast.data.table(variable ~ cbn_name + stat) %>%
        .[na.omit(match(names(vvs$vrbl_lbls), variable))]

    ## use real names
    dt_vrbl_lbls <- data.table(vrbl_name = names(vvs$vrbl_lbls), vrbl_lbl =  vvs$vrbl_lbls)
    wide_tbl_sprt[dt_vrbl_lbls, variable := latexTranslate(vrbl_lbl), on = .(variable = vrbl_name)]

    ## fill up missing values with "-"
    wide_tbl_sprt[is.na(wide_tbl_sprt)] <- "--"

    stat_dt_sprt <- data.table(stat_name = func_names, stat_lbl = c("Mean", "SD", "Min.", "Max."))

    ## generate cmidrules: better than hlines
    cmidrules <- map_chr(seq(1,3), ~sprintf("\\cmidrule(r){%s-%s}", (.x * 4)-2, (.x*4)+1)) %>% paste0(collapse = "")
    ## generate the headers: some hacking with hlines/cmidrules
    clm_names <- list()
    clm_names$pos <- list(-1, -1)
    clm_names$command <- c(
        paste0(paste0(c("\\hline \n ",
                        map_chr(cbn_lbls_dt$cbn_lbl, ~sprintf("\\multicolumn{4}{c}{%s}", .x))),
                      collapse = " & "),  # gen dataset n=x
               " \\\\ \n", cmidrules), # add dataset cmidrules separators
        ## gen stat headers1
        paste0(paste0(c(" \n Variable", rep(stat_dt_sprt$stat_lbl, 3)), collapse = " & "), " \\\\ \n"))


    xtable(wide_tbl_sprt, align = c("l", "p{6.5cm}", rep("l", 12))) %>%
        print(include.rownames = F, include.colnames = F,
              file = paste0(TABLE_DIR, "summary_stats4.tex"),
              add.to.row = clm_names,
              hline.after = c(0),
              sanitize.text.function = identity)

}

gen_render_sum_stats_rates(df_reg_rts, cbn_dfs_rates_uscld, vvs)


## ** yeet outliers
## *** global

vvs$all_rel_vars


hist(df_reg_rts$hnwi_nbr_5M)
max(df_reg_rts$hnwi_nbr_5M, na.rm = T)

meanx <- mean(df_reg_rts$hnwi_nbr_5M, na.rm = T)
medx <- median(df_reg_rts$hnwi_nbr_5M, na.rm = T)
sdx <- sd(df_reg_rts$hnwi_nbr_5M, na.rm = T)

cix_hi <- meanx + 4*sdx
cix_lo <- meanx - 4*sdx

lapply(seq(3,6), \(x)
       filter(df_reg_rts, hnwi_nbr_5M > (meanx + x*sdx) | hnwi_nbr_5M < (meanx - x*sdx)) %>% adt() %>%
       .[, .N, iso3c] %>% .[, src := paste0("sd", x)]) %>% rbindlist() %>%
    dcast.data.table(iso3c ~ src, value.var = "N", drop=F) %>%
    .[order(-sd6, -sd5, -sd4, -sd3)]

filter(df_reg_rts, hnwi_nbr_5M > (meanx + x*sdx) | hnwi_nbr_5M < (meanx - x*sdx), iso3c == "ARE") %>% sel


check_outliers <- function(dfx, vrbl, ret_obj) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate summary of which countries have how many outliers at 3-6 SD

    print(vrbl)
    dtx <- adt(dfx)
    ## vrbl <- "hnwi_nbr_5M"
    
    meanx <- mean(dtx[[vrbl]], na.rm = T)
    sdx <- sd(dtx[[vrbl]], na.rm = T)

    
    ## x <- adt(mtcars) %>% .[, lapply(.SD, mean), .SDcols = vrbl, drop = F]

    ## meanx <- dtx[, mean(get(vrbl), na.rm = T)]
    ## sdx <- dtx[, mean(get(vrbl), na.rm = T)]

    ## adt(mtcars) %>% .[disp == 160, disp := NA] %>% .[, mean(disp, na.rm = T)]

    sd_cols <- paste0("sd", seq(6,3))


    outlier_sumry_up <- map_dfr(seq(3,6), ~dtx[get(vrbl) > (meanx + .x*sdx)] %>% .[, .N, iso3c] %>%
                                              .[, `:=`(src = paste0("sd", .x), dir ="up")])

    outlier_sumry_down <- map_dfr(seq(3,6), ~dtx[get(vrbl) < (meanx - .x*sdx)] %>% .[, .N, iso3c] %>%
                                                .[, `:=`(src = paste0("sd", .x), dir = "down")])
    
    outlier_sumry <- rbind(outlier_sumry_up, outlier_sumry_down)
    outlier_sumry$vrbl <- vrbl
    

    ## generating outlier summary visual: look at where the outliers are, in wide format

    if (nrow(outlier_sumry) > 0) {
        outlier_sumry_vis <- dcast.data.table(outlier_sumry, iso3c ~ src, value.var = "N", drop = F)
        
        outlier_sumry_vis[,setdiff(sd_cols, names(outlier_sumry_vis))] <- NA
        setorderv(outlier_sumry_vis, cols = sd_cols, order = -1, na.last = T)
        outlier_sumry_vis$vrbl <- vrbl
        
    } else {

        outlier_sumry_vis <- NULL
    }

    ## get actual data with outlier values
    vrbl_cols <- c("iso3c", "year", vrbl, "src")
    outlier_vlus <- map_dfr(seq(3,6), ~dtx[get(vrbl) > (meanx + .x*sdx) | get(vrbl) < (meanx - .x*sdx)] %>%
                                          .[, src := paste0("sd", .x)] %>% .[, ..vrbl_cols]) %>% 
        .[, paste0(src, collapse = ","), by=c("iso3c", "year", vrbl)]
    
    return(get(ret_obj))
}

check_outliers(df_reg_rts, "hnwi_nbr_5M", ret_obj = "outlier_sumry")
check_outliers(df_reg_rts, "pm_density_global", ret_obj = "outlier_sumry")

outlier_vars_to_check <- setdiff(vvs$all_rel_vars, "NPO.tax.exemption")


outlier_sumry <- map_dfr(outlier_vars_to_check, ~check_outliers(df_reg_rts, .x, ret_obj = "outlier_sumry"))
outlier_sumry[dir=="down", N:= -N] # make the ones where the outlier is to the bottom negative

outlier_sumry[, .(cnt = sum(N)), iso3c][order(-cnt)] %>%
    .[, country_name := countrycode(iso3c, "iso3c", "country.name")] 

outlier_sumry %>% copy() %>% 
    .[countrycode(iso3c, "iso3c", "un.region.name") != ("Europe")] %>% 
    ggplot(aes(x=N, y=iso3c, fill = src)) +
    geom_bar(stat="identity") +
    facet_wrap(~vrbl, scales = "free")



## check cases on country-variable basis
## check_outliers(df_reg_rts, "ghweal992j", ret_obj = "outlier_vlus") %>% print(n=100)
check_outliers(df_reg_rts, "hnwi_nbr_30M", ret_obj = "outlier_vlus") %>% print(n=100)
viz_lines(df_reg_rts, y="hnwi_nbr_1M")
viz_lines(df_reg_rts, y="hnwi_nbr_5M")
viz_lines(df_reg_rts, y="hnwi_nbr_30M")
viz_lines(df_reg_rts, y="hnwi_nbr_200M")
viz_lines(df_reg_rts, y="hnwi_nbr_1B") ## doesn't make sense to use, too many zeroes even in richest ones




check_outliers(df_reg_rts, "ghweal992j", ret_obj = "outlier_vlus") %>% print(n=100)

check_outliers(df_reg_rts, "shweal992j_p90p100", ret_obj = "outlier_vlus") %>% print(n=100)
check_outliers(df_reg_rts, "shweal992j_p99p100", ret_obj = "outlier_vlus") %>% print(n=100)

filter(df_reg_rts, iso3c %in% c("DEU", "FRA")) %>% select(iso3c, year, smorc_dollar_fxm) %>% print(n=100)
filter(df_reg_rts, iso3c %in% c("DEU", "QAT")) %>% select(iso3c, year, NY.GDP.PCAP.CDk) %>% print(n=100)

check_outliers(df_reg_rts, "smorc_dollar_fxm", ret_obj = "outlier_vlus") %>% print(n=100)
viz_lines(df_reg_rts, y="smorc_dollar_fxm", duration = 2, time_level = "cuts")

filter(df_reg_rts, smorc_dollar_fxm > 1500) %>% select(iso3c, year, smorc_dollar_fxm) %>% print(n=50)
filter(df_reg, iso3c == "QAT") %>% select(iso3c, year, smorc_dollar_fxm, nbr_opened) %>% print(n=50)
check_outliers(cbn_dfs_rates$cbn_all, "smorc_dollar_fxm_lag0", ret_obj = "outlier_vlus") %>% print(n=50)
map(cbn_dfs_counts, ~filter(.x, iso3c == "QAT") %>% nrow()) # check how many CYs affected


check_outliers(df_reg_rts, "sptinc992j_p90p100", ret_obj = "outlier_vlus")
viz_lines(df_reg_rts, y="sptinc992j_p90p100")
filter(df_reg_rts, iso3c == "MWI") %>% select(iso3c, year, sptinc992j_p90p100) %>% print(n=50)

check_outliers(df_reg_rts, "sptinc992j_p99p100", ret_obj = "outlier_vlus")
viz_lines(df_reg_rts, y="sptinc992j_p99p100")

check_outliers(df_reg_rts, "ti_tmitr_interact", ret_obj = "outlier_vlus")
check_outliers(df_reg_rts, "tmitr_approx_linear20step", ret_obj = "outlier_vlus")
viz_lines(df_reg_rts, y="tmitr_approx_linear20step", duration = 1)


df_reg_rts %>% mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>% 
viz_lines(y="ghweal992j", facets = "region")

## **** outlier detection splong

dt_splong <- gen_dt_splong(cbn_dfs_rates_uscld, df_reg_rts)

## debug 
dt_splong %>% copy() %>% .[variable == "nbr_opened" & cbn_name == "cbn_all"] %>%
    .[, max_vlu := max(value), iso3c] %>%
    .[max_vlu %in% rev(unique(max_vlu))[1:5]] %>% .[, .N, iso3c]

## check variables
dt_splong[, .N, by = .(variable, cbn_name)] %>% print(n=100)


## highest value per variable and combination
## first generate  max value per combination-country-variable,
## then generate threshold (largest 5), then filter those who surpass threshold

library(plotly)

p <- dt_splong %>% copy() %>%
    .[!grepl("sqrd", variable) & !grepl("global", variable) & !grepl("squared", variable)] %>%
    ## .[variable == "pm_density"] %>% 
    .[, max_vlu := max(value), by = .(cbn_name, variable, iso3c)] %>%
    .[, thld := rev(sort(unique(max_vlu)))[8], by = .(cbn_name, variable)] %>%
    .[max_vlu > thld] %>%
    ggplot(aes(x=year, y=value, color = iso3c)) +
    geom_line() +
    ## facet_grid(variable ~ cbn_name, scales = "free", switch = "y") +
    facet_wrap(~ variable +cbn_name, scales = "free") + 
    theme(strip.text.y.left = element_text(angle = 0),
          panel.spacing = unit(0, "lines"))
p
    
ggplotly(p)    


## **** some more manual outlier detection, leading to yeeting of ISL and BHS
## previous outlier detection not global

## requires dt_splong

    dt_splong[variable == "clctr_cnt_cpaer" & cbn_name == "cbn_no_cult_spending_and_mitr"] %>% atb() %>% 
        viz_lines(y="value", duration = 4)

    filter(df_reg_rts, SP.POP.TOTLm > 5)

    filter(df_reg_rts, SP.POP.TOTLm > 5) %$%
        hist(SP.POP.TOTLm, breaks = 100)

    plot(map_int(seq(0, 10, 0.1), ~nrow(filter(cbn_dfs_rates$cbn_all, SP_POP_TOTLm_lag0_uscld < .x))), type = 'l')

    viz_lines(cbn_dfs_rates$cbn_no_cult_spending_and_mitr, y = "hnwi_nbr_30M_lag0", duration = 4)

    filter(cbn_dfs_rates$cbn_all, year == 2000) %>% select(iso3c, cnt_contemp_1990) %>%
        arrange(-cnt_contemp_1990)

    viz_lines(df_reg_rts, y = "cnt_contemp", duration = 4)
    
    ## df_reg_rts %>% filter(iso3c %in% c("BHS", "ISL")) %>% group_by(iso3c) %>% summarize(sum(nbr_opened))
    ## filter(cbn_dfs_rates$cbn_no_cult_spending_and_mitr, iso3c == "ISL", nbr_opened == 1)

    df_reg %>% filter(iso3c %in% c("BHS", "ISL")) %>% select(iso3c, year, clctr_cnt_cpaer) %>% print(n=200)




## **** check squared weirdness
filter(df_reg_rts, iso3c == "KOR") %>% select(iso3c, year, pm_density, pm_density_sqrd, SP.POP.TOTLm) %>%
    arrange(-year)

filter(df_reg, iso3c == "KOR") %>% select(iso3c, year, pm_density, pm_density_sqrd, SP.POP.TOTLm) %>%
    arrange(-year)

filter(df_reg_rts, iso3c == "DEU") %>% select(iso3c, year, pm_density, pm_density_sqrd, SP.POP.TOTLm) %>%
    arrange(-year)

filter(df_reg, iso3c == "DEU") %>% select(iso3c, year, pm_density, pm_density_sqrd, SP.POP.TOTLm) %>%
    arrange(-year)




## *** within

df_lag <- df_reg_rts %>% adt() %>%
    melt(id.vars = c("iso3c", "year"), measure.vars = vvs$all_rel_vars) %>%
    .[order(iso3c, variable, year)] %>%
    ## .[iso3c == "DEU" & variable %in% c("hnwi_nbr_30M", "gptinc992j")] %>% na.omit() %>% 
    .[, vlu_lag := shift(value), by=c("iso3c", "variable")]
    ## .[, x:= shift(value)] %>% 
    ## print(n=80)
    
df_lag[variable == "nbr_closed_cum_global"]

df_lag2 <- df_lag %>% na.omit() %>% copy() %>%
    .[,`:=`(diff_minus =value - vlu_lag, diff_dvide1 = value/vlu_lag, diff_dvide2 = vlu_lag/value)] %>%
    .[!is.infinite(diff_dvide1) & !is.infinite(diff_dvide2)] %>% # filter out infinite values
    .[, max_diff := pmax(diff_dvide1, diff_dvide2)] %>% # pmax: parallelized max: pick which value is larger
    .[, sd_v := sd(value), by = "variable"] %>% # add vrbl sd to see whether change is large in terms of overall SD
    .[, sd_c := sd(value), by = c("iso3c", "variable")] %>% ## add country-variable sd sz
    .[, `:=`(sd_sz_v = diff_minus/sd_v, sd_sz_c = diff_minus/sd_c)]
    


    
hist(df_lag2$max_diff, breaks = 30)

df_lag2[max_diff > 1.3 & sd_sz_v > 1] %>% print(n=100)

df_lag2[max_diff > 2 & sd_sz_c > 2 & sd_sz_v > 0.4] %>% print(n=100)
## hmm there seem to be some pretty drastic changes in hnwi in NOR (hnwi_
## also in smorc, but that's fairly plausible

## inspect HNWI variables, yuuuge fluctuations for ITA

adt(df_reg_rts)[iso3c %in% c("NOR", "ITA", "DEU", "FRA")] %>% atb() %>% 
    viz_lines(y="hnwi_nbr_30M", duration = 1)

adt(df_reg)[iso3c == "USA", .(year, ghweal992j)] %>% na.omit() %>% plot(type='l')


wealth_df <- get_wealth_df("wid_v3")

filter(wealth_df, iso3c == "ITA") %>% select(year, pct_lo, wealth_usd21) %>% unique() %>%
    ## filter(wealth_usd21 < 27e6 & wealth_usd21 > 25e6) %>%
    filter(wealth_usd21 < 1e8 & wealth_usd21 > 2e7) %>% 
    ggplot(aes(x=year, y=wealth_usd21, group = pct_lo)) +
    geom_line() 
    ## coord_cartesian(ylim = c(26e6, 27e6), xlim = c(2003,2005))



    
df_lag2[max_diff > 1.3 & sd_sz_c > 1 & sd_sz_v > 0.3] %>%
    ## .[, .N, variable] %>% .[order(-N)]
    .[variable == "sptinc992j_p99p100", .(iso3c, year, value, vlu_lag, sd_sz_c, sd_sz_v)] %>%
    .[order(-sd_sz_c, -sd_sz_v)]  %>%
    ## print(n=25) %>%
    # just merge those with df_reg_rts to inspect
    .[, .(iso3c)] %>% unique() %>% adt(df_reg_rts)[., on="iso3c"] %>% atb() %>% 
    viz_lines(y="sptinc992j_p99p100", facets = "iso3c", duration = 1) # 
    
    



    

    
