## * descriptives
## library(psych)

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

## cbn_plots <- gen_cbn_plots(cbn_dfs_rates_uscld, df_reg)
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


## render_xtsum_prop_plt(df_reg_rts)

render_xtsum_plt2 <- function(cbn_dfsx, df_regx, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate variability plots for variables and samples
        
    ## get super long data 
    dt_splong <- gen_dt_splong(cbn_dfsx, df_regx) %>%
        .[!grepl("_global", variable) & variable %!in% vvs$crscn_vars] %>% ## yeet global and cross-sectional vars
        .[, any_na := any(is.na(value)), by = .(cbn_name, variable)] %>% ## adjust variables to combinations
        .[any_na == F]

    ## xtsum(dt_splong[cbn_name == "cbn_all" & variable == "nbr_opened"], value, iso3c)
    ## proc_xtsum(dt_splong[cbn_name == "cbn_all" & variable == "nbr_opened"], "value", "iso3c")

    ## do it per variable and combination
    dt_xtsum <- dt_splong %>% .[iso3c != "ISL"] %>% 
        .[, proc_xtsum(.SD, "value", "iso3c"), by = .(cbn_name, variable)] %>%
        .[, within_overall_prop := within/overall]
    
    ## dt_xtsum$within_overall_prop

    
    plt_xtsum <- dt_xtsum %>%
        .[, variable2 := factor(variable, levels =  rev(names(vvs$vrbl_lbls)))] %>% 
        ggplot(aes(x=within_overall_prop, y = variable2, fill = cbn_name)) +
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
        theme(legend.position = "bottom") +
        scale_y_discrete(labels = vvs$vrbl_lbls) +
        labs(x="within-country variance / overall variance", y = 'variable')


    plt_to_pdf(plt_xtsum, width = 7, height = 8, fig_name = "plt_xtsum")

}

## render_xtsum_plt2(cbn_dfs_rates_uscld, df_reg_rts, vvs)

## * descriptives per combination

## ** summary table





## nicely_fmt_number(28123.21)
## nicely_fmt_number(0.212123)


## ** main

if (identical(args, character(0))) {
    stop("functions are done")
}


if (is.null(args[[1]])) {
    stop("functions are DONE")
}

cbn_plots <- gen_cbn_plots(cbn_dfs_rates_uscld, df_reg)
lapply(cbn_plots, \(x) plt_to_pdf(x$plt, width = x$width, height=x$height, fig_name = paste0(x$name, "_v2")))


render_xtsum_prop_plt(df_reg_rts)

render_xtsum_plt2(cbn_dfs_rates_uscld, df_reg_rts, vvs)



## gen_render_sum_stats_rates(df_reg_rts, cbn_dfs_rates_uscld, vvs)

print("all descriptives done")



