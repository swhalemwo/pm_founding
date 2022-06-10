    library(psych)

## * generate the descriptives tables/figures for the update

### ** tax incentives

get_taxinc_descriptives <- function() {

    df_taxinc <- read_in_tax_incentives()


    tax_inc_descriptives <- describe(df_taxinc[tax_vars_caf], skew = F)

    tax_inc_xtbl <- xtable(
        tax_inc_descriptives[names(tax_inc_descriptives) != "vars"],
        label = "tax_inc_descriptives",
        caption = "Tax Incentive Descriptives"
    )

    print(tax_inc_xtbl,
          include.rownames = T,
          ## floating = FALSE,
          file = paste0(TABLE_DIR, "tax_inc_descriptives.tex")
          )

    res.pca.caf <- prcomp(na.omit(df_taxinc[,tax_vars_caf]),scale = T)
    df_taxinc$region <- countrycode(df_taxinc$iso3c, "iso3c", "un.regionsub.name")

    p.scree <- fviz_eig(res.pca.caf, title = "scree plot")
    p.arrows <- viz_pca_arrows(res.pca.caf, title = "factor loadings")

    pdf(paste0(FIG_DIR, "tax_inc_pca_caf.pdf"), height = 5, width = 10)
    grid.arrange(grobs = list(p.scree, p.arrows), ncol=2, as.table=F)
    dev.off()

    
    
    ind_plt <- create_ind_plot(df_taxinc, res.pca.caf, label_col = "country", title = "CAF country plot", color_col = "region")
    pdf(paste0(FIG_DIR, "tax_inc_pca_caf_ind.pdf"), height = 11, width = 19)
    plot(ind_plt)
    dev.off()
}

## get_taxinc_descriptives() 


## ** mow

get_mow_descriptives <- function() {
    
    mow_res <- get_mow_dfs()

    mow_df <- mow_res$mow_crssctn

    
    mow_cntns <- mow_res$mow_cntns

    cry_df <- as_tibble(unique(df_anls$iso3c), .name_repair = ~c("iso3c"))
    mow_df <- as_tibble(merge(cry_df, mow_df, all.x = T))

    mow_df[is.na(mow_df)] <- 0
    
    
    mow_descs <- describe(mow_df[names(mow_df) != "iso3c"], skew = F)
    mow_tbl <- xtable(mow_descs[names(mow_descs) %!in% c("vars", "se")],
                      label = "mow_descriptives",
                      caption = "Museum of the World Descriptives")

    print(mow_tbl, include.rownames=T, file = paste0(TABLE_DIR, "mow_descriptives.tex"))


    ## also add the longitudinal measures here for the argument that in recent years coverage has declined
    ## also add that many recently opened private museums are not included, such as Voorlinden, Inhotim, Long Museum


    ## maybe facets
    expand_df <- tidyr::expand(mow_cntns, year=year, iso3c=iso3c)
    mow_lines <- as_tibble(merge(expand_df, mow_cntns, all.x = T))
    mow_lines[is.na(mow_lines)] <- 0

    mow_lines_melt <- as_tibble(reshape2::melt(mow_lines, id=c("year", "iso3c")))

    max_crys <- aggregate(value ~ iso3c, mow_lines_melt, sum) %>%
        arrange(value) %>%
        top_n(8) %>%
        pull(iso3c) 
    

    library(tidyquant)
    
    mow_facets <- ggplot(filter(mow_lines_melt, iso3c %in% max_crys), aes(x=year, y=value, color = iso3c)) +
        facet_wrap(~ variable, scales = "free") + 
        geom_ma(n=5, linetype="solid") +
        scale_color_brewer(palette = "Paired") +
        labs(y="count of museum openings, 5 year rolling avg")

    
    pdf(paste0(FIG_DIR, "mow_facets.pdf"), width = 8, height = 4)
    plot(mow_facets)
    dev.off()
    
}

## get_mow_descriptives()
    
## ** HWNI

get_hnwi_descriptives <- function() {

## the table, some columns in scientific notation

    df_hwni <- get_hnwi_pcts()

    cutoff_vlus <- c(1e6, 2.5e6, 5e6, 10e6, 50e6, 100e6, 250e6,500e6)
    col_names <- unlist(lapply(cutoff_vlus, function(x) paste0("pct_cutoff_", sanitize_number(x))))

    df_hwni2 <- unique(df_hwni[which(df_hwni[,col_names] < 10),])
    hwni_descs <- describe(df_hwni2[,names(df_hwni2) %!in% c("iso3c", "region", "label", "year")], skew = F)

    hnwi_tbl <- xtable(hwni_descs[names(hwni_descs) %!in% c("vars", "se", "range")],
                       caption = "HNWI descriptives (excluding largest outliers)",
                       label = "hnwi_descs",
                       digits = c(2,2,-2, -2,2,-2)
                       )

    print(hnwi_tbl, include.rownames = T, file = paste0(TABLE_DIR, "hnwi_descs.tex"))


## want random label position
## ideally would want location on graph that places the label at a spot where there is little overlap

    df_hwni2 <- filter(df_hwni, pct_cutoff_10M < 1)

    df_hwni2$region <- countrycode(df_hwni2$iso3c, "iso3c", "un.regionsub.name")

    ## distribute labels across plot 

    label_df <- df_hwni2 %>%
        group_by(iso3c) %>%
        summarise(year = sample(year, size = 1), label = sample(iso3c,1))

    df_hwni2 <- as_tibble(merge(df_hwni2, label_df, all.x = T))

    ## spread lines across facets

    max_lines <- 12

    df_hwni2$colr <- 0

    for (i in unique(df_hwni2$region)) {
        print(i)
        ctr <- 1
        while (TRUE) {
            crys <- unique(filter(df_hwni2, region==i)$iso3c)
            ## print(len(crys))
            crys_sel <- crys[1:min(max_lines, len(crys))]
            df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$region <- paste0(i, "_", ctr)
            df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$colr <- as.numeric(factor(df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$iso3c))
            ctr <- ctr+1
            if (len(crys) <= max_lines) {
                break
            }
        }
    }



    hwni_plt <- ggplot(filter(df_hwni2, region != "Melanesia_1"), aes(x=year, y=pct_cutoff_10M, group = iso3c, color = factor(colr))) +
        facet_wrap(~ region, scales = "free", ncol = 4) +
        geom_line(size = 1, show.legend = F) +
        geom_label_repel(aes(label = label), show.legend = F, size=3) +
        scale_color_manual(values = colors_manual)
    ## scale_color_brewer(palette = "Paired")

    pdf(paste0(FIG_DIR, "hwni_curves.pdf"), width = 18, height = 13)
    plot(hwni_plt)
    dev.off()

}

## get_hnwi_descriptives()


## ** controls 
get_control_descriptives <- function() {


    controls_descs <- describe(df_reg[,c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "sptinc992j_p99p100", "shweal992j_p99p100", "ghweal992j", "gptinc992j", "nbr_opened_cum", "nbr_opened_cum_sqrd")], skew = F)

    controls_tbl <- xtable(controls_descs[names(controls_descs) %!in% c("vars", "se", "range")],
                           caption = "Descriptives for controls",
                           label = "controls_desc")

    print(controls_tbl, file = paste0(TABLE_DIR, "controls_desc.tex"), include.rownames=T)
}

get_all_descriptives <- function(){
    get_taxinc_descriptives()
    get_mow_descriptives()
    get_hnwi_descriptives()
    get_control_descriptives()
}

## get_all_descriptives()

## * descriptives based on df_reg



df_reg$smorc_dollar_fx <- df_reg$smorc_dollar_fx/1e6

rel_vars <- c("nbr_opened" = "Number of Private Museums opened",
              "sum_core" = "Tax incentives",
              "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax Incentives",
              "tmitr_approx_linear20step" = "Marginal Income Tax Rate (%)",
              "hnwi_nbr_1M" = "# HNWIs with net worth >= 1M",
              "hnwi_nbr_5M" = "# HNWIs with net worth >= 5M",
              "hnwi_nbr_30M" = "# HNWIs with net worth >= 30M",
              "hnwi_nbr_200M" = "# HNWIs with net worth >= 200M",
              "hnwi_nbr_1B" = "# HNWIs with net worth >= 1B",
              "sptinc992j_p90p100" = "Income share of top 10%",
              "sptinc992j_p99p100" = "Income share of top 1%",
              "gptinc992j" = "Gini of pre-tax income",
              "ghweal992j"= "Gini of net wealth",
              "shweal992j_p90p100" = "Wealth share of top 10%",
              "shweal992j_p99p100" = "Wealth share of top 1%",
              "smorc_dollar_fxm" = "Gvt cultural spending (millions)",
              "NY.GDP.PCAP.CDk" = "GDP per capita (thousands)",
              "SP.POP.TOTLm" = "Population (millions)",
              "cnt_contemp_1985" = "# Museums of contemporary art in 1985",
              "clctr_cnt_cpaer" = "# Collectors in Artnews collector list",
              "ln_s" = "ln(s)",
              "cons" = "cons",
              "ln_r" = "ln(r)",
              "cnt_contemp_1995" = "# of modern/contemp. art museums in 1995"
              )





get_more_vrbl_info <- function(x) {

df_reg %>% select(iso3c, year, x) %>% na.omit() %>%
    mutate(vrbl = x) %>% 
    summarize(nbr_crys = n_distinct(iso3c),
              min_time = min(year), max_time = max(year),
              time_range = paste0(min_time, "-", max_time),
              vrbl = unique(vrbl)) %>%
    select(vrbl, nbr_crys, time_range) %>% as.list()
}
## get_more_vrbl_info("smorc_dollar_fx")

more_vrbl_info <- rbindlist(lapply(names(rel_vars), get_more_vrbl_info))


                  
              
var_table <- describe(df_reg[,names(rel_vars)]) %>% select("Country-years" = n, mean, sd, median, min, max)
rownames(var_table) <- recode(rownames(var_table), !!!rel_vars)
var_table$`# Countries` <- more_vrbl_info$nbr_crys

var_xtbl <- xtable(var_table, caption = "main variables (all monetary variables are in or based on 2021 constant US dollars)", label = "var_desc", digits = c(0, 0, rep(2,6)))

print(var_xtbl, file = paste0(TABLE_DIR, "var_desc.tex"), include.rownames = T, hline.after =c(-1,0,7,11))

## * coverage visualizations

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
    scale_color_manual(values = colors_manual3)

plt_to_pdf <- function(plt, width, height, fig_name) {
    if (missing(fig_name)) { 
        fig_name <- deparse(substitute(plt))
    }

    
    
    pdf(paste0(FIG_DIR, fig_name, ".pdf"), width = width, height = height)
    plot(plt)
    dev.off()
}

plt_to_pdf2 <- function(plt, width, height) {
    
    
    
    
    pdf(paste0(FIG_DIR, fig_name, ".pdf"), width = width, height = height)
    plot(plt)
    dev.off()
}

plt_to_pdf(cpltns_vrbl_plot, width = 8, height = 4)





library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

gen_cbn_plots <- function(cbn_dfs, df_reg) {
    #' generate plots that describe the variable coverage 
      
    source_df <- lapply(names(cbn_dfs), \(x) cbn_dfs[[x]] %>% select(iso3c, year) %>%
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
    world_w_data_facet <- lapply(names(cbn_dfs), \(x) merge(world,
                                      filter(source_df, source == x), all.x = T) %>%
                                      ## mutate(source = ifelse(is.na(source), paste0(x, "na") ,source)) %>%
                                      mutate(source_facet = x) %>% 
                                atb()) %>%
        list.rbind() %>%
        mutate(source_facet = factor(source_facet, levels = names(cbn_dfs)))
    
    filter(world_w_data_facet, source == "cbn_all")

    merge(world,     
          filter(source_df, source == "cbn_all"), all.x = T) %>% atb()

    plt_world_facet <- ggplot(world_w_data_facet) +
        geom_sf(aes(geometry = geometry, fill= source, alpha = cnt)) +
        coord_sf(ylim = c(-55, 83)) + 
        facet_wrap(~source_facet, ncol = 1)

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
        scale_color_manual(values = colors_manual3, labels = rel_vars) +
        scale_linetype_manual(labels = rel_vars)

        
    
    cpltns_vrbl_plot_res <- list(plt = cpltns_vrbl_plot, width = 6, height = 4, name = "cpltns_vrbl_plot")

    return(list(plt_world_facet_res, plt_world_stacked_res, tile_plt_res, line_plt_res, cpltns_vrbl_plot_res))
}



cbn_plots <- gen_cbn_plots(cbn_dfs, df_reg)
lapply(cbn_plots, \(x) plt_to_pdf(x$plt, width = x$width, height=x$height, fig_name = paste0(x$name, "_v1")))
