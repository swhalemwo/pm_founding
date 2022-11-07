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

## some help functions
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



## * coverage visualizations




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

## * clustering

get_df_clust <- function(df_reg, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the dataframe used for clustering 

    df_clust_prep <- df_reg %>%
        filter(year >= 1995) %>% 
        select(iso3c, year, NY.GDP.PCAP.CDk, sptinc992j_p99p100, shweal992j_p99p100,
               NPO.tax.exemption, Ind.tax.incentives, cnt_contemp_1995,
               hnwi_nbr_30M, SP.POP.TOTLm) %>%
        mutate(cnt_contemp_1995 = cnt_contemp_1995/SP.POP.TOTLm,
               hnwi_nbr_30M = hnwi_nbr_30M/SP.POP.TOTLm) %>%
        select(-SP.POP.TOTLm) 


    ## pivot the columns into wider
    df_clust <- df_clust_prep %>%
        pivot_wider(id_cols = iso3c, names_from = year,
                    values_from = setdiff(names(df_clust_prep), vvs$base_vars))

    
    ## select the cross-sectional variables separately, yeet them from main df, then re-add them once
    ## (otherwise get added for every year)
    crscn_vrbls <- c("NPO.tax.exemption", "Ind.tax.incentives", "cnt_contemp_1995")
    crscn_vrbls_to_keep <- c("NPO.tax.exemption_2014", "Ind.tax.incentives_2014", "cnt_contemp_1995_1995")

    crscn_data <- df_clust %>% select(crscn_vrbls_to_keep)

    df_clust2 <- df_clust %>% select(-starts_with(crscn_vrbls)) %>%
        bind_cols(crscn_data)

    
    ## NA investigation
    ## rows: countries
    ## na.omit(df_clust2)

    ## df_clust2 %>%
    ##     mutate(nbr_nas = rowSums(is.na(.))) %>%
    ##     select(iso3c, nbr_nas) %>%
    ##     arrange(-nbr_nas) %>%
    ##     print(n=100)
    ## ## hmm there are around 20 with missing where it's worth considering to keep them (nbr_nas < 30)

    ## columns: variables

    ## colsums_na <- colSums(is.na(df_clust2))
    ## colsums_na_dt <- data.table(vrbl = names(colsums_na), nbr_na = colsums_na)
    ## colsums_na_dt[order(-nbr_na)] %>% print(n=50)
    ## colsums_na_dt[order(nbr_na)] %>% print(n=500)
    ## seems to go high quite quickly: sptinc992j, hnwi, shweal all have at least 40 missing
    ## yeeting all them would basically mean only using GDP and cross-sectional variables to cluster

    ## -> unless there's a comfy option to adjust distances to missing values, ~50 countries have to be yeeted



    return(df_clust2)
}

get_df_clust_lame <- function(df_reg, cutoffs = seq(0, 1, 0.25)) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' super lame way of clustering based on 2020 HDI cutoffs

    hdi_prep <- df_reg %>%
        filter(year == 2020) %>%
        select(iso3c, year, hdi) %>% adt()
        
    
    ## prepare for rolling join: remove last number (max)
    qntls <- quantile(hdi_prep$hdi, probs = cutoffs, na.rm = T) %>% adt() %>% .[1:.N-1]
    names(qntls) <- "hdi"
    qntls$cluster <- seq(1,nrow(qntls))
    ## qntls$asdf <- qntls$qntl
    
    ## do rolling joins work? yup
    dt_hdi_clustered <- qntls[na.omit(hdi_prep), on = "hdi", roll = Inf]

    ## test that rolling worked
    ## dt_hdi_clustered[, paste(paste0(iso3c, ":", hdi), collapse = ""), by = cluster] %>% .[order(cluster)]
    ## dt_hdi_clustered[, .(min_hdi = min(hdi), max_hdi = max(hdi)), cluster]

    return(dt_hdi_clustered)

}

get_df_clust_lame(df_reg)

    


run_cluster <- function(dists, method, nbr_clusts, na_rm) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' evaluate the clustering solution

    clusts <- hclust(dists, method = method)
    ## plot(clusts)

    ## incplt_cases <- which(!complete.cases(as.matrix(dists)))
    ## df_clust[incplt_cases,] %>% adf()

    ## as.matrix(dists)[incplt_cases,]
    
    
    tree_cutted <- cutree(clusts, k=nbr_clusts)

    clust_tbl <- table(tree_cutted)

    skew <- skewness(clust_tbl)
    mean_nbr_crys <- mean(clust_tbl)
    min_nbr_crys <- min(clust_tbl)
    max_nbr_crys <- max(clust_tbl)

    ## number of clusters with only one entry
    nbr_clusters_w_one <- len(which(clust_tbl == 1))


    return(
        list(
            nbr_clusts = nbr_clusts,
            method = method,
            skew = skew,
            mean_nbr_crys = mean_nbr_crys,
            min_nbr_crys = min_nbr_crys,
            max_nbr_crys = max_nbr_crys,
            nbr_clusters_w_one = nbr_clusters_w_one,
            na_rm = na_rm)) 
}


## just yeet Channel Islands and South Sudan (don't have anything in common)
df_clust <- get_df_clust(filter(df_reg, iso3c %!in% c("CHI", "SSD")), vvs)

dists_wna <- dist(df_clust)
dists_wona <- dist(na.omit(df_clust))

dist_options <- list(
    wna = dists_wna,
    wona = dists_wona)
    

## nrow(dists)
## ncol(dists)
## as.matrix(dists)[,2]
run_cluster(dist_options[["wna"]], "ward.D", 8, "wna")

clust_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
nbr_clusts <- seq(3,8)
na_rm <- c("wna","wona")
clust_cfg_df <- expand.grid(clust_method = clust_methods, nbr_clusts = nbr_clusts, na_rm = na_rm) %>% adt()

clust_res <- apply(clust_cfg_df, 1, \(x) run_cluster(dists = dist_options[[x[["na_rm"]]]],
                                                     method = x[["clust_method"]],
                                                     nbr_clusts = x[["nbr_clusts"]],
                                                     na_rm = x[["na_rm"]])) %>%
    rbindlist()


ggplot(clust_res, aes(x=skew, y = mean_nbr_crys, color = na_rm, size = max_nbr_crys)) +
    geom_point() +
    facet_wrap(~nbr_clusts)

## 8 clusters, wna, smallest skew

## clust_res[nbr_clusts == 8 &na_rm == "wna"]



world <- ne_countries(scale = "medium", returnclass = "sf") %>% atb() %>%
    select(iso3c = iso_a3, geometry)


assign_clusters <- function(df_clust, tree_cutted, na_rm) {
    #' assign the clustering solution to df_clust: IDK IF WORKS, not tested
    ## yeet NAs if necessary 
    if (na_rm == "wona") {
        df_clust2 <- na.omit(df_clust)
    } else {
        df_clust2 <- df_clust
    }
        
    df_clust2$clust <- tree_cutted
}



plot_world_clustered <- function(df_clustrd) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' plot visualization on map 


    world_clstrd <- left_join(world, 
                              select(df_clustrd, iso3c, cluster))
    
    plt_world_clstrd <- ggplot(world_clstrd, aes(geometry = geometry, fill = factor(cluster))) +
        geom_sf() +
        coord_sf(ylim = c(-55, 83)) +
        scale_fill_brewer(palette = "Set3")

    return(plt_world_clstrd)
}

## get_df_clust_lame(df_reg) %>% # , cutoffs = c(0, 0.4, 0.6, 0.8,1)) , cutoffs = c(0,0.5, 0.7, 0.82,1)
##     plot_world_clustered()


           
           


## ** cluster-based analysis 

## construct custom versions of base functions that by default yeet nas
mean_wo_na <- function(...) {
    mean(..., na.rm = T)}
median_wo_na <- function(...) {
    median(..., na.rm = T)}
sd_wo_na <- function(...) {
    sd(..., na.rm = T)}
sum_wo_na <- function(...) {
    sum(..., na.rm = T)}


sumrz_clusters <- function(df_reg_clstrd, mean_vrbls, sum_vrbls, sum_vrbls_pure) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' summarize the clustered df_reg
    #' calculate mean and sd of interval (mean) variables
    #' convert the sum_vrbls into rates per capita (per million capita)
    #' aggregate the sum_vrbls_pure into sums

    names(mean_vrbls) <- paste0(mean_vrbls, "_mean")

    ## vector funcs 
    vc_funcs <- c(mean_wo_na, sd_wo_na, median_wo_na)
    names(vc_funcs) <- c("mean", "sd", "median")

    ## use across() to process multiple interval variables: mean and sd (custom versions)
    df_reg_clstr_smrzd <- df_reg_clstrd %>%
        group_by(cluster, year) %>%
        summarize(across(all_of(unname(mean_vrbls)), .fns = vc_funcs, .names = "{.col}_{.fn}")) %>%
        adt()
    
    
    ## need to construct manual df to rename variables back to strings 
    melt_vrbl_fctrs_dt <- data.table(vrbl_label = mean_vrbls, variable = factor(seq(1:len(mean_vrbls))))

    ## multi-column melting; for some reason converts variable names to numeric factor
    # use patterns to melt several colmns
    clstr_melt_mean_sd <- melt(df_reg_clstr_smrzd, measure = patterns("mean$", "sd$", "median$"),
                               value.name = c("mean", "sd", "median")) %>%
        ## have to recode variable with update join
        .[melt_vrbl_fctrs_dt, variable := vrbl_label, on = "variable"] %>% 
        .[, `:=`(high = mean + sd, low = mean - sd, type = "intvl")] # calculate ribbons 

    ## process count variables: into rates per capita: first shape into long, then aggregate
    clstr_melt_rates <- df_reg_clstrd %>% select(iso3c, cluster, year, sum_vrbls) %>%
        pivot_longer(cols = c(sum_vrbls), names_to = "variable") %>%
        inner_join(select(df_reg_clstrd, iso3c, year, SP.POP.TOTLm)) %>%
        na.omit() %>% # yeet countries that miss either population or value to get good rates
        group_by(year, cluster, variable) %>%
        summarize(value = sum(value), SP.POP.TOTLm = sum(SP.POP.TOTLm)) %>% # sum by cluster
        mutate(rate = value/SP.POP.TOTLm) %>% # then calculate rates
        ## rename rate to mean to fit with clstr_melt_mean_sd, also assign rate to median to have option to use it
        select(year, cluster, variable, mean=rate, median = rate) %>% 
        mutate(type = "cnt")

    ## process sum_vrbls_pure variables: just aggregate without calculating rates
    clstr_melt_cnts <- df_reg_clstrd %>% select(iso3c, cluster, year, all_of(sum_vrbls_pure)) %>%
        pivot_longer(cols = c(sum_vrbls_pure), names_to = "variable") %>%
        group_by(year, cluster, variable) %>%
        summarize(value = sum(value, na.rm = T)) %>%
        select(year, cluster, variable, mean = value, median = value) %>%
        mutate(variable = paste0(variable, "_pure"), type = "cnt_pure")
        

    clstrd_melt_cbn <- bind_rows(clstr_melt_mean_sd, clstr_melt_rates, clstr_melt_cnts)

    

    return(clstrd_melt_cbn)
}


render_cluster_means <- function(df_reg, rates) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate and plot with rollmean_custom some cluster means/medians
    #' should be split up later into separate functions tho:
    #' - clustering
    #' - summarizing (kinda own function, but variable specification should be argument)
    #' - plotting

    if (!rates) {
        sum_vrbls <- c("nbr_opened", "clctr_cnt_cpaer", "cnt_contemp", "smorc_dollar_fxm", "NY.GDP.TTL",
                       "hnwi_nbr_30M")
        sum_vrbls_pure <- c("SP.POP.TOTLm", "nbr_opened", "hnwi_nbr_30M")
        mean_vrbls <- c("NY.GDP.PCAP.CD", "gptinc992j", "ghweal992j", "hdi", 
                        "tmitr_approx_linear20step")

    } else if (rates) {
        sum_vrbls <- c("nbr_opened", "NY.GDP.TTL")
        sum_vrbls_pure <- c("SP.POP.TOTL", "nbr_opened")
        mean_vrbls <- c("NY.GDP.PCAP.CD", "gptinc992j", "ghweal992j", "hdi", 
                        "tmitr_approx_linear20step", "clctr_cnt_cpaer", "cnt_contemp", "smorc_dollar_fxm",
                        "hnwi_nbr_30M")

    }
    
    
    ## slice_max(df_reg_rts, order_by = clctr_cnt_cpaer, n=50) %>% select(iso3c, year, clctr_cnt_cpaer) %>%
    ##     left_join(df_reg_clstrd %>% select(iso3c, cluster)) %>% adf()
    


    df_reg_clstrd <- get_df_clust_lame(df_reg) %>%
        select(iso3c, cluster) %>%
        inner_join(df_reg, .) %>%
        filter(year >= 1995) %>%
        mutate(NY.GDP.TTL = NY.GDP.PCAP.CD * SP.POP.TOTLm)
    
    clstr_melt_mean_sd <- sumrz_clusters(df_reg_clstrd, mean_vrbls, sum_vrbls, sum_vrbls_pure)

    ## add some manual labels -> should be made more elegant, also the distinction between counts (pure) and rates 
    
    cluster_sumry_addgns <- c(
        "hdi" = "Human Development Index", 
        "hnwi_nbr_30M_pure" = "HNWI with net worth 30M USD (count)",
        "nbr_opened_pure" = "private museums openings (count)",
        "NY.GDP.PCAP.CD" = "country-avg. GDP per cap.",
        "NY.GDP.TTL" = "individual-avg. GDP per cap.",
        "SP.POP.TOTL_pure" = "population")

    plt_clstr_means <- clstr_melt_mean_sd %>%
        group_by(cluster, variable) %>%
        arrange(cluster, variable, year) %>%
        mutate(mean_ra = rollmean_custom(mean, win_len = 6)) %>%
        ggplot(aes(x=year, y=mean_ra, color = factor(cluster), fill = factor(cluster))) +
        geom_line() +
        ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
        facet_wrap(~variable, scales = "free", labeller = as_labeller(
                                                   c(cluster_sumry_addgns, vvs$vrbl_lbls))) +
        theme(legend.position = c(0.7,0.08))

    pdf(paste0(FIG_DIR, "plt_clstr_means.pdf"), width = 12, height = 7)
    plot(plt_clstr_means)
    dev.off()
}

render_cluster_means(df_reg_rts, rates = T)


## ## compare difference between median and mean (for interval variables)
## clstr_melt_mean_sd[, .(cluster, year, variable, mean, median)] %>%
##     melt(measure.vars = c("mean", "median"), variable.name = "summary_type") %>%
##     ggplot(aes(x=year, y=value, color = factor(cluster), fill = factor(cluster), linetype = summary_type)) +
##     geom_line() +
##     ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
##     facet_wrap(~variable, scales = "free")


## ## basic spaghetti line plotting
## ggplot(df_reg_clstrd, aes(x=year, y=gptinc992j, group = iso3c, color = factor(cluster))) +
##     geom_line()


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

render_xtsum_plt2 <- function(cbn_dfsx, df_regx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate variability plots for variables and samples
    1
    4
    5
    65
    3
    12
    23
    
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



}

render_xtsum_plt2(cbn_dfs_rates_uscld, df_reg_rts)

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

v_nicely_fmt_number <- function(vlus) {
    sapply(vlus, nicely_fmt_number)
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


## ** kernel 

vrbls_to_log <- c("hnwi_nbr_1M", "hnwi_nbr_5M", "hnwi_nbr_30M", "hnwi_nbr_200M",
                  "smorc_dollar_fxm", "smorc_dollar_fxm_sqrd", "NY.GDP.PCAP.CDk",
                  "SP.POP.TOTLm", "clctr_cnt_cpaer", "nbr_opened_cum", "nbr_opened_cum_sqrd",
                  "cnt_contemp_1990", "cnt_contemp_1990_squared")

## test all kind of data transformations that make 
dtx_cbn2 <- dtx_cbn %>% copy() %>% 
    ## .[, z := scale_wo_attr(value), by = .(variable, cbn_name)] %>%
    ## .[z > - & z < 2 & value != 0] %>% ## yeet outliers
    .[variable %in% vrbls_to_log, value := log(value+0)]



dtx_cbn2[is.na(value), .N, variable]
dtx_cbn[is.na(value), .N, variable]



## raw kernels? 
dtx_cbn2 %>%
    ggplot(aes(x=value, y=..density.., group = interaction(variable, cbn_name), color = cbn_name)) +
    ## geom_histogram(color = "black", fill = "lightgrey", lwd = 0.2, bins = 30) +
    geom_density(position = "identity", show.legend = F) + 
    facet_wrap(~variable , scales = "free") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

## first calculate the kernel multipliers
krnl_mltplrs <- dtx_cbn %>% .[, .(hist_counts = hist(value, plot = F, breaks = 1)$counts[1],
                  hist_densty = hist(value, plot = F, breaks = 1)$density[1]), by=c("cbn_name", "variable")] %>%
    .[, mltplr := hist_counts/hist_densty] %>% 
    .[, .(cbn_name, variable, mltplr)]

## then calculate the kernels (with density), then scale them with joined-on multipliers
krnl_prep <- dtx_cbn %>%
    .[, .(krnl_x = density(value, na.rm = T, from = min(value, na.rm = T), to = max(value, na.rm = T))$x,
          vlu_krnl = density(value, na.rm = T, from = min(value, na.rm = T), to = max(value, na.rm = T))$y),
      by=c("cbn_name", "variable")]

krnl_mltpld <- krnl_prep %>% 
    krnl_mltplrs[on=.] %>%
    .[, vlu_mltpld := vlu_krnl * mltplr]


## original kernels
krnl_prep %>%
    ggplot(aes(x=krnl_x, y = vlu_krnl, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")



## kernels corresponding to counts (probably don't)
krnl_mltpld %>%
    ggplot(aes(x=krnl_x, y=vlu_mltpld, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")


cbn_dtn <- lapply(names(cbn_dfs)[1:3], \(x) list(cbn_name =x, cbn_n = nrow(cbn_dfs[[x]]))) %>% rbindlist()

krnl_n_scaled <- cbn_dtn[on=krnl_mltpld] %>%
    .[, vlu_n_scaled := vlu_mltpld/cbn_n]

## kernels corresponding to ratio (probably don't)
krnl_n_scaled %>%
    ggplot(aes(x=krnl_x, y=vlu_n_scaled, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")

## check whether kernel values add up properly across combinations -> THEY DON'T!!!!!
## neither original nor n-rescaled ones
krnl_prep[order(variable) ,sum(vlu_krnl), by=.(variable, cbn_name)] %>% adf()
krnl_n_scaled[order(variable) ,sum(vlu_n_scaled), by=.(variable, cbn_name)] %>% adf()


filter(krnl_prep, variable == "sptinc992j_p99p100") %>%
    ggplot(aes(x=krnl_x, y=vlu_krnl, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")


## gptinc992j

filter(dtx_mlt, variable == "hnwi_nbr_1M") %>% ungroup() %>% 
    ggplot(aes(x=value)) +
    ## geom_histogram(aes(y=..count..), color = "black", fill = "lightgrey") +
    geom_density(aes(y=..count..), bw = "nrd0", n=10, trim=T)

x <- stats::density(filter(dtx_mlt, variable == "hnwi_nbr_1M")$value, from = 0, bw = 0.1)
tv <- c(1e8, 2e8, 3e8, 1e7, 1e6)
tv <- c(1e3, 2e3, 3e3, 1e2, 1e1)
tv <- c(1e-2, 2e-2, 3e-2, 1e-3, 1e-4)
x <- stats::density(tv, from = 0)
plot(x[["x"]], x[["y"]], type = "l")
hist(tv, breaks = 100)



    geom_density((), color = "red")
    ## geom_histogram(aes(y=..density..), color = "black", fill = "lightgrey") +
    ## geom_density(aes(y=..density..), color = "red") 

myhist <- hist(mtcars$mpg)
multiplier <- myhist$counts / myhist$density
mydensity <- density(mtcars$mpg)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

ggplot(mtcars, aes(x=mpg, y=..count..)) +
    geom_histogram(bins=10) +
    geom_density()

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
    
    



    

    
