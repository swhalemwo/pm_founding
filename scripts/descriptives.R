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

get_df_clust_lame(df_reg) %>% # , cutoffs = c(0, 0.4, 0.6, 0.8,1)) , cutoffs = c(0,0.5, 0.7, 0.82,1)
    plot_world_clustered()

df_reg_clstrd <- get_df_clust_lame(df_reg) %>%
    select(iso3c, cluster) %>%
    inner_join(df_reg, .) %>%
    filter(year >= 1995) %>%
    mutate(NY.GDP.TTL = NY.GDP.PCAP.CD * SP.POP.TOTLm)

           
           


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



sum_vrbls <- c("nbr_opened", "clctr_cnt_cpaer", "cnt_contemp", "smorc_dollar_fxm", "NY.GDP.TTL", "hnwi_nbr_30M")
sum_vrbls_pure <- c("SP.POP.TOTL", "nbr_opened", "hnwi_nbr_30M")
mean_vrbls <- c("NY.GDP.PCAP.CD", "gptinc992j", "ghweal992j", "hdi", 
                "tmitr_approx_linear20step")
clstr_melt_mean_sd <- sumrz_clusters(df_reg_clstrd, mean_vrbls, sum_vrbls, sum_vrbls_pure)
ggplot(clstr_melt_mean_sd, aes(x=year, y=mean, color = factor(cluster), fill = factor(cluster))) +
    geom_line() +
    ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
    facet_wrap(~variable, scales = "free")

## test rollmean_custom again
clstr_melt_mean_sd %>%
    group_by(cluster, variable) %>%
    arrange(cluster, variable, year) %>%
    mutate(mean_ra = rollmean_custom(mean, win_len = 6)) %>%
    ggplot(aes(x=year, y=mean_ra, color = factor(cluster), fill = factor(cluster))) +
    geom_line() +
    ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
    facet_wrap(~variable, scales = "free")



## compare difference between median and mean (for interval variables)
clstr_melt_mean_sd[, .(cluster, year, variable, mean, median)] %>%
    melt(measure.vars = c("mean", "median"), variable.name = "summary_type") %>%
    ggplot(aes(x=year, y=value, color = factor(cluster), fill = factor(cluster), linetype = summary_type)) +
    geom_line() +
    ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
    facet_wrap(~variable, scales = "free")



## basic spaghetti line plotting
ggplot(df_reg_clstrd, aes(x=year, y=gptinc992j, group = iso3c, color = factor(cluster))) +
    geom_line()


## ** xtsum based descriptives

## finding out the weird internal workings of xtsum
x <- xtsum(df_reg, nbr_opened, iso3c)

## xtsum_dt <- data.table(id = c(1,1,2,2,3,3), score1 = c(70,70,60,80,90,50), score2 = c(70,70,70,90,90,30))
## xtsum(xtsum_dt, score1, id)
## xtsum(xtsum_dt, score2, id)

## ## calculate within mean 
## xtsum_dt[, xbar := mean(score2), by = id]
## xtsum_dt2 <- xtsum_dt[, .(id, score2, xbar, s_within_p1 = score2 - xbar)]

## xtsum_dt2[, sd(s_within_p1 + mean(score2))]
## xtsum_dt2[, .(s_within_p1, s_within_p1^2)] %>% .[, sqrt(sum(V2)/5)]


xtsum(df_reg, nbr_opened, iso3c) %>% adt()

lapply(c(vvs$all_rel_vars, "nbr_opened"), \(x)
       xtsum(df_reg, get(x), iso3c) %>% adt() %>% 
       list(name = x,
            overall = .[["sd"]][[1]],
            bewteen = .[["sd"]][[2]],
            within = .[["sd"]][[3]]))
    
## stupidly inefficiently calculating xtsum for every number separately since apparently I can't pipe properly
## -> move into separate function
xtsum_res <- mclapply(c(vvs$all_rel_vars, "nbr_opened"), \(x)
                      list(name = x,
                           sds = xtsum(df_reg, get(x), iso3c)$sd) %>%

                           
                           overall = xtsum(df_reg, get(x), iso3c)$sd[[1]],
                           between = xtsum(df_reg, get(x), iso3c)$sd[[2]],
                           within = xtsum(df_reg, get(x), iso3c)$sd[[3]]),
                      mc.cores = 6) %>%
    rbindlist()

xtsum_res2 <- xtsum_res %>% copy() %>%
    .[, `:=`(between_prop = between/overall, within_prop = within/overall,
             within_between_ratio = within/between)]
             
lbl_dt <- data.table(vrbl = names(vvs$vrbl_lbls), vrbl_name = vvs$vrbl_lbls)

xtsum_res2[lbl_dt, name := vrbl_name, on = .(name = vrbl)]

ggplot(xtsum_res2, aes(x=within_between_ratio, y = name)) +
    geom_bar(stat="identity")

ggplot(xtsum_res2, aes(x=within_prop, y=between_prop, color = within_between_ratio)) +
    geom_point() +
    geom_text_repel(aes(label = name), ) +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = 0.5) +
    scale_color_gradient2(low = "blue", mid = "darkgray", high = "red", midpoint = 1) +
    xlim(c(min(xtsum_res2$within_prop)-0.2,max(xtsum_res2$within_prop))) +
    ylim(c(min(xtsum_res2$between_prop), max(xtsum_res2$between_prop +0.03)))
