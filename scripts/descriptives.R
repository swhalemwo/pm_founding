    library(psych)

## * generate the descriptives tables/figures for the update

### ** tax incentives

generate_taxinc_descriptives <- function() {

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

generate_taxinc_descriptives() 


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

get_mow_descriptives()
    
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

get_hnwi_descriptives()


