df_taxinc <- read_in_tax_incentives()

pdf(paste0(FIG_DIR, "tax_incentive_cors.pdf"), width = 21, height = 12)
chart.Correlation(df_taxinc[,tax_vars_all], pch=21)
dev.off()


df_taxinc$subregion <- countrycode(df_taxinc$iso3c, "iso3c", "un.regionsub.name")
ps.caf <- pca_viz(df=df_taxinc, vars = tax_vars_caf, title = "CAF",  color_col = "subregion")
ps.all <- pca_viz(df=df_taxinc, vars = tax_vars_all, title = "All",  color_col = "subregion")



pdf(paste0(FIG_DIR, "pca_cprn.pdf"), width=14, height=14)
grid.arrange(grobs = c(ps.caf, ps.all), ncol=2, as.table=FALSE)
dev.off()

## can use pca_viz to get the individual plots (isn't really unix principle but comfy)
p.ind.all <- pca_viz(df=df_taxinc, vars = tax_vars_all, title = "All", label="country",  color_col = "subregion")[[4]]
p.ind.caf <- pca_viz(df=df_taxinc, vars = tax_vars_caf, title = "CAF", label="country", color_col = "subregion")[[4]]

pdf(paste0(FIG_DIR, "pca_ind_all.pdf"), width = 18, height = 10)
p.ind.all
p.ind.caf
dev.off()


## checking the fits directly
ps.all2 <- pca_viz(pca.res = res.pca.all, title = "all2")
grid.arrange(grobs = ps.all2)


## ** comparing correlations between different PCA solutions


pca_df_caf <- create_pca_df(df=df_taxinc, pca.res = res.pca.caf, id.col = "iso3c", rename_cols = "caf")
pca_df_all <- create_pca_df(df=df_taxinc, pca.res = res.pca.all, id.col = "iso3c", rename_cols = "all")


pca_cpr_df <- as_tibble(merge(pca_df_all, pca_df_caf, by="iso3c"))
chart.Correlation(pca_cpr_df[,2:ncol(pca_cpr_df)])


## compare how factors scores correlate depending on whether Hudson variables are included
## factor 2 no change really (r=0.97), but factor 1 "only" has 0.69 (nice) correlation
## think that's good enough tho to justify using CAF scores generally, if necessary


## ** checking pca scores 
res.pca.cbn <- get_taxinc_dfs()

ggplot(res.pca.cbn, aes(x=PC1_all, y=PC2_all)) +
    geom_point() +
    geom_label_repel(aes(label=iso3c))


