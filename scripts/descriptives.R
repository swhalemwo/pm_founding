
## generate the descriptives tables/figures for the update

generate_taxinc_descriptives <- function() {

    df_taxinc <- read_in_tax_incentives()
    library(psych)

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
