
## generate the descriptives tables/figures for the update

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
      file = paste0(TABLE_DIR, "tax_inc_descriptives.tex"),
      ## tabular.environment = 'longtable'
      )




