
TAX_INCENTIVES_DIR <- paste0(PROJECT_DIR, "data/tax_incentives/")

## ** CAF rules to give by
df_rules <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "rules_to_give_by.csv")))
df_rules$iso3c <- countrycode(df_rules$country, "country.name", "iso3c")

