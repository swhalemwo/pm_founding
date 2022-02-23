
TAX_INCENTIVES_DIR <- paste0(PROJECT_DIR, "data/tax_incentives/")

## ** CAF rules to give by
df_rules <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "rules_to_give_by.csv")))
df_rules$iso3c <- countrycode(df_rules$country, "country.name", "iso3c")
## huh that went well 

## ** CAF world giving index
df_give <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "caf_world_giving_index.csv")))

## df_give$iso3c <- countrycode(df_give$Country, "country.name", "iso3c")
## df_give$wbcc <- countrycode(df_give$Country, "country.name", "wb")
## table(df_give$iso3c == df_give$wbcc)
## wb and iso3c give the same everywhere except for Kosovo, which wb recognizes -> just use "wb" as output and pretend it's iso3c

## i think i'm doing that anyways
## countrycode(unique(df_anls$iso3c), "iso3c", "wb") == unique(df_anls$iso3c)
## well kinda: the codes that I manually added didn't include neither Kosovo nor Channel Islands, so for those ISO and WB are the same, but WB df did probably use WB codes

df_give$iso3c <- countrycode(df_give$Country, "country.name", "wb")


## ** Hudson
df_hudson <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "hudson.csv")))
df_hudson$iso3c <- countrycode(df_hudson$Country, "country.name", "iso3c")



