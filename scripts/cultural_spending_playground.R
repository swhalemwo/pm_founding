## * cultural_spending_playground

## ** compare OECD and WID exchange rates, when using market exchange rates (xlcusx999i) for WID they basically completely match

## use PPP rates tho LOL

## OECD
## oecd_fx_df <- filter_sdmx_results("exchange rates")
## unique(oecd_fx_df$description.en)

## oecd_str <- "Real effective exchange rates"
## oecd_str <- "Foreign effective weighted exchange rates"
## oecd_str <- "EXCH"
## oecd_str <- "ppp"

## filter(oecd_fx_df, description.en == oecd_str)

## download_oecd_dataset("CSPCUBE", "INTLCOMP_T1")
## download_oecd_dataset("CSP2012", "INTLCOMP_T1")
## download_oecd_dataset("CSP2012", "INTLCOMP")
## ## downloads not working properly 
## ## EO97_OUTLOOK97: economic outlook 2015
## ## tbh would be interesting to see the impact of predictions

## maybe just use the website?
## https://data.oecd.org/conversion/exchange-rates.htm

oecd_cur_df <- as_tibble(read.csv(paste0(PROJECT_DIR, "data/OECD/fx_rates.csv")))
oecd_cur_df <- select(oecd_cur_df, iso3c=LOCATION, year = TIME, oecd_fx = Value)
 
## comparison

cur_df_cpr <- as_tibble(merge(
    wid_currency_df,
    oecd_cur_df))
   
cor(cur_df_cpr$xlcusx999i, cur_df_cpr$oecd_fx)

cur_df_sum <- group_by(cur_df_cpr, iso3c) %>%
    summarize(corx= cor(xlcusx999i, oecd_fx))

hist(cur_df_sum$corx, breaks = 20)

filter(cur_df_sum, corx > 0.9) 
filter(cur_df_sum, corx < 0.7)
## maybe can use same UN data source to get GDP? and

cur_df_cpr$region <- countrycode(cur_df_cpr$iso3c, "iso3c", "un.region.name")
cur_df_cpr <- create_facets(cur_df_cpr, "region", "iso3c", 5)

pdf(paste0(FIG_DIR, "fx_comparison_WID_OECD.pdf"), width = 17, height = 10)
cur_df_cpr %>%
    group_by(iso3c) %>%
    mutate(corx = cor(xlcusx999i, oecd_fx)) %>%
    ## filter(corx < 0.8) %>%
    pivot_longer(cols=c("xlcusx999i", "oecd_fx")) %>%
    ggplot(aes(x=year, y=value, color = iso3c, linetype = name)) +
    facet_wrap(~iso3c, scales = "free") +
    geom_line()
dev.off()
