## **** testing wbstats

library(wbstats)

x <- wb_indicators(lang="en")

gdp_vars <- x[which(grepl("gdp", x$indicator, ignore.case = TRUE)),]
gdp_vars$indicator
gdp_vars$indicator_id

filter(gdp_vars, indicator_id == "NY.GDP.MKTP.CD")$indicator_desc

## try C1.12

indicatorx <- "C1.12"
indicatorx <- "C1.9"
indicatorx <- "1.0.HCount.1.90usd"
indicatorx <- "NY.GDP.MKTP.CD"

z <- wb_data(indicator=indicatorx, start_date=2015, end_date=2000)
y <- wb_data(indicator=indicatorx, mrv=10000)

table(z$date)
table(y$date)


## **** getting gdp to work

indicatorx <- "NY.GDP.PCAP.CD"

gdp_api <- wb_data(indicator = indicatorx, start_date = STARTING_YEAR, end_date = ENDING_YEAR)

df_gdp_pcap_molt$year <- as.numeric(as.character(df_gdp_pcap_molt$year))
df_gdp_pcap_molt <- filter(df_gdp_pcap_molt, year > 1984)

gdp_api_min <- gdp_api[,c("iso3c","country", "date", "NY.GDP.PCAP.CD")]
names(df_gdp_pcap_molt) <- c("country", "iso3c", "date", "gdp_pcap")

gdp_cpr <- as_tibble(merge(gdp_api_min, df_gdp_pcap_molt))
gdp_cpr$diff <- gdp_cpr$NY.GDP.PCAP.CD - gdp_cpr$gdp_pcap

table(filter(gdp_cpr, diff > 1000)$country)
table(filter(gdp_cpr, diff > 100)$country)
table(filter(gdp_cpr, diff > 1000)$date)

as.data.frame(filter(gdp_cpr, diff > 1000))

hist(gdp_cpr$diff, breaks = 1000)
mean(gdp_cpr$diff, na.rm=T)

nrow(filter(gdp_cpr, diff < 0.01))/nrow(filter(gdp_cpr, diff < 1e08))

png(paste0(FIG_DIR, "myanmar_twitter.png"), width=800, height=600)
filter(gdp_api, country %in% c("Myanmar", "United States", "Luxembourg")) %>%
    ggplot(aes(x=date, y=NY.GDP.PCAP.CD, group=country, color = country)) +
    geom_line(size=2) +
    theme(legend.text = element_text(size=13))
dev.off()

filter(df_gdp_pcap_molt, country %in% c("Myanmar", "United States", "Germany", "Luxembourg")) %>%
    ggplot(aes(x=date, y=gdp_pcap, group=country, color = country)) +
    geom_line()

## **** compare gini
df_wb_gini_molt$year <- as.numeric(as.character(df_wb_gini_molt$year))
filter(df_wb_gini_molt, year > 1984)

gini_cpr_prep <- df_wb_gini_molt
names(gini_cpr_prep) <- c("country", "iso3c", "date", "gini")

gini_cpr <- as_tibble(merge(x[,c("iso3c", "country", "date", "SI.POV.GINI")], gini_cpr_prep))
gini_cpr$diff <- gini_cpr$gini - gini_cpr$SI.POV.GINI

summary(gini_cpr$diff)
hist(gini_cpr$diff)

## gini didn't change at all

## compare population


df_wb_population_molt$year <- as.numeric(as.character(df_wb_population_molt$variable))
filter(df_wb_population_molt, year > 1984)

population_cpr_prep <- df_wb_population_molt[,c("V1", "V2", "year", "value")]
names(population_cpr_prep) <- c("country", "iso3c", "date", "population")

population_cpr <- as_tibble(merge(x[,c("iso3c", "country", "date", "SP.POP.TOTL")], population_cpr_prep))
population_cpr$diff <- population_cpr$population - population_cpr$SP.POP.TOTL

summary(population_cpr$diff)
hist(population_cpr$diff, breaks = 100)

as.data.frame(filter(population_cpr, diff > 10000))
