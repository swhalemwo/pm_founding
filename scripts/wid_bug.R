library(tidyverse)
library(data.table)
library(gridExtra)


BUG_DIR <- "/home/johannes/ownCloud/wid/wid_bug/"
## directory with the complete country files from https://wid.world/data/


country_dfs <- lapply(c("ES", "DE", "US", "NL", "DK"), function(x) as_tibble(read.csv(paste0(BUG_DIR, "WID_data_", x, ".csv"), sep = ";")))
country_df <- as_tibble(Reduce(function(x,y,...) rbind(x,y), country_dfs))
df_thweal <- filter(country_df, variable == "thweal992j")

df_thweal$percentile_low <- as.numeric(unlist(lapply(strsplit(df_thweal$percentile, split='p'), function(x) x[2])))

result <- df_thweal %>%
    group_by(country, percentile_low,year) %>%
    summarise(nbr_vlus = length(unique(value))) %>%
    group_by(country) %>%
    summarise(nbr_values_country = max(nbr_vlus))

print(result)
## only Spain has more than 1 value for lower percentile of threshold



## visual check

p1 <- ggplot(filter(df_thweal, country=="ES" & year == 1995), aes(x=percentile_low, y=log10(value))) +
    geom_point()
## -> it seems as if two time series are included in the case of Spain


## all years of Spain
p2 <- ggplot(filter(df_thweal, country=="ES"), aes(x=percentile_low, y=log10(value), color=factor(year))) +
    geom_line() +
    geom_point()


## comparison with other countries, use 1995 as example
p3 <- ggplot(filter(df_thweal, year==1995), aes(x=percentile_low, y=log10(value), color=country)) +
    geom_line()



pdf(paste0(BUG_DIR, "plots.pdf"), width = 8, height = 12)
grid.arrange(p1,p2,p3)
dev.off()
