## * MOW playground
## ** some visualizations

df_mow <- as_tibble(read.csv(paste0(MOW_DIR, "mow.csv")))

df_mow$iso3c <- countrycode(df_mow$country, "country.name", "iso3c")

mow_type <- as_tibble(read.csv(paste0(MOW_DIR, "type.csv")))
mow_clsfcn <- as_tibble(read.csv(paste0(MOW_DIR, "classification.csv")))

mow_fndgs <- as_tibble(merge(filter(df_mow[,c("idx", "name", "founding_date1", "iso3c")], founding_date1 > 1900), mow_type, by=c("idx")))

mow_fndgs$cnt <- 1

viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 8, grp = "type", extra = FALSE)





## check that viz_lines can also be used to aggregate dataframe
y <- viz_lines(mow_fndgs, x="founding_date1", y="cnt", time_level = "ra", duration = 1, grp = "iso3c", extra = FALSE, fill_up = T)

y <- viz_lines(na.omit(mow_fndgs[,c("founding_date1", "cnt", "iso3c")]), x="founding_date1", y="cnt", time_level = "ra", duration = 1, grp = "iso3c", extra = FALSE, fill_up = T)


y2 <- fill_up2(na.omit(mow_fndgs[,c("founding_date1", "cnt", "iso3c")]), x="founding_date1", y="cnt", grp="iso3c")

y2 <- fill_up2(mow_fndgs, x="founding_date1", y="cnt", grp="iso3c")

head(filter(y, grp == "DEU"))

filter(mow_fndgs, is.na(iso3c))
       

viz_lines(filter(mow_fndgs, founding_date1 > 1985), x="founding_date1", y="cnt", time_level = "ra", duration = 3, grp = "iso3c", fill_up = T)

x <- as_tibble(aggregate(cnt ~ founding_date1 + iso3c, mow_fndgs, sum))
head(filter(x, iso3c == "DEU"),8)
## ** more cleaning/completeness checks
## *** country


country_tbl <- table(df_mow$country)

country_tbl[rev(order(country_tbl))[1:10]]
## 63 museums have no country 
## -> looks good


## *** city
city_tbl <- table(df_mow$city)

city_tbl[rev(order(city_tbl))[1:10]]
## around 4k have no city 

## *** some manual check
as.data.frame(filter(mow_fndgs,
                     country == "Netherlands" & founding_date1 >= 1970 &
                     founding_date1 < 1980 & type == "Art Museum")[,c("name", "founding_date1")])


## *** some basic look at contemporary art museums

## around 2k museums, 1250 have founding date

mow_cpaer <- as_tibble(merge(filter(mow_clsfcn, clsfcn == "Art, Modern and Contemporary"), df_mow))
mow_cpaer$cnt <- 1

p1 <- viz_lines(filter(mow_cpaer, founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, return="plot")
p2 <- viz_lines(filter(mow_cpaer, founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, extra = "cum_rate", return="plot")
p3 <- viz_lines(filter(mow_cpaer[mow_cpaer$country %!in% c("United States of America", "Germany"),], founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, max_lines = 8, extra = "cum_rate", return="plot")
p4 <- viz_lines(filter(mow_cpaer[mow_cpaer$country %!in% c("United States of America", "Germany"),], founding_date1 >= 1900), x = "founding_date1", y="cnt", time_level = "ra", duration = 10, grp = "country", fill_up = T, max_lines = 8, return = "plot")

pdf(paste0(FIG_DIR, "mow_contemporary.pdf"), width = 14, height = 8)
grid.arrange(p1,p4,p2,p3, ncol=2, nrow=2)
dev.off()


## hmm canada so early huh
## italy has actually a lot of founding going on until the end.. does it mean database is maybe less incomplete than I thought? 

## *** keyword co-occurence
kwds_w_cpaer <- as_tibble(merge(mow_clsfcn, mow_cpaer[,c("idx")]))
tbl_kwds <- table(kwds_w_cpaer$clsfcn)
as.data.frame(tbl_kwds[order(tbl_kwds)])

## Decorative and Applied Arts   23
##                Art, Russian   27
##                 Photography   28
##                   Sculpture   38
## History, Local and Regional   45
##                         Art   62
## Art, Modern and Contemporary 2035





