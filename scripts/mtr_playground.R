## * marginal tax rates playground

## ** comparison between EFW and OECD## ** oecd
filter_sdmx_results("marginal tax")
filter_sdmx_results("TABLE_I7_TAX")


filter_sdmx_results("tax rate")

datasets_already_there <- list.files(OECD_DATA_DIR)
options(timeout = 120)
download_oecd_dataset("TABLE_I7", "TOP_MRATE")
download_oecd_dataset("TABLE_I7", "PERS_ITAX")


## works
dfx <- get_dataset("TABLE_I7")
table(dfx$TAX)

oecd_mtr <- as_tibble(read.csv(paste0(OECD_DATA_DIR, "TABLE_I7")))

mtr_cpr <- as_tibble(merge(
    select(oecd_mtr, iso3c=COU, year=Time, ObsValue),
    select(efw_df, iso3c, year, data_Top.marginal.income.tax.rate)))

mtr_cpr$diff <- mtr_cpr$ObsValue - mtr_cpr$data_Top.marginal.income.tax.rate


mtr_cpr$region <- countrycode(mtr_cpr$iso3c, "iso3c", "un.region.name")

viz_lines(dfx=mtr_cpr, x="year", y="diff", time_level = "ra", duration = 1, grp="iso3c", facets = "region", max_lines = 8)

viz_lines(dfx=mtr_cpr, x="year", y="ObsValue", time_level = "ra", duration = 1, grp="iso3c", facets = "region", max_lines = 8)



summary(mtr_cpr$diff)
hist(mtr_cpr$diff, breaks = 30)
## mean of -3.25 -> EFW/Fraser thinks there are higher MRTs than OECD
## t-test?

ttest <- t.test(mtr_cpr$ObsValue, mtr_cpr$data_Top.marginal.income.tax.rate, paired = T, alternative = "two.sided")



## plotting the differences between OECD and EFW
cpr_melt <- as_tibble(reshape2::melt(mtr_cpr, id=c("iso3c", "year", "region")))
table(cpr_melt$variable)

cpr_melt_facets <- as_tibble(
    rbind(
        filter(cpr_melt, variable =="ObsValue") %>%
        create_facets(facets = "region", grp="iso3c", max_lines=4),
        filter(cpr_melt, variable =="data_Top.marginal.income.tax.rate") %>%
        create_facets(facets = "region", grp="iso3c", max_lines=4)))

cpr_melt_facets <- cpr_melt_facets %>%
    group_by(facetcol) %>%
    mutate(colr = as.character(as.numeric(factor(iso3c))))


    

ggplot(cpr_melt_facets, aes(x=year, y=value, color=colr, linetype=variable)) +
    geom_line() +
    facet_wrap(~facetcol) +
    scale_color_manual(values = colors_manual3)





## test_df <- as.data.frame(cbind(c(1,2,3, 1,2,3,1,2,3), c(4,5,6, 8,9,10, 3,4,5),
##                                c(rep("a",3), rep("b",3), rep("c", 3))))
## test_df$V1 <- as.numeric(test_df$V1)
## test_df$V2 <- as.numeric(test_df$V2)
## cor(test_df$V1, test_df$V2)
## ggplot(test_df, aes(x=V1, y=V2, color=V3)) +
##     geom_line()



## ** world bank search


## ** world bank
## only
wb_search("marginal")
## only has marginal corporate tax

wb_search("tax rate")$indicator_desc
## also only corporate taxes 


wb_search("income tax") %>%
    as.data.frame()


wb_search("tax rate")[1:8,]$indicator


## ** wid (uses margina tax rate in 2018 report)

## *** trying to find the series in WID, doesn't work


mtax_qry <- "select * from wid_v1 where variable like '%tax%'"

tax_df <- as_tibble(dbGetQuery(con, mtax_qry))

tax_df_fltrd <- filter(tax_df, iso3c %in% c("USA", "DEU") & year < 1950)

plotr <- function(vrbl) {
    dfx <- filter(tax_df_fltrd, variable == vrbl)
    
    plt <- ggplot(dfx, aes(x=year, y=value, color=iso3c)) +
        geom_line() +
        labs(title = vrbl)
    
    return(plt)
}

tax_vars <- unique(tax_df_fltrd$variable)
names(tax_vars) <- tax_vars

tax_plts <- lapply(tax_vars, plotr)

for (i in seq(1,len(tax_plts))) {
    plot(tax_plts[[i]])
    print(names(tax_plts)[i])
    readline("jj")
    }

## *** trying to calculate it
pretax_qry <- "select iso3c, year, variable, value, percentile from wid_v2 where varx='ptinc'"
pretax_df <- as_tibble(dbGetQuery(con, pretax_qry))
table(pretax_df$variable) %>% sort()

posttax_qry <- "select iso3c, year, variable, value, percentile from wid_v2 where varx='diinc'"
posttax_df <- as_tibble(dbGetQuery(con, posttax_qry))
table(posttax_df$variable) %>% sort()

posttax_df %>%
    filter(variable =="sdiinc992j", year >= 1995) %>%
    group_by(iso3c, year) %>%
    summarize(there=1) %>%
    cpltns_checker(varx="there")
