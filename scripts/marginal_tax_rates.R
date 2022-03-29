
## ** oecd
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

viz_lines(dfx=mtr_cpr, x="year", y="diff", time_level = "ra", duration = 1, grp="iso3c", facets = "region", max_lines = 8;)

summary(mtr_cpr$diff)
hist(mtr_cpr$diff, breaks = 30)
## mean of -3.25 -> EFW/Fraser thinks there are higher MRTs than OECD
## t-test?

ttest <- t.test(mtr_cpr$ObsValue, mtr_cpr$data_Top.marginal.income.tax.rate, paired = T, alternative = "two.sided")




## test_df <- as.data.frame(cbind(c(1,2,3, 1,2,3,1,2,3), c(4,5,6, 8,9,10, 3,4,5),
##                                c(rep("a",3), rep("b",3), rep("c", 3))))
## test_df$V1 <- as.numeric(test_df$V1)
## test_df$V2 <- as.numeric(test_df$V2)
## cor(test_df$V1, test_df$V2)
## ggplot(test_df, aes(x=V1, y=V2, color=V3)) +
##     geom_line()




## ** world bank
## only
wb_search("marginal")
## only has marginal corporate tax

wb_search("tax rate")$indicator_desc
## also only corporate taxes 


wb_search("income tax") %>%
    as.data.frame()


wb_search("tax rate")[1:8,]$indicator

## ** fraser institute/economic freedom of the world

FRASER_DIR <- paste0(PROJECT_DIR, "data/Fraser Institute/")
EFW_FILES <- list.files(FRASER_DIR, pattern = "csv")

extract_efw_data <- function(filename) {
    #' read in the EFW data, rename the data columns
    efw_df <- as_tibble(read.csv(paste0(FRASER_DIR, filename), skip=4))

    ## rel_cols <- c("Year", "ISO_Code", "marginal income tax rate")
    ## names(rel_cols) <- rel_cols

    ## all kinds of columns have "data"
    data_cols_pos <- which(unlist(lapply(names(efw_df), function(x) scramblematch("data.", x))))
    data_cols <- names(efw_df)[data_cols_pos]
    names(data_cols) <- data_cols
    
    data_cols_edited <- lapply(data_cols, function(x)
        list(orig_name = x,
             mod_name = paste0("data_", names(efw_df)[which(names(efw_df) == x)-1])))

    data_cols_edited <- unlist(lapply(data_cols, function(x)
        paste0("data_", names(efw_df)[which(names(efw_df) == x)-1])))
    
    names(efw_df)[data_cols_pos] <- data_cols_edited

    return(efw_df)
}
    

efw_dfs <- lapply(EFW_FILES, extract_efw_data)
efw_names <- unlist(lapply(efw_dfs, names))
table(efw_names)
## names are consistent across datafiles

efw_df <- as_tibble(Reduce(function(x,y) rbind(x,y), efw_dfs))


## efw_melt <- as_tibble(reshape2::melt(efw_df[,2:ncol(efw_df)], id=names(efw_df)[2:4]))
## sample(na.omit(efw_melt$value), 100)

## efw_melt$value[grepl("-", efw_melt$value)]

## filter(efw_melt, grepl("[^-]-[^-]", efw_melt$value)) %>%
##     select(variable) %>%
##     table() %>%
##     sort()

## ## hyphens in data_Top.marginal.income.tax.rate and data_Top.marginal.income.and.payroll.tax.rate

## filter(efw_melt, grepl("[^-]-[^-]", efw_melt$value))

## ## look for other weird values

## weird_values <- filter(efw_melt, grepl("[^0-9.]", efw_melt$value))$value
## weird_values <- gsub(",", "", weird_values) ## filtering out commas
## weird_values[is.na(as.numeric(weird_values))]
## ## seems I capture all stuff like this 


## differences don't seem super huge,
## think it's best to use larger one since indicator is about top

efw_clean_values <- function(values){
                                   #     
    print(values[1:10])
    values_clean <- gsub(",", "", values)
    splits <- strsplit(as.character(values_clean), split="-")
    
    splits_max <- as.numeric(unlist(lapply(splits, function(x) x[max(1,len(x))])))
    return(splits_max)

    
}

## efw_remove_hyphens(efw_df$data_Top.marginal.income.tax.rate)

## overwrite stuff, I don't like this but is fastest now 
efw_df[,5:ncol(efw_df)] <- lapply(efw_df[,5:ncol(efw_df)], efw_clean_values)
names(efw_df)[2:3] <- c("year", "iso3c")


## efw_df$data_Top.marginal.income.tax.rate

## cpltns_checker(efw_df[,c("data_Top.marginal.income.tax.rate
    
efw_df %>%
    select(iso3c=ISO_Code, year=Year, tmitr=data_Top.marginal.income.tax.rate) %>%
    cpltns_checker(varx="tmitr")

efw_df$region <- countrycode(efw_df$ISO_Code, "iso3c", "un.region.name")

filter(efw_df, Year >=1985) %>%
    select(iso3c=ISO_Code, year=Year, tmitr=data_Top.marginal.income.tax.rate, region) %>%
    viz_lines(x="year", y="tmitr", time_level = "ra", duration = 2, grp="iso3c", facets = "region", max_lines=8)

efw_base <- as_tibble(expand.grid(iso3c=unique(efw_df$ISO_Code), year = seq(1985, 2020)))
efw_fill_up <- as_tibble(merge(efw_base, efw_df, all.x = T))

## use na.rm=F to return leading NAs
efw_fill_up <- efw_fill_up %>%
    select(iso3c, year, data_Top.marginal.income.tax.rate) %>%
    group_by(iso3c) %>%
    mutate(tmirt_approx_linear = na.approx(data_Top.marginal.income.tax.rate, na.rm = F),
           tmirt_approx_step = na.locf(data_Top.marginal.income.tax.rate, na.rm = F))


cpltns_checker(efw_fill_up, varx="tmirt_approx_linear")
cpltns_checker(efw_fill_up, varx="tmirt_approx_step")
cpltns_checker(efw_fill_up, varx="data_Top.marginal.income.tax.rate")
filter(efw_fill_up, iso3c=="AGO")

table(is.na(filter(efw_fill_up, is.na(tmirt_approx_linear))$tmirt_approx_step))

filter(efw_fill_up, is.na(tmirt_approx_linear) & !is.na(tmirt_approx_step))
## step interpolation carries over last value -> has the values for 2020


## if I somehow predict tax incentives, I shouldn't use linearly imputed values for predictions, rather impute predicted values linearly afterwards

## would reeaaaaaaaaally like to run some PCA on this EFW data, but would need better regression methods
## also too data-driven 




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
