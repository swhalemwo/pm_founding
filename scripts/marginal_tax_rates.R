
## ** oecd
filter_sdmx_results("marginal tax")

filter_sdmx_results("tax rate")

datasets_already_there <- list.files(OECD_DATA_DIR)
options(timeout = 120)
download_oecd_dataset("TABLE_I7", "TOP_MRATE")

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
    

efw_dfs <- lapply(efw_files, extract_efw_data)

efw_names <- unlist(lapply(efw_dfs, names))
table(efw_names)
## names are consistent across datafiles

locate_col(fraser_df, "marginal income tax")


fraser_df[,col_nbr]
fraser_df[,1]


    




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
