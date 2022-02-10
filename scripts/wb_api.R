
download_WB_data <- function(indx){
    #' downloads data with the WB API, can take single WB variable codes or vectors thereof
    print(paste0("now downloading: ", indx))
    indx_data <- wb_data(indicator = indx, start_date = STARTING_YEAR, end_date = ENDING_YEAR)
    return(indx_data[,c("iso3c", "country", "date", indx)])
}

## dfx <- download_WB_data(c("NY.GDP.PCAP.CD", "SI.POV.GINI"))

save_WB_data <- function(df) {
    #' saves WB data
    write.table(df, file = paste0(PROC_DATA_DIR, "WB_data_proc.csv"))
}

## save_WB_data(dfx)


read_WB_data <- function() {
    df_wb <- as_tibble(read.table(paste0(PROC_DATA_DIR, "WB_data_proc.csv")))
}

## df_wb <- read_WB_data()



get_WB_data <- function(indx, refresh_all=FALSE) {
    #' get WB variables, download all new if asked to do so, else read from file
    df_wb_local <- read_WB_data()
    wb_vars_there <- names(df_wb_local)[4:len(names(df_wb_local))]
    print(paste0("wb_vars_there: ", wb_vars_there))

    if (refresh_all) {
        print(paste0("download all indicators anew: ", indx))
        df_wb_api <- download_WB_data(indx)

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% indx)]
        
    } else {
        wb_vars_to_download <- indx[which(indx %!in% wb_vars_there)]
        print(length(wb_vars_to_download))
        print(paste0("download selected indicator(s) anew: ", wb_vars_to_download)) #
        
        if (length(wb_vars_to_download) > 0) {
            df_wb_api <- download_WB_data(wb_vars_to_download)
        } else {
            df_wb_api <- df_wb_local[,c("iso3c", "country", "date")]
        }

        unchanged_vars <- wb_vars_there[which(wb_vars_there %!in% wb_vars_to_download)]
        print(paste0("unchanged_vars: ", unchanged_vars))
        ## need to this out: how to get the variables which shouldn't be changed; think i got it now
    }

    df_wb_new <- as_tibble(merge(df_wb_local[,c("iso3c", "country", "date", unchanged_vars)], df_wb_api))
    save_WB_data(df_wb_new)

    return(df_wb_new[,c("iso3c", "country", "date", indx)])
    
}


## indx <- c("NY.GDP.PCAP.CD", "SI.POV.GINI", "NY.GDP.PCAP.KD.ZG", "SP.POP.TOTL")
## x <- get_WB_data(indx)


