library(tibble)
library(OECD)

options(show.error.messages = TRUE)
options(show.error.locations = TRUE)


'%!in%' <- function(x,y)!('%in%'(x,y))
sdmx_res_fltrd <- as_tibble(read.csv("/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv"))

OECD_DATA_DIR <- "/home/johannes/ownCloud/oecd/api_data/"

download_oecd_df <- function(datasetx, filter_list){
    #' actual downloading 
    dfx <- get_dataset(datasetx, filter=filter_list)
    write.csv(dfx, paste0(OECD_DATA_DIR, datasetx))
    print("success")
    done <- TRUE
    }


datasets_already_there <- list.files(OECD_DATA_DIR)


options(timeout = 1000)

download_oecd_dataset <- function(datasetx, idx) {
    print(paste0("dataset: ", datasetx, " id: ", idx))
    if (datasetx %!in% datasets_already_there){

        done <- FALSE
        filter_list <- list(c(idx))

        while (done==FALSE) {
            res <- tryCatch({
                print(paste0(filter_list, collapse = "-"))
                download_oecd_df(datasetx, filter_list)
                done <- TRUE
            },
            error=function(e) {
                print(paste0("error: ", length(filter_list)))
                filter_list <- rev(append(rev(filter_list), ""))
            })
            filter_list <- res
            Sys.sleep(2)
            
            if (length(filter_list) == 20) {
                done <- TRUE
                print("quit after too many tries")
            }
                
        }
    }
}      

print(sdmx_res_fltrd)


## download_oecd_dataset("BATIS_EBOPS2002", "S287")

apply(sdmx_res_fltrd, 1, function(x) download_oecd_dataset(x["sdmx_id"], x["id"]))


