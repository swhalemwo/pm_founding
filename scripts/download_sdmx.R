library(OECD)
df_oecd <- get_datasets()

SDMX_DIR <- "/home/johannes/ownCloud/oecd/SDMX/"

'%!in%' <- function(x,y)!('%in%'(x,y))
len <- length

files_there <- list.files(SDMX_DIR)
files_there2 <- unlist(lapply(files_there, function(x) {substring(x,1, nchar(x)-4)}))


for (idx in df_oecd$id){

    if (idx %!in% files_there2) {

        sdmx_url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", idx)
        dest_file <- paste0(SDMX_DIR, idx, ".xml")
        print(sdmx_url)
        
        res <- tryCatch(
            download.file(sdmx_url, dest_file),
            error=function(e) e
        )
        #v# https://stackoverflow.com/questions/8093914/use-trycatch-skip-to-next-value-of-loop-upon-error
        
        Sys.sleep(5)
    } else {
        print(paste0("pass ", idx))
    
    }
}
