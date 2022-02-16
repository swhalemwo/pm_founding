

## * oecd
## ** library access, doesn't have labels tho 
library(OECD)
library(rsdmx)

SDMX_DIR <- "/home/johannes/ownCloud/oecd/SDMX/"
SDMX_TBL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_based_tables/"

SDMX_FAIL_DIR <- "/home/johannes/ownCloud/oecd/sdmx_parsing_fail"
SDMX_FAIL_FILE <- paste0(SDMX_FAIL_DIR, "/fails.csv")
OECD_DATA_DIR <- "/home/johannes/ownCloud/oecd/api_data/"


## ** sdmx parsing proc



proc_codelist <- function(dsd, codelistx, sdmx_id) {
    #' convert sdmx codelist into df, outsourced to own function for better trycatching
    codelist_df <- as.data.frame(slot(dsd, "codelists"), codelistId = codelistx)
    names_codelist_df <- names(codelist_df)
    codelist_df$codelist <- codelistx
    codelist_df$sdmx_id <- sdmx_id
    ## reorder the columns so that I can easier grep/awk the databases/codelists where culture terms occur
    codelist_df <- codelist_df[,c("sdmx_id", "codelist", names_codelist_df)]

    filename <- paste0(SDMX_TBL_DIR, sdmx_id, "---", codelistx, ".csv")
    write.csv(codelist_df, filename)
}


proc_sdmx_file <- function(sdmx_file){
    #' process an sdmx file: print all the codelists to file
    sdmx_id <- substr(sdmx_file, 1, nchar(sdmx_file)-4)
    dsd <- readSDMX(paste0(SDMX_DIR, sdmx_file), isURL = FALSE)
    ## print(slotNames(dsd)) overall slotnames are the same
    cls <- slot(dsd, "codelists")
    codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
    ## print(codelists) ## not every SDMX has the same codelist, unsurprisingly

    for (codelistx in codelists){
        print(codelistx)
        res <- tryCatch(
            proc_codelist(dsd, codelistx, sdmx_id),
            error=function(e) {
                ## FAILED_LIST <- c(failed_list, c(sdmx_id, codelistx))
                ## print(c(failed_list, c(sdmx_id, codelistx)))

                ## save which files failed to parse 
                write.table(paste(sdmx_id, codelistx, sep=","), file=SDMX_FAIL_FILE, append=TRUE,
                            col.names = FALSE, row.names = FALSE, quote = FALSE)
                
                }
        )
    }
}

## system(paste0("cd ", SCRIPT_DIR, " && Rscript download_sdmx.R"))

## files_there <- list.files(SDMX_DIR)
## mclapply(files_there, proc_sdmx_file, mc.cores = 6)

## proc_sdmx_file("WSECTOR.xml")


## ** working with parsing results
## get files with grepping in /home/johannes/ownCloud/oecd/sdmx_based_tables


find_sdmx_tables <- function(term) {
    #' grep parsed sdmx tables for terms, return all that fit it
    cmd <- paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv '", term, "'")
    tables <- system(cmd, intern = TRUE)
    return(tables)
}

## find_tables ("cultural services")

filter_sdmx_results <- function(sdmx_terms) {
    #' search the sdmx results, extract indicators matching to sdmx_terms 
    relevant_sdmx_tables <- unique(unlist(lapply(sdmx_terms, find_sdmx_tables)))

    sdmx_res_tbls <- lapply(relevant_sdmx_tables, function(x) as_tibble(read.csv(paste0(SDMX_TBL_DIR, x))))

    sdmx_res_tbl_names <- unlist(lapply(sdmx_res_tbls, names))
    table(sdmx_res_tbl_names)

    ## find main columns to focus on
    col_names <- c("sdmx_id", "codelist", "id", "label.en", "description.en")

    sdmx_res_cbn <- as_tibble(Reduce(function(x,y, ...) rbind(x[,col_names],y[,col_names]),sdmx_res_tbls))

    ## grepping multiple columns works best with apply, lapply on names(df) doesn't work properly for some reason 
    ## seems using pipe I can use multiple terms with pipe 
    sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl(" cultural|museum|cultural services", x)))>0),]

    return(sdmx_res_fltrd)

}

## system(paste0("cd ", SCRIPT_DIR, " && Rscript download_sdmx.R"))

## files_there <- list.files(SDMX_DIR)
## mclapply(files_there, proc_sdmx_file, mc.cores = 6)


## sdmx_terms <- c("museum", "cultural services", " cultural")
## sdmx_res_fltrd <- filter_sdmx_results(sdmx_terms)

## write to file to read from separate download_oecd.R, so that it can be read in by download_oecd.R
## actually by now could just import the funcs (filter_sdmx_results) in download_oecd.R to generate the table there
## write.csv(sdmx_res_fltrd, "/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv")


## sdmx_res_fltrd$id

## sdmx_res_fltrd[28,]
## hello "D90T92" my old friend
## let's see how to query you


## *** functionalized download 




download_oecd_df <- function(datasetx, filter_list){
    #' actual downloading 
    dfx <- get_dataset(datasetx, filter=filter_list)
    write.csv(dfx, paste0(OECD_DATA_DIR, datasetx))
    print("success")
    done <- TRUE
    }



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
                ## adding/removing empty filter strings to get everything,
                ## structure of dataset differs -> filter has to be expanded until dataset structure is matched
                
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



## this code is also run in download_oecd.R, not sure if I should have it here too
## could just call it with:
## system(paste0("cd ", SCRIPT_DIR, " && Rscript download_oecd.R"))
## datasets_already_there <- list.files(OECD_DATA_DIR)
## options(timeout = 10)
## apply(sdmx_res_fltrd, 1, function(x) download_oecd_dataset(x["sdmx_id"], x["id"]))



## ** processing actual data

## first get those that sufficient country coverage


create_oecd_df_list <- function() {
    #' make all the oecd dfs into named list for nicer access
    oecd_dfs <- list()
    for (i in datasets_already_there){
        dfx <-  as_tibble(read.csv(paste0(OECD_DATA_DIR, i)))
        oecd_dfs[[i]] <- dfx
    }
    return(oecd_dfs)
}


## oecd_dfs <- create_oecd_df_list()
## find data set column sets 
check_oecd_data_sets <- function(oecd_dfs, datasets_already_there) {
    #' check whether datasets have a country column that is one of c("LOCATION", "COU", "COUNTRY")
    #' if not, put them in list, check/update that manually 

    ## figuring out what are the location unit columns
    ## namesx <- unlist(lapply(datasets_already_there, function(x) names(read.csv(paste0(OECD_DATA_DIR, x)))))
    ## namesx_tbl <- table(namesx)
    ## namesx_tbl[order(namesx_tbl)]

    ## country col is either: LOCATION (23), COU (12), COUNTRY(3)
    ## exceptions (so far): "BIMTS_CPA","TEC5_REV4"  ,"TEC6_REV4" ,"TEC9_REV4"
    ## find exceptions automatically where there's no column named "LOCATION", "COU", "COUNTRY"

    namesx_exceptions <- c()

    for (i in datasets_already_there){
        ## dfx <- read.csv(paste0(OECD_DATA_DIR, i))
        dfx <- oecd_dfs[[i]]
        
        if (length(intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))) == 0) {
            namesx_exceptions <- c(namesx_exceptions, i)
        }
    }
    ## check manually (should check whenever I change the datasets to be included)

    ## first batch 
    ## - BIMTS_CPA: Balanced International Merchandise Trade Statistics: only from 2007
    ##   also Olav doesn't believe in diffusion anyways
    ## - TEC5_REV4: Trade by enterprise characteristics: only costa rica, and no values -> scrap
    ## - TEC6_REV: only Turkey and Israel 
    ## - TEC9_REV4: more Trade by enterprise characteristics: but only Czech Republic and Luxembourg
    ## could be that the same measure is scattered across different databases,
    ## but if it is scattered it's probablyl not the same stuff to begin with 

    ## second batch 
    ## "CRS1" ## about agriculture anyways
    ## "CRS1_GREQ" ## about agriculture anyways
    ## "DV_DCD_GENDER" ## about agriculture anyways
    ## "DV_DCD_PPFD" ## about agriculture anyways 
    ## "MULTISYSTEM" ## about agriculture anyways 
    ## "RIOMARKERS"  ## about agriculture anyways
    ## "SOCX_DET" ## only 38 rows, seems all about spain 
    ## "TSEC1" ## not clear what it is, but only 134 rows
    ## "TSEC1_COPY" ## also unclear, but also only 134 rows

    checked_exceptions <- c("BIMTS_CPA","TEC5_REV4","TEC6_REV4","TEC9_REV4","CRS1","CRS1_GREQ","DV_DCD_GENDER","DV_DCD_PPFD","MULTISYSTEM","RIOMARKERS","SOCX_DET","TSEC1","TSEC1_COPY")

    ## lapply(setdiff(namesx_exceptions, checked_exceptions), print)

    datasets_to_check_manually <- setdiff(namesx_exceptions, checked_exceptions)
    print(datasets_to_check_manually)


    if (len(datasets_to_check_manually) == 0) {
        print("no unchecked datasets")
    } else if (len(datasets_to_check_manually) > 0) {
        warning("there are some datasets that don't work but have not been manually checked")
    }
}




vague_cvrg <- function(namex){
    dfx <- as_tibble(read.csv(paste0(OECD_DATA_DIR, namex)))
    country_col <- intersect(names(dfx), c("LOCATION", "COU", "COUNTRY"))
    country_nbr <- nrow(unique(dfx[,country_col]))
    year_min <- min(dfx$Time)
    year_max <- max(dfx$Time)
    time_cvrg <- year_max-year_min
    return(list(
        namex = namex,
        country_nbr = country_nbr,
        year_min = year_min,
        year_max = year_max,
        time_cvrg=time_cvrg))
}

## vague_cvrg(datasets_already_there[1])

filter_vague_crvg <- function() {

    vague_cvrg_res <- lapply(setdiff(datasets_already_there, namesx_exceptions), vague_cvrg)

    vague_res_df <- as_tibble(rbindlist(vague_cvrg_res))
    ## as.data.frame(vague_res_df)

    return(vague_res_df)
}


## don't put downloading oecd code into an overall function, will take forever, functions should run selectively/once

RE_RUN_OECD_DOWNLOADS <- FALSE

if (RE_RUN_OECD_DOWNLOADS) {

    system(paste0("cd ", SCRIPT_DIR, " && Rscript download_sdmx.R"))

    files_there <- list.files(SDMX_DIR)
    mclapply(files_there, proc_sdmx_file, mc.cores = 6)


    sdmx_terms <- c("museum", "cultural services", " cultural")
    sdmx_res_fltrd <- filter_sdmx_results(sdmx_terms)

    ## write to file to read from separate download_oecd.R, so that it can be read in by download_oecd.R
    ## actually by now could just import the funcs (filter_sdmx_results) in download_oecd.R to generate the table there
    write.csv(sdmx_res_fltrd, "/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv")


    ## this code is also run in download_oecd.R, not sure if I should have it here too
    ## could just call it with:
    ## system(paste0("cd ", SCRIPT_DIR, " && Rscript download_oecd.R"))
    datasets_already_there <- list.files(OECD_DATA_DIR)
    options(timeout = 10)
    apply(sdmx_res_fltrd, 1, function(x) download_oecd_dataset(x["sdmx_id"], x["id"]))

    oecd_dfs <- create_oecd_df_list()

    check_oecd_data_sets(oecd_dfs, datasets_already_there)

    vague_res_df <- filter_vague_crvg()

    filter(vague_res_df, country_nbr > 25 & time_cvrg > 25)
}

    
## focus on AEA
## STANI4_2016
## STANI4_2020
