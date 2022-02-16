## *** tests, probably can be removed

df_oced <- as_tibble(get_datasets())
## 1495 datasets noice

df_stan <- as_tibble(
    get_dataset("STANI4_2020", start_time = 2010, end_time = 2011,
                filter = list(c("DEU"))
                ))
## there's probably some query limit on the number of rows




dfx <- as_tibble(get_dataset("EPL_OV", 
                   filter = list(c("DEU", "FRA"), 
                                 c("EPRC_V1", "EPRC_V2")), 
                   start_time = 2008, end_time = 2010))


wdix <- read_dta("/home/johannes/ownCloud/wid/wid_2018_report/Computer Codes/Global Wealth Inequality/gpinterized.dta")
## only about   CN   FR   GB   US   WO

wdix <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/wid-data-25102021.dta"))
wdix_hist <- as_tibble(read_dta("/home/johannes/ownCloud/wid/wid_2022_report/data/WO_hist.dta"))

dfx2 <- as_tibble(get_dataset("SNA_TABLE11",
                              start_time = 2010, end_time = 2011,
                              filter = list(c("DEU"))
                ))

##  sdmx parsing tests


idx <- "STANI4"

sdmx_test <- readSDMX(dest_file, isURL = FALSE)


slotNames(sdmx_test)
## [1] "organisationSchemes" "concepts"            "codelists"          
## [4] "datastructures"      "xmlObj"              "schema"             
## [7] "header"              "footer"             
## these 8 overall slots seem to be general thing? 


cls <- slot(sdmx_test, "codelists")
slotNames(cls)
typeof(slot(cls, "codelists")) ## a list
len(slot(cls, "codelists"))


codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id")) #get list of codelists
codelist <- as.data.frame(slot(sdmx_test, "codelists"), codelistId = "CL_STANI4_IND") #get a codelist

## sdmx basically seems like annoyingly formatted  dict

codelist$idx <- idx

write.csv(codelist, paste0(SDMX_TBL_DIR, idx, ".csv"))

relevant_sdmx_tables <- c(
    "AEA---CL_AEA_ACTIVITY.csv",
    "BIMTS_CPA---CL_BIMTS_CPA_CPA_VER_2_1.csv",
    "DIOC_OCCUPATION_DET---CL_DIOC_OCCUPATION_DET_DET_OCCUP.csv",
    "FATS_OUT3_SERV---CL_FATS_OUT3_SERV_SERV.csv",
    "FDI_CTRY_IND_SUMM---CL_FDI_CTRY_IND_SUMM_ECO_ACT.csv",
    "FDI_INC_IND---CL_FDI_INC_IND_ECO_ACT.csv",
    "FDI_POS_IND---CL_FDI_POS_IND_ECO_ACT.csv",
    "ERTR_ACC---CL_ERTR_ACC_ACT.csv",
    "FDI_FLOW_IND---CL_FDI_FLOW_IND_ECO_ACT.csv",
    "FATS_IN3_SERV---CL_FATS_IN3_SERV_SERV.csv",
    "FDI_CTRY_ECO_HIST---CL_FDI_CTRY_ECO_HIST_ECO_ACT.csv",
    "NCM_LIVE---CL_NCM_LIVE_INDICATOR.csv",
    "NCM_STAGING---CL_NCM_STAGING_INDICATOR.csv",
    "SNA_TABLE42---CL_SNA_TABLE42_ACTIVITY.csv",
    "SNA_TABLE8A_ARCHIVE---CL_SNA_TABLE8A_ARCHIVE_ACTIVITY.csv",
    "SNA_TABLE31---CL_SNA_TABLE31_ACTIVITY.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_TRANSACT.csv",
    "SNA_TABLE44---CL_SNA_TABLE44_PRODUCT.csv",
    "SNA_TABLE6A_ARCHIVE---CL_SNA_TABLE6A_ARCHIVE_ACTIVITY.csv",
    "SDBS_BDI---CL_SDBS_BDI_SEC.csv",
    "SNA_TABLE7A_SNA93---CL_SNA_TABLE7A_SNA93_ACTIVITY.csv",
    "TEC1_REV4_COPY---CL_TEC1_REV4_COPY_SECTOR.csv",
    "SNA_TABLE8A---CL_SNA_TABLE8A_ACTIVITY.csv",
    "TEC5_REV4---CL_TEC5_REV4_SECTOR.csv",
    "SSIS_BSC---CL_SSIS_BSC_ISIC3.csv",
    "STANI4_2020---CL_STANI4_2020_IND.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_TRANSACT.csv",
    "SNA_TABLE40---CL_SNA_TABLE40_PRODUCT.csv",
    "SNA_TABLE6A---CL_SNA_TABLE6A_ACTIVITY.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_TRANSACT.csv",
    "SNA_TABLE30---CL_SNA_TABLE30_PRODUCT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_TRANSACT.csv",
    "SNA_TABLE43---CL_SNA_TABLE43_PRODUCT.csv",
    "SNA_TABLE7A_ARCHIVE---CL_SNA_TABLE7A_ARCHIVE_ACTIVITY.csv",
    "TEC9_REV4---CL_TEC9_REV4_SECTOR.csv",
    "SNA_TABLE9A---CL_SNA_TABLE9A_ACTIVITY.csv",
    "SSIS_BSC_ISIC4---CL_SSIS_BSC_ISIC4_ISIC4.csv",
    "STANI4_2016---CL_STANI4_2016_IND.csv",
    "SNA_TABLE41---CL_SNA_TABLE41_ACTIVITY.csv",
    "SDBS_BDI_ISIC4---CL_SDBS_BDI_ISIC4_SEC.csv",
    "TEC1_REV4---CL_TEC1_REV4_SECTOR.csv",
    "TEC6_REV4---CL_TEC6_REV4_SECTOR.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_ACTIVITY.csv",
    "SNA_TABLE45---CL_SNA_TABLE45_PRODUCT.csv",
    "SNA_TABLE7A---CL_SNA_TABLE7A_ACTIVITY.csv",
    "STANI4---CL_STANI4_IND.csv",
    "STANINDICATORSI4---CL_STANINDICATORSI4_IND.csv")



sdmx_tables_musem <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'museum'"),
    intern = TRUE)
    
sdmx_tables_cultural_services <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv 'cultural services'"),
    intern = TRUE)

sdmx_tables_cultural <- system(
    paste0("cd ", SDMX_TBL_DIR, " && grep -irl --include \\*.csv ' cultural'"),
    intern = TRUE)

relevant_sdmx_tables <- Reduce(union, list(sdmx_tables_musem, sdmx_tables_cultural_services, sdmx_tables_cultural))

sdmx_res_fltrd <- sdmx_res_cbn[which(rowSums(apply(sdmx_res_cbn, 2, function(x) grepl("museum", x)))>0),]


## *** figuring out download 

x <- as_tibble(get_dataset("STANI4_2020", filter = list("D90T92")))

## maybe possible to first search which column is actually the indicator?

unlist(lapply(names(df_stan), function(x) grepl("D90T92", df_stan[,x])))
## for STAN, D90T92 is in the first column 

## trying to pass all for countries, just get specific indicator
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("AUTx"),c("PROD"), c("D90T92"))))
## this works for some reason

## passing all doesn't work tho :(
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c("all"),c("PROD"), c("D90T92"))))

x <- as_tibble(get_dataset("STANI4_2020", filter="PROD.AUT", pre_formatted = TRUE))

## working url: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/AUT.PROD.D90T92/all
## not working: https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/STANI4_2020/all.PROD.D90T92/all

## https://data.oecd.org/api/sdmx-json-documentation/: CAN USE EMPTY STRING TO SELECT ALL
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c("PROD"), c("D90T92"))))
x <- as_tibble(get_dataset("STANI4_2020", filter=list(c(""),c(""), c("D90T92"))))


## AEA actually has the same structure huh 
x2 <- as_tibble(get_dataset("AEA", filter = list(c(""),c(""), c("R90-R92"))))

## FDI_POS_IND: doesn't have the same structure
## adding/removing empty filter strings doesn't seem to work

x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""), c("R91"))))
## actually does when just adding enough LUL 
x3 <- as_tibble(get_dataset("FDI_POS_IND", filter = list(c(""),c(""),c(""), c(""), c(""), c(""),  c(""), c(""), c("R91"))))


## write to file to read from separate download_oecd.R
## write.csv(sdmx_res_fltrd, "/home/johannes/Dropbox/phd/papers/org_pop/data/oecd_dbs/sdmx_res_fltrd.csv")

## *** testing how to call data downloading best 

test_printer <- function(datasetx, idx){
    print(paste0("datasetx: ", datasetx))
    print(paste0("idx: ", idx))
    print("-----------")
    }

apply(sdmx_res_fltrd, 1, test_printer, datasetx=sdmx_id, idx=id)

apply(sdmx_res_fltrd, 1, function(x) test_printer(x$sdmx_id, x$id))
apply(sdmx_res_fltrd, 1, function(x) test_printer(x["sdmx_id"], x["id"]))

test_printer("asdf", "jjj")

apply(sdmx_res_fltrd, 1, function(x) print(names(x)))
apply(sdmx_res_fltrd, 1, function(x) print(x["sdmx_id"]))


