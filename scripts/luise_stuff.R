## * luise 

## all_clctrs exists only in `readin_artnews_all` -> has to be run when debugging that function

## ** artnews to check 

df_excl <- create_excel_df(PMDB_FILE, only_pms = F)

pmdb_luise <- df_excl %>% select(founder_name_pmdb = "Founder name") %>%
    unique() %>% na.omit()


clctrs_luise <- all_clctrs %>% select(collector_name_artnews = clctr_name) %>% unique() %>%
    mutate(source = "artnews")


## %>% 
##     mutate(collector_name_artnews = "",
##            notes = "")


## check overlap automatically


pmdb_luise_aut_match <- merge(pmdb_luise, clctrs_luise, by.x = "founder_name_pmdb",
                              by.y = "collector_name_artnews", all.x = T) %>%
    atb() %>%
    mutate(collector_name_artnews = ifelse(is.na(source), "", founder_name_pmdb),
           notes = "") %>%
    select(-source)

pmdb_luise_aut_match %>% write.table(
        file = paste0(PROJECT_DIR, "scripts/artnews_cleaning/pm_founder_names.csv"),
        row.names = F, sep = ";")



clctrs_luise %>% select(collector_name_artnews) %>% 
    write.table(file = paste0(PROJECT_DIR, "scripts/artnews_cleaning/artnews_collector_names.csv"),
                row.names = F, sep = ";")



    



## ** bvd




## *** bvd api
library(RPostgres)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user = "johannesae")

testquery <- "SELECT bvdid, first_name, middle_name, last_name, full_name 
FROM bvd.ob_dmc_current_only_l limit 100"


testdata <- dbGetQuery(wrds, testquery) %>% atb()

dbGetQuery(wrds, "describe bvd.ob_dmc_current_only_l")

dbGetQuery(wrds, "select distinct libname from dictionary.libnames")


## dbGetQuery(wrds, "list tables")
## dbGetQuery(wrds, "show tables;")

## this works 
dbGetQuery(wrds, "SELECT table_name FROM information_schema.tables")



## dbListTables(wrds) ## only 14
## dbListObjects(wrds) ## 876 weird things
## dbListFields(wrds, "bvd.ob_dmc_current_only_l") ## table not found
## dbListResults(


dbGetQuery(wrds, "select distinct bvdid from bvd.ob_dmc_current_only_l limit 10")

dbGetQuery(wrds, "select * from bvd.ob_industry_classifications_l limit 10")



## * scrap
## *** manual 
library(haven)
df_bvd <- read_sas("/home/johannes/Downloads/test4.sas7bdat")

pass <- "{SAS002}E2DE3C1F47177E042B981B5917E72D0A1C60B13029F5E6E6"
my_username <- "johannesae"


res <- dbSendQuery(wrds,"select date,dji from DJONES.DJDAILY (obs=10)")
data <- fetch(res, n = -1)
