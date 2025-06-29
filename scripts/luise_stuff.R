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
library(collapse)

dGQ <- dbGetQuery

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user = "johannesae")

testquery <- "SELECT bvdid, first_name, middle_name, last_name, full_name 
FROM bvd.ob_dmc_current_only_l limit 100"


testdata <- dbGetQuery(wrds, testquery) %>% atb()

## describe table


dbGetQuery(wrds, "describe bvd.ob_dmc_current_only_l")
dbGetQuery(wrds, "select distinct libname from dictionary.libnames")

dbGetQuery(wrds, "
select table_name, column_name, data_type from information_schema.columns
where table_name = 'ob_dmc_current_only_l'") %>% adt
## bvd isn't part of name

## see what other columns are in information_schema.columns
dt_bvdinfo <- dbGetQuery(wrds, "
select * from information_schema.columns where table_name = 'ob_dmc_current_only_l'") %>% adt

str(dt_bvdinfo)
dt_bvdinfo[, .N, table_catalog]

## relevant columns:
## table_catalog
## table_schema
dt_bvdinfo[, .N, table_schema]


dt_bvdinfo[table_schema == "bvd"] %>% str

## test some other db
dGQ(wrds, "
select * from information_schema.columns where table_schema = 'crsp'") %>% adt

## foreign keys
dGQ(wrds, "
select * from pg_constraint limit 10")

dGQ(wrds, "select count(*) as cnt from pg_constraint")
## huh only 481 foreign keys?



## there's a pg_constraint table 

## doesn't work (doesn't show anything)
## https://soft-builder.com/how-to-list-all-foreign-keys-in-postgresql-database/
dGQ(wrds,
    "SELECT conrelid::regclass AS table_name, 
       conname AS foreign_key, 
       pg_get_constraintdef(oid) 
FROM   pg_constraint 
WHERE  contype = 'f' 
AND    connamespace = 'public'::regnamespace   
ORDER  BY conrelid::regclass::text, contype DESC;")
    

dGQ(wrds, " SELECT conrelid::regclass AS table_name, conname AS foreign_key, pg_get_constraintdef(oid) FROM pg_constraint WHERE contype = 'f' and conrelid::regclass::text = 'bvd.ob_dmc_current_only_l' AND connamespace = 'public'::regnamespace ORDER BY conrelid::regclass::text, contype DESC; ")




## get BvD tables
dt_bvdinfo2 <- dGQ(wrds, "
select * from information_schema.columns where table_schema = 'bvd'") %>% adt

dt_bvdinfo2[, .(funique(table_name))]





## dbGetQuery(wrds, "list tables")
## dbGetQuery(wrds, "show tables;")

## this works 
dt_table_info <- dbGetQuery(wrds, "SELECT table_name FROM information_schema.tables") %>% adt

## but where are the BvD tables?
dt_table_info[table_name == "bvd.ob_dmc_current_only_l"] # no result
dt_table_info[grep("ob_dmc_current_only_l", table_name)] # "bvd." is not part of table name? 



## dbListTables(wrds) ## only 14
## dbListObjects(wrds) ## 876 weird things
## dbListFields(wrds, "bvd.ob_dmc_current_only_l") ## table not found
## dbListResults(


dbGetQuery(wrds, "select distinct bvdid from bvd.ob_dmc_current_only_l limit 10")

dbGetQuery(wrds, "select * from bvd.ob_industry_classifications_l limit 10")


## ** bvd.ob_dmc_current_only_l exploration
dGQ(wrds, "select count(*) as cnt from bvd.ob_dmc_current_only_l") # 12m
dGQ(wrds, "select count(*) as cnt from bvd.ob_dmc_previous_l") # 13m

dt_pinault <- dGQ(wrds, "select * from bvd.ob_dmc_current_only_l where last_name = 'Pinault' and first_name = 'Francois'") %>% adt

dt_pinault[grep("francois", first_name, ignore.case = T), .N, .(bvdid, first_name, middle_name, last_name)]

dt_pinault[grep("francois", first_name, ignore.case = T), .N, .(first_name, middle_name, last_name)]

dt_pinault[grep("francois", first_name, ignore.case = T),
           .(bvdid, ctryiso, first_name, middle_name, last_name, appointment_date)] %>% print(n=200)

dt_pinault[grep("francois", first_name, ignore.case = T),
           .(bvdid, date_of_birth, first_name, middle_name, last_name, appointment_date)] %>% print(n=200)

dt_pinault[, .N, .(cpycontacts_header_iddirector, full_name)]

dt_pinault %>% head %>% adf



## * scrap
## *** manual 
library(haven)
df_bvd <- read_sas("/home/johannes/Downloads/test4.sas7bdat")



res <- dbSendQuery(wrds,"select date,dji from DJONES.DJDAILY (obs=10)")
data <- fetch(res, n = -1)
