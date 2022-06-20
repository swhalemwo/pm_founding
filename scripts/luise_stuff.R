## ** luise 

## all_clctrs exists only in `readin_artnews_all` -> has to be run when debugging that function

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



    



## *** bvd
library(haven)
df_bvd <- read_sas("/home/johannes/Downloads/test4.sas7bdat")

pass <- "{SAS002}E2DE3C1F47177E042B981B5917E72D0A1C60B13029F5E6E6"
my_username <- "johannesae"


res <- dbSendQuery(wrds,"select date,dji from DJONES.DJDAILY (obs=10)")
data <- fetch(res, n = -1)




## *** bvd api
library(RPostgres)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user = "johannesae")

testquery <- "SELECT bvdid, first_name, middle_name, last_name, full_name 
FROM bvd.ob_dmc_current_only_l"


testdata <- dbGetQuery(wrds, testquery) %>% atb()

