## * reg_anls_playground

## ** sqlite test 

## needed packages for this script
## install.packages("sqldf")  # install this package if necessary
library(sqldf)




## connection to the database TestDB.sqlite
db=dbConnect(SQLite(), dbname="TestDB.sqlite")

dbExecute(conn = db, "PRAGMA foreign_keys=ON")

## create the first table of the database
dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS artists
        (ID INTEGER,
        name TEXT,
        PRIMARY KEY (ID))")

## create the second table
dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS tracks
        (track_ID INTEGER,
        title TEXT,
        artist INTEGER,
        FOREIGN KEY(artist) REFERENCES artists(ID),
        PRIMARY KEY (track_ID))")

## filling the artist table with two rows
dbSendQuery(conn = db,
            paste0("INSERT INTO artists
        VALUES (1,'Tom Chapin')"))
dbSendQuery(conn = db,
            paste0("INSERT INTO artists
        VALUES (2,'Harry Chapin')"))

## filling the tracks table
dbSendQuery(conn = db,
            paste0("INSERT INTO tracks
        VALUES (1,'Cats in the Cradle',1)"))
## with the following tracks filling order there must occur an error
### but how to switch on the 'FOREIGN KEY'
dbSendQuery(conn = db,
            paste0("INSERT INTO tracks
        VALUES (2,'Cats in the Cradle',2)"))

## list the tables of the database
print(dbListTables(db))

## list the columns of a specific table
print(dbListFields(db,"artists"))  # of artists
print(dbListFields(db,"tracks"))   # of tracks

## show the data ...
print(dbReadTable(db,"artists"))  # of artists
print(dbReadTable(db,"tracks"))   # of tracks

dbDisconnect(db)





## ** sqlite reg_res_objs

library(RSQLite)

db_regres <- dbConnect(RSQLite::SQLite(), "/home/johannes/reg_res.sqlite")
dbExecute(conn = db_regres, "PRAGMA foreign_keys=ON")

## gof_df_cbn

prep_sqlitedb(dbx=db_regres, dfx=df_reg_anls_cfgs_wide, table_title = "df_reg_anls_cfgs_wide",
              constraints = c("PRIMARY KEY (mdl_id)"), insert_data = T)

## gof_df_cbn
prep_sqlitedb(dbx=db_regres, dfx=gof_df_cbn, table_title = "gof_df_cbn", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, gof_names)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

## df_anls base
prep_sqlitedb(dbx = db_regres, dfx = df_anls_base, table_title = "df_anls_base", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

dbRemoveTable(db_regres, "df_anls_within")

prep_sqlitedb(dbx = db_regres, dfx = df_anls_within, table_title = "df_anls_within", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              ## could add link to df_reg_anls_cfgs_wide, but don't think I need to?
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)"))
## "FOREIGN KEY (mdl_id) REFERENCES gof_df_cbn (mdl_id)"))


prep_sqlitedb(dbx = db_regres, dfx = df_anls_all, table_title = "df_anls_all", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)"))


prep_sqlitedb(dbx = db_regres, dfx = df_best_mdls, table_title = "df_best_mdls", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "UNIQUE(cbn_name, vrbl_name_unlag)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

adt(df_best_mdls)[, .N, .(vrbl_name_unlag, cbn_name)][, .N, N]
adt(mdl_summary)[, .N, .(vrbl_name_unlag, cbn_name)][, .N, N]



prep_sqlitedb(dbx = db_regres, dfx = mdl_summary, table_title = "mdl_summary", insert_data = T,
              constraints =c(
                  "PRIMARY KEY (cbn_name, vrbl_name_unlag)",
                  "FOREIGN KEY (cbn_name, vrbl_name_unlag) REFERENCES df_best_mdls (cbn_name, vrbl_name_unlag)"))

