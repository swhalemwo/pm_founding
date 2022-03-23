


## get the gted data with the proper sheets
df_gted <- lapply(list(tep=1, ## tax expenditure provision
                       rev=2, ## Revenue forgone
                       ben=3, ## number of beneficiaries
                       cry=4), ## country data
                  function(x) as_tibble(read_excel(
                                  paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = x)))

## ** exploring df_gted$tep

names(df_gted$tep)

len(table(df_gted$tep$`Beneficiaries - Level 1`))
## beneficiaries level 1: 12 categories, 937 non-profit things
## level 2 is not standardized

table(df_gted$tep$`Estimation method`)
## estimation method basically always revenue foregone

table(df_gted$tep$`Data Type`)
## Source: name of source, not standardized
## Data Type: something about precision (estimates, somewhat/very disaggregated)

table(df_gted$tep$`Policy objective - Level 1`)
## mostly not stated

table(df_gted$tep$`Time frame`)
## permanent (almost 90%), sunset clause, temporary

table(df_gted$tep$`Tax base - Level 2`)
## 1: multiple, other, goods/services, income, property
## 2: 27 categories, all kinds of more detailed things
## 3: 12 categories: some more detailed stuff? categories like alcohol, fuel, tobacco??



table(is.na(df_gted$tep$`TE Description`))
head(df_gted$tep$`TE Description`[!is.na(df_gted$tep$`TE Description`)])
## some description, maybe can use it for filtering for art/museum stuff

table(head(df_gted$tep$`TE name (English)`[!is.na(df_gted$tep$`TE name (English)`)], 300))
## name, maybe useful for grepping too 

head(df_gted$tep$`Implementation and modifications`[!is.na(df_gted$tep$`Implementation and modifications`)], 30)
## some comment on introduction, maybe useful for information

table(df_gted$tep$`Type of TE`)
## 9 categories: deduction, deferral, exemption, multiple, ohter, reduced rate, tax credits, zero-rated


names(df_gted$tep)

## main variables I need:
## - Provision ID
## - Beneficiaries - Level 1
## - Policy objective - Level 1
## - Tax base - Level 1
## - Tax base - Level 2
## - Tax base - Level 3
## - TE Description
## - TE name (English)
## - Implementation and modifications
## - Time Frame



## ** exploring df_gted$rev

names(df_gted$rev)
## LCU: local currency unit

table(is.na(df_gted$rev$ProvisionID))

sample(df_gted$rev$ProvisionID, 30)

df_gted$rev$region <- countrycode(df_gted$rev$Country, "iso3c", "un.regionsub.name")

df_gted$rev$region <- countrycode(df_gted$rev$Country, "iso3c", "un.region.name")

df_gted$rev %>%
    group_by(Country, Year,region) %>%
    summarize(rf_pct=sum(`RF % of Tax`)) %>%
    viz_lines(dfx=.,x="Year", y="rf_pct", time_level = "ra", duration = 4, grp="Country", fill_up = F, facets="region")


## ** merging tep and 


## *** exploring ids
setdiff(df_gted$rev$ProvisionID, df_gted$tep$`Provision ID`)
## all provision IDs that are in rev are also in tep
setdiff(df_gted$tep$`Provision ID`, df_gted$rev$ProvisionID)
## 5.3k provisions that are in tep are not in rev

len(unique(df_gted$tep$`Provision ID`))
len(unique(df_gted$rev$ProvisionID))
## guess that means that for like 24% of provisions there is no estimate of revenue forgone

## *** actual merging

df_gted$tep[,c("Provision ID","Beneficiaries - Level 1","Policy objective - Level 1","Tax base - Level 1","Tax base - Level 2","Tax base - Level 3","TE Description","TE name (English)","Implementation and modifications","Time frame")]




## can merge to df_gted_rev over `Provision ID`


