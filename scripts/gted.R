


## get the gted data with the proper sheets
df_gted <- lapply(list(tep=1, ## tax expenditure provision
                       rev=2, ## Revenue forgone
                       ben=3, ## number of beneficiaries
                       cry=4), ## country data
                  function(x) as_tibble(read_excel(
                                  paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = x)))

## ** exploring df_gted$tep

names(df_gted$tep)

table(df_gted$tep$`Beneficiaries - Level 1`)
len(table(df_gted$tep$`Beneficiaries - Level 1`))
## beneficiaries level 1: 12 categories, 937 non-profit things
## level 2 is not standardized

table(is.na(df_gted$tep$`Beneficiaries - Level 2`))
## 11k have some unstandardized beneficiaries, 11k don't

grepl_term <- "collector"

table(grepl(grepl_term, df_gted$tep$`Beneficiaries - Level 2`))
sample(df_gted$tep$`Beneficiaries - Level 2`[grepl(grepl_term, df_gted$tep$`Beneficiaries - Level 2`)],2)
## 515 about art, but mostly about stuff in word (particular, start, parties)
## only 4 about " art "
## only 2: about "collector"
## 8 about "museum", but also sounds a lot like it could be about public ones? or could be outsourced ones like Stedelijk?
## 24 about " cultural" ("cultural" has 194, but probably most agricultural)




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

df_gted$rev$region <- countrycode(df_gted$rev$Country, "iso3c", "un.region.name")

plt_tax_gone <- df_gted$rev %>%
    group_by(Country, Year,region) %>%
    summarize(rf_pct=sum(`RF % of Tax`)) %>%
    viz_lines(dfx=.,x="Year", y="rf_pct", time_level = "ra", duration = 4, grp="Country", fill_up = F, facets="region", return="plot")

pdf(paste0(FIG_DIR, "gted_tax_gone.pdf"), width=16, height=10)
plot(plt_tax_gone +
     labs(title = "Percentage of Tax foregone according to GTED"))
dev.off()


## ** merging tep and rev


## *** exploring ids
setdiff(df_gted$rev$ProvisionID, df_gted$tep$`Provision ID`)
## all provision IDs that are in rev are also in tep
setdiff(df_gted$tep$`Provision ID`, df_gted$rev$ProvisionID)
## 5.3k provisions that are in tep are not in rev

len(unique(df_gted$tep$`Provision ID`))
len(unique(df_gted$rev$ProvisionID))
## guess that means that for like 24% of provisions there is no estimate of revenue forgone

## *** actual merging

df_gted$teprev <- as_tibble(merge(
    df_gted$tep[,c("Provision ID","Beneficiaries - Level 1","Policy objective - Level 1","Tax base - Level 1","Tax base - Level 2","Tax base - Level 3","TE Description","TE name (English)","Implementation and modifications","Time frame")],
    df_gted$rev, by.x = "Provision ID", by.y = "ProvisionID"))

## focus on provision to non-profits

df_gted$teprev_npo <- 
    filter(df_gted$teprev, `Beneficiaries - Level 1` == "Non-profit organizations/NGOs/Philanthropic organizations/foundations")
len(unique(df_gted$teprev_npo$Country))
## 51 countries are reporting provisions to NPO etc
## US not included 

plt_tax_gone_npo <- df_gted$teprev_npo %>%
    group_by(Country, Year, region) %>%
    summarize(rf_pct = sum(`RF % of Tax`)) %>%
    viz_lines(x="Year", y="rf_pct", time_level = "ra", duration = 4, grp = "Country", facets="region", max_lines = 8, return = "plot")

pdf(paste0(FIG_DIR, "gted_tax_gone_npo.pdf"), width = 16, height = 10)
plot(plt_tax_gone_npo + 
     labs(title = "Percentage of Tax foregone due to provisions to non-profits/foundations/NGO/philanthropy (GTED)"))
dev.off()

## need to focus on countries that report provision level data
## not just countries that report provisions for NPOs
## but US not included -> try grepping that's a good trick: 

df_rf_pct <- df_gted$teprev %>%
    group_by(Country, Year) %>%
    summarize(rf_pct = sum(`RF % of Tax`), year=max(Year,1), iso3c=sample(Country,1)) %>%
    select(iso3c, year, rf_pct) %>%
    ungroup() %>%
    na.omit() 

table(df_gted$teprev$Year)
table(df_rf_pct$year)


df_rf_pct_npo <- df_gted$teprev_npo %>%
    group_by(Country, Year) %>%
    summarize(rf_pct = sum(`RF % of Tax`), year=max(Year,1), iso3c=sample(Country,1)) %>%
    select(iso3c, year, rf_pct) %>%
    ungroup() %>%
    na.omit() 
    
    
cpltns_checker(df_rf_pct[,c("year", "iso3c", "rf_pct")], "rf_pct")
cpltns_checker(df_rf_pct_npo[,c("year", "iso3c", "rf_pct")], "rf_pct")


## can merge to df_gted_rev over `Provision ID`


