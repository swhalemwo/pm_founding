

## ** fraser institute/economic freedom of the world

FRASER_DIR <- paste0(PROJECT_DIR, "data/Fraser Institute/")
EFW_FILES <- list.files(FRASER_DIR, pattern = "csv")
names(EFW_FILES) <- EFW_FILES

extract_efw_data <- function(filename) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' read in the EFW data, rename the data columns
    efw_df <- as_tibble(read.csv(paste0(FRASER_DIR, filename), skip=4))

    ## rel_cols <- c("Year", "ISO_Code", "marginal income tax rate")
    ## names(rel_cols) <- rel_cols

    ## all kinds of columns have "data"
    data_cols_pos <- which(unlist(lapply(names(efw_df), function(x) scramblematch("data.", x))))
    data_cols <- names(efw_df)[data_cols_pos]
    names(data_cols) <- data_cols
    
    data_cols_edited <- lapply(data_cols, function(x)
        list(orig_name = x,
             mod_name = paste0("data_", names(efw_df)[which(names(efw_df) == x)-1])))

    data_cols_edited <- unlist(lapply(data_cols, function(x)
        paste0("data_", names(efw_df)[which(names(efw_df) == x)-1])))
    
    names(efw_df)[data_cols_pos] <- data_cols_edited

    return(efw_df)
}

## extract_efw_data("economicdata2020-2020.csv")
    


## efw_melt <- as_tibble(reshape2::melt(efw_df[,2:ncol(efw_df)], id=names(efw_df)[2:4]))
## sample(na.omit(efw_melt$value), 100)

## efw_melt$value[grepl("-", efw_melt$value)]

## filter(efw_melt, grepl("[^-]-[^-]", efw_melt$value)) %>%
##     select(variable) %>%
##     table() %>%
##     sort()

## ## hyphens in data_Top.marginal.income.tax.rate and data_Top.marginal.income.and.payroll.tax.rate

## filter(efw_melt, grepl("[^-]-[^-]", efw_melt$value))

## ## look for other weird values

## weird_values <- filter(efw_melt, grepl("[^0-9.]", efw_melt$value))$value
## weird_values <- gsub(",", "", weird_values) ## filtering out commas
## weird_values[is.na(as.numeric(weird_values))]
## ## seems I capture all stuff like this 


## differences don't seem super huge,
## think it's best to use larger one since indicator is about top

efw_clean_values <- function(values){
                                   #     
    ## print(values[1:10])
    values_clean <- gsub(",", "", values)
    splits <- strsplit(as.character(values_clean), split="-")
    
    splits_max <- as.numeric(unlist(lapply(splits, function(x) x[max(1,len(x))])))
    return(splits_max)

    
}

te_cry_input <- function(iso3c, years, values) {
    #' manual input to trading economics (te) data
    data.table(iso3c = iso3c, year = years, tmitr = values)
}
    


te_manual_mitr_input <- function() {
    #' manually filled in data from tradingeconomics (te)

    ## x here is the result of construct_mtrs()
    ## go through block and check which countries have missing values, look them up individually at e.g.:
    ## https://tradingeconomics.com/yemen/personal-income-tax-rate

    ## x %>% mutate(no_value = is.na(tmitr)) %>%
    ##     filter(year > 2000) %>%
    ##     group_by(iso3c) %>% 
    ##     mutate(nbr_nas = sum(no_value)) %>%
    ##     filter(nbr_nas > 2) %>%
    ##     select(iso3c, year, tmitr) %>% adf() %>%
    ##     mutate(country_name = countrycode(iso3c, "iso3c", "country.name"))

    te_mnl_mitrs <- rbind(
        te_cry_input("AGO", c(seq(2004, 2008)), rep(15,5)),
        te_cry_input("ALB", 2004, 25),
        te_cry_input("ARM", seq(2003,2007), 20),
        te_cry_input("BIH", 2004, 5),
        te_cry_input("BLR", c(seq(2004,2008), seq(2009,2014), 2015), c(rep(30,5), rep(12,6), 13)),
        te_cry_input("BRN", seq(2004,2009), 0),
        te_cry_input("GIN", seq(2006,2012), 40),
        te_cry_input("IRQ", seq(2009,2015), 15),
        te_cry_input("KHM", 2009, 20),
        te_cry_input("LAO", seq(2009, 2013), c(25,25,28,28,24)),
        te_cry_input("LBN", seq(2004,2009), c(20)),
        te_cry_input("LBY", seq(2009, 2012), c(15, 10, 10, 10)),
        te_cry_input("LSO", seq(2006, 2010), 35),
        te_cry_input("MDG", 2007, 30),
        te_cry_input("QAT", seq(2004, 2009), 0),
        te_cry_input("RWA", seq(2006, 2010), 30),
        te_cry_input("SAU", seq(2001,2009), 0),
        te_cry_input("SDN", seq(2006, 2015), c(20,20, rep(15,8))),
        te_cry_input("SLE", seq(2004, 2010), c(35,35, rep(30,5))),
        te_cry_input("SWZ", seq(2004, 2009), 33),
        te_cry_input("SYC", seq(2006, 2012), 15),
        te_cry_input("TJK", seq(2008,2009), 13),
        te_cry_input("YEM", seq(2004,2010), 20))

    return(te_mnl_mitrs)
}




## efw_xl <- read_excel(paste0(FRASER_DIR, "efotw-2022-master-index-data-for-researchers-iso.xlsx"), skip=3)
## names(efw_xl) <- 

## efw_col_names <- na.omit(unlist(efw_xl[1,]))

construct_mtrs <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' wrapper function

    efw_dfs <- lapply(EFW_FILES, extract_efw_data)

    ## efw_names <- unlist(lapply(efw_dfs, names))
    ## x <- imap_dfr(efw_dfs, ~data.table(cnames = names(.x), filename = .y))
    ## x[, Nx := .N, cnames]
    ## x[Nx!=26] %>% adf()

    ## table(efw_names)
    ## data.table(namex = efw_names) %>% .[, .N, namex] %>% .[, .N, N]
    
    ## names are consistent across datafiles

    efw_df <- as_tibble(Reduce(function(x,y) rbind(x,y), efw_dfs))

    ## efw_remove_hyphens(efw_df$data_Top.marginal.income.tax.rate)

    ## overwrite stuff, I don't like this but is fastest now 
    efw_df[,5:ncol(efw_df)] <- lapply(efw_df[,5:ncol(efw_df)], efw_clean_values)
    names(efw_df)[2:3] <- c("year", "iso3c")


    ## efw_df$data_Top.marginal.income.tax.rate
    ## cpltns_checker(efw_df[,c("data_Top.marginal.income.tax.rate
    
    efw_base <- as_tibble(expand.grid(iso3c=unique(efw_df$iso3c), year = seq(1985, 2020)))
    efw_fill_up <- as_tibble(merge(efw_base, efw_df, all.x = T))
    ## use na.rm=F to return leading NAs

    ## prepare/test merging of efw (economic freedom of the world) and TE (tradings economics data)
    efw_dt_prep <- select(efw_fill_up, iso3c, year, tmitr = data_Top.marginal.income.tax.rate) %>% adt()

    te_mnl_mitrs <- te_manual_mitr_input()

    ## check that TE only adds CYs where EFW has NAs: negative joining TE from EFW should not decrease rows
    if (nrow(na.omit(efw_dt_prep)) != nrow(na.omit(efw_dt_prep)[!te_mnl_mitrs, on=.(iso3c, year)])) {
        stop("TE and EFW CYs are overlapping")}

    ## change name in TE tmitrs to allow update join (te_mnl_mitrs should only change NAs in efw_dt_prep
    setnames(te_mnl_mitrs, "tmitr", "tmitr_te")
    efw_dt_prep2 <- efw_dt_prep %>% copy() %>% .[te_mnl_mitrs, tmitr := tmitr_te, on = .(iso3c, year)]
    ## manual counting sums 
    ## sum(is.na(efw_dt_prep$tmitr))
    ## sum(is.na(efw_dt_prep2$tmitr))


    efw_fill_up2 <- atb(efw_dt_prep2) %>% 
        ## efw_fill_up2 <- efw_fill_up %>%
        ##     select(iso3c, year, tmitr = data_Top.marginal.income.tax.rate) %>%
        group_by(iso3c) %>%
        mutate(tmitr_approx_linear = na.approx(tmitr, na.rm = F),
               tmitr_approx_step = na.locf(tmitr, na.rm = F),
               tmitr_approx_linear20step = tmitr_approx_linear)

    ## fill in last values manually like a fucking tool
    ## efw_fill_up2[which(efw_fill_up2$year == 2020),c("tmitr_approx_linear20step")] <- efw_fill_up2[which(efw_fill_up2$year == 2019),c("tmitr_approx_linear20step")]

    return(efw_fill_up2)
    
    ## cpltns_checker(efw_fill_up2, varx="tmitr")
    ## cpltns_checker(efw_fill_up2, varx="tmitr_approx_linear")
    ## cpltns_checker(efw_fill_up2, varx="tmitr_approx_step")
    ## cpltns_checker(efw_fill_up2, varx="tmitr_approx_linear_2020step")
    ## ## filter(efw_fill_up, iso3c=="AGO")

    
    ## table(is.na(filter(efw_fill_up, is.na(tmirt_approx_linear))$tmirt_approx_step))

    ## filter(efw_fill_up, is.na(tmirt_approx_linear) & !is.na(tmirt_approx_step))
    ## step interpolation carries over last value -> has the values for 2020



    ## if I somehow predict tax incentives, I shouldn't use linearly imputed values for predictions, rather impute predicted values linearly afterwards

    ## would reeaaaaaaaaally like to run some PCA on this EFW data, but would need better regression methods
## also too data-driven 


}

## efw_df <- construct_mtrs()

## efw_df %>%
##     cpltns_checker(varx="tmitr_approx_linear_2020step")

## efw_df$region <- countrycode(efw_df$iso3c, "iso3c", "un.region.name")

## pdf(paste0(FIG_DIR, "tmitr.pdf"), width=16, height=10)
## filter(efw_df, year >=1985) %>%
##     select(iso3c, year, tmitr=data_Top.marginal.income.tax.rate, region) %>%
##     viz_lines(x="year", y="tmitr", time_level = "ra", duration = 2, grp="iso3c", facets = "region", max_lines=8)
## dev.off()
