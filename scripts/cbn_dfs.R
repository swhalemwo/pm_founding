## * combine dfs

check_dupl_names <- function(df1_name, df2_name) {
    #' diagnosing whether some names are not unique
    #' non-unique names are surrounded by ----; collision appears in preceding df pair
    
    names1 <- names(dfs_to_combine[[df1_name]])
    names2 <- names(dfs_to_combine[[df2_name]])
    
    print(paste0(df1_name, "-", df2_name))

    names_intersect <- intersect(names1, names2)

    if (!setequal(names_intersect, c("iso3c", "year")) & !setequal(names_intersect, c("iso3c"))) {
        ## print(setdiff(names_intersect, c("iso3c", "year")))
        ## print(names_intersect)

        print("---------")
        print(names_intersect)
        ## print(names1)
        ## print(names2)
        print("---------")
        
    }

    NULL
    ## else {
    ##     ## print("no overlap")
    ## }
}

    





get_df_reg <- function(df_anls) { 
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' combine together df_anls, inequality, tax incentive, mow, hnwi

    df_anls$NY.GDP.PCAP.CDk <- df_anls$NY.GDP.PCAP.CD/1000
    df_anls$SP.POP.TOTLm <- df_anls$SP.POP.TOTL/1e6

    ## cumulative number of opened
    ## df_anls$nbr_opened_cum <- ave(df_anls$nbr_opened, df_anls$iso3c, FUN = cumsum)


    ## df_anls$nbr_opened_cum_sqrd <- (df_anls$nbr_opened_cum^2)/100
    ## have to divide by 100 otherwise R glmer.nb complains

    ## PMs opened per 1m people -> rate
    df_anls$nbr_opened_prop <- df_anls$nbr_opened/(df_anls$SP.POP.TOTLm)

    ## filter(df_anls, nbr_opened_prop > 1)
    ## iceland, monaco, cyprus LUL


    ## inequalities
    df_ineq <- get_all_ineqs(WID_VX)

    ## tax incentives
    df_taxinc <- get_taxinc_dfs()

    ## mow

    mow_res <- get_mow_dfs()
    mow_crssctn <- mow_res$mow_crssctn
    mow_cntns <- mow_res$mow_cntns

    

    ## marginal tax rates 
    df_mtr <- construct_mtrs()

    ## artnews '
    df_artnews <- generate_artnews_data(yeet_pm_founders = F)
    

    ## cultural spending
    ## df_cult <- gen_cult_spending()
    df_cult <- gen_cult_spending_imptd()
    
    
    df_hnwi <- get_hnwi_pcts(WID_VX)

    df_hdi <- get_hdi()


    ## combine everything
    dfs_to_combine <- list(df_anls=df_anls, df_hnwi=df_hnwi, df_ineq=df_ineq, df_taxinc=df_taxinc,
                           mow_crssctn=mow_crssctn, mow_cntns=mow_cntns,df_mtr=df_mtr,
                           df_artnews=df_artnews, df_cult=df_cult, df_hdi = df_hdi)

    ## checking that names are unique    
    check_df_name_unqns(dfs_to_combine, skip_var_names = c("iso3c", "year"))
    ## check_dupl_names() # can use check_dupl_names to check manually

    

    df_reg <- as_tibble(Reduce(function(x,y,...) merge(x,y, all.x = TRUE), dfs_to_combine))





    ## fill up MOW data, maybe other databases later (IDA, Artfacts)
    fill_up_cols <- unique(c(names(mow_cntns), names(mow_crssctn)))
    df_reg[fill_up_cols][is.na(df_reg[fill_up_cols])] <- 0

    ## fill up AN cols only from 1990 (min year) onwards
    min_AN_year <- adt(df_reg)[clctr_cnt_cpaer != 0, min(year)] # yeet CYs before AN ranking existed
    fill_up_cols_AN <- setdiff(unique(names(df_artnews)), c("iso3c", "year"))
        ## df_reg[fill_up_cols_AN][is.na(df_reg[fill_up_cols_AN]) & df_reg$year >= min_AN_year] <- 0

    df_reg <- adt(df_reg)[year >= min_AN_year, (fill_up_cols_AN) := replace(.SD, is.na(.SD), 0),
                              .SDcols = fill_up_cols_AN] %>% atb()

    ## diag_dt <- adt(df_reg_fld)[, lapply(.SD, \(x) sum(is.na(x)))] %>% melt %>% .[, src := "dt"]

    ## test that it looks good
    ## ## replace all in tibbles with base approach 
    ## fill_up_cols2 <- unique(c(names(mow_cntns), names(mow_crssctn), names(df_artnews)))
    ## df_reg2 <- adt(df_reg) %>% copy() %>% atb()
    ## df_reg2[fill_up_cols2][is.na(df_reg2[fill_up_cols2])] <- 0
    ## diag2 <- adt(df_reg2)[, lapply(.SD, \(x) sum(is.na(x)))] %>% melt() %>% .[, src := "base"]

    ## rbind(diag_dt, diag2) %>%
    ##     dcast.data.table(variable ~ src) %>%
    ##     .[, diff := base - dt] %>%
    ##     .[diff != 0]

    ## df_reg_fld[, lapply(.SD, \(x) sum(is.na(x))), .SDcols = fill_up_cols_AN, year]
    ## ## -> seems to be fine
    


    ## generate counts from percentages for HNWI
    hnwi_names <- names(df_hnwi)[3:ncol(df_hnwi)]

    hnwi_cnts <- as_tibble((df_reg[hnwi_names] * df_reg$SP.POP.TOTL)/100)
    names(hnwi_cnts) <- gsub("pct_cutoff", "hnwi_nbr", hnwi_names)
    df_reg <- as_tibble(cbind(df_reg, hnwi_cnts))

        
    ## generate interaction
    df_reg$ti_tmitr_interact <- df_reg$Ind.tax.incentives * df_reg$tmitr_approx_linear20step

    df_reg$SP.POP.TOTL <- df_reg$SP.POP.TOTL/1

    ## filter out 
    meme_countries <- c("ZWE", # ZWE: cur_df has weird values -> virtually everything affected
                        "ISL", # small population -> high clctr_cnt_cpaer/cnt_contemp_1990
                        "BHS",  # small population -> high clctr_cnt_cpaer
                        "ARE") # yuge decrease in HNWI
    df_reg2 <- filter(df_reg, iso3c %!in% meme_countries)

    return(df_reg2)

    
}

## mtcars$vs_scaled <- scale_wo_attr(mtcars$vs)

## t1 <- lm(mpg ~ cyl + vs, mtcars) 
## t2 <- lm(mpg ~ cyl + vs_scaled, mtcars) 

## t3 <- lm(mpg ~ cyl * vs, mtcars) 
## t4 <- lm(mpg ~ cyl * vs_scaled, mtcars) 

## t5 <- lm(mpg ~ scale_wo_attr(cyl) * vs, mtcars)
## t6 <- lm(mpg ~ scale_wo_attr(cyl) * vs_scaled, mtcars) 


## screenreg(list(t1, t2, t3, t4, t5, t6))


check_if_var_is_longitudinal <- function(dfx, vx) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' check weather a variable has 0 within-variation -> not longitudinal

    ## xtsum(dfx, NY.GDP.PCAP.CD, iso3c)

    sd_within <- xtsum(dfx, get(vx), iso3c)$sd[3]
    return(sd_within != 0)
}


## check_if_var_is_longitudinal(df_reg, "clctr_cnt_all")
## check_if_var_is_longitudinal(df_reg, "cnt_art_2010")




## ** plug holes in df

impute_variable_linearly <- function(dfx, varx, max_gap_size=MAX_GAP_SIZE) {
    #' fill in gaps with linear imputation as long as they're not larger than max_gap_size
    

    dfx_imptd <- panel_fill(dfx, .flag = "flag", .i = iso3c, .t=year)

    na_filled_name <- paste0(varx, "_na_filled")
    vx <- varx



    dfx_imptd_approx <- dfx_imptd %>%
        mutate(!!varx := ifelse(!flag, get(varx), NA)) %>% 
        group_by(iso3c) %>% 
        mutate(!!varx := na.approx(get(varx), maxgap = max_gap_size, rule = 2))
    
    return(dfx_imptd_approx)

}

get_linear_imputation_vars_to_check <- function(df_regx, lngtd_vars) {

    #' extract the variables where linear imputation results in different coverage

    names(lngtd_vars) <- lngtd_vars
    gap_check <- lapply(lngtd_vars, \(x) na.omit(select(df_regx, iso3c, year, all_of(x))) %>%
                                         impute_variable_linearly(.,x) %>%
                                         na.omit())

    gap_check_cnts <- lapply(gap_check, nrow)
    gap_check_orgnl <- lapply(lngtd_vars, \(x) nrow(na.omit(select(df_regx, iso3c, year, all_of(x)))))



    gap_check_df <- list.cbind(list(gap_check_cnts, gap_check_orgnl)) %>% adf()
    rownames(gap_check_df) <- lngtd_vars
    gap_check_df <- gap_check_df %>% atb() %>% unnest(cols = c(V1, V2))
    gap_check_df$variable <- lngtd_vars

    gap_check_df <- gap_check_df %>% 
        mutate(diff = V1 - V2)
    
    filter(gap_check_df, diff > 0) %>% pull(variable)
    
}


imputation_checker <- function(vrbl_to_check, df_regx) {
    

    print(paste0("vrbl_to_check: ", vrbl_to_check))

    dfx <- df_regx %>% select(iso3c, year, all_of(c(vrbl_to_check))) %>% na.omit()%>%
        impute_variable_linearly(varx = vrbl_to_check) %>% na.omit() %>%
        group_by(iso3c) %>% 
        mutate(nbr_flags = n_distinct(flag)) %>% 
        filter(nbr_flags > 1)
        
    ## print(nrow(dfx))
        
    plt <- dfx %>% 
        ggplot(aes(x=year, y=get(vrbl_to_check))) +
        geom_line() +
        geom_point(aes(color = flag)) +
        facet_wrap(~iso3c, scales = "free") +
        labs(y=vrbl_to_check)

    print(plt)

    readline(prompt = "press enter to continue")

}

## imputation_checker(vrbl_to_check)
## imputation_checker("hnwi_nbr_1M")

check_lin_imptd_vars <- function(df_regx, lngtd_vars) {
    #' wrapper function for visually checking whether the linearly imputed variables make sense
    
    vars_to_check <- get_linear_imputation_vars_to_check(df_regx, lngtd_vars)

    lapply(vars_to_check, \(x) imputation_checker(x, df_regx))
}
    


## x <- gen_cult_spending_imptd()
## ## nrow(x)
## y <- impute_variable_linearly(dfx = x, varx = "smorc_dollar_fxm")

## y %>%
##     group_by(iso3c) %>% 
##     mutate(nbr_flags = n_distinct(flag)) %>% 
##     filter(nbr_flags > 1) %>% 
##     ggplot(aes(x=year, y=smorc_dollar_fxm)) +
##     geom_line() +
##     geom_point(aes(color = flag)) +
##     facet_wrap(~iso3c, scales = "free")
    



MANUAL_IMPUTATION_CHECK <- F

check_imputed_values <- function(df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## check all df vars that are not "base vars"
    ## also exclude tmitr because has only 1 obs for some countries, and I'm never gonna use that purely anyways
    lin_imptd_vars <- (setdiff(names(df_reg), c("iso3c", "region", "year", "name", "country", "tmitr")))

    ## narrow down to longitudinal variables
    lngtd_flag <- sapply(lin_imptd_vars, \(x) check_if_var_is_longitudinal(df_reg, x))
    imputation_vars_to_check <- lin_imptd_vars[lngtd_flag]
    
    ## narrow down to variables that change in coverage when values are imputed
    imputation_vars_to_check2 <- get_linear_imputation_vars_to_check(df_reg, imputation_vars_to_check)

    if (MANUAL_IMPUTATION_CHECK){
        check_lin_imptd_vars(df_reg, imputation_vars_to_check2)
    }
    return(imputation_vars_to_check2)
}

impute_df_reg_vrbls <- function(df_reg) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    ## impute the variables that are suitable and replace the original values
    

    ## get the variables which need imputing 
    vrbls_to_impute <- check_imputed_values(df_reg)

    ## generate the imputed data
    lin_imptd_data <- lapply(vrbls_to_impute, \(x)
                             impute_variable_linearly(na.omit(select(df_reg, iso3c, year, all_of(x))), x) %>%
                             select(-flag))

    df_reg_imptd <- Reduce(\(x, y) merge(x, y, all = T), lin_imptd_data) %>% atb()

    
    ## replace the data for the variables that are getting imputed
    vars_to_replace <- setdiff(names(df_reg_imptd), c("iso3c", "year"))
    df_reg_imptd_cbn <- merge(df_reg %>% select(-all_of(vars_to_replace)),
                              df_reg_imptd, all = T) %>% atb()

    return(df_reg_imptd_cbn)
}



## impt_test <- merge(select(df_reg_pre_impt, iso3c, year, smorc_dollar_fxm),
##                    select(df_reg, iso3c, year, smorc_dollar_fxm_imptd = smorc_dollar_fxm)) %>% atb()

## is.na(impt_test$smorc_dollar_fxm) %>% table()
## is.na(impt_test$smorc_dollar_fxm_imptd) %>% table()

## impt_test$diff <- apply(impt_test, 1, \(x) !identical(x[["smorc_dollar_fxm"]], x[["smorc_dollar_fxm_imptd"]]))
    
## filter(impt_test, diff) %>% adf()

## impt_test %>% 
##     group_by(iso3c) %>%
##     mutate(nbr_diffs = sum(diff)) %>%
##     filter(nbr_diffs > 0) %>% 
##     pivot_longer(cols = c(smorc_dollar_fxm, smorc_dollar_fxm_imptd)) %>%
##     ggplot(aes(x=year, y=value)) +
##     geom_line() +
##     geom_jitter(aes(color = name, shape = name)) +
##     facet_wrap(~iso3c, scales = "free")
    



