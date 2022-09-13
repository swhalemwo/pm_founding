## * artnews test 


testf_crctns_mrgr <- function(test_name, dt_excl2, an_merge_res, artnews_time_df, founder_years_fltrd1) {
    #' test whether merging-filtering of artnews mentions is done correctly

    ## generate subset df 
    dt_excl2_sbst <- dt_excl2[`Founder name` == test_name]

    ## generate founder years for one collector 
    founder_years_vec <- apply(dt_excl2_sbst, 1, \(x)
                               seq(x[["year_opened_int"]], min(x[["year_closed"]], ENDING_YEAR + 1, na.rm=T))) %>%
        unlist() %>% unique()

    an_years <- artnews_time_df %>% adt() %>% .[, clctr_name := gsub(" ", " ", clctr_name)] %>%
        .[clctr_name == test_name]

    ## setdiff(

    ## y <- adt(mtcars)
    ## y %>% .[,mpg := mpg*2]


    first_museum_opened <- , min(year_opened_int)]
    last_museum_closed <- dt_excl2[`Founder name` == test_name, max(ENDING_YEAR+1, year_closed, na.rm = T)]
    
    }



gto <- function(...) {
    #' get test object: shorter way of creating test objects
    expect_true(!is.null(...))
}


gto(an_merge_res <- gen_merge_res(create_excel_df(PMDB_FILE, only_pms = F)))
gto(dt_excl2 <- create_excel_df(PMDB_FILE) %>% adt() %>% .[, .(ID, `Founder name`, year_opened_int, year_closed)])


## ** generate objects 
## expect_true(!is.null(
##                  an_merge_res <- gen_merge_res(create_excel_df(PMDB_FILE, only_pms = F))))
    
## expect_true(!is.null(
##                  dt_excl2 <- create_excel_df(PMDB_FILE) %>% adt() %>% .[, .(ID, `Founder name`, year_opened_int, year_closed)]))

## ** 

test_that("an merge_res tests", {

    test_names <- c("Donald Hess", "François Pinault", "Wang Wei and Liu Yiqian", "Cindy and Howard Rachofsky")

    lapply(test_names, \(x) expect_true(any(findt(x, an_merge_res$collector_name_artnews))))

    
           
    
})


