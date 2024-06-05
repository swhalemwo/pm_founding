
exp_diversity <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    l_wbcache <- wb_cache()

    dt_wbvrbls <- l_wbcache$indicators %>% adt

    ## immigration, foreign born
    dt_wbvrbls[grepl("immig", indicator), .(indicator)]
    dt_wbvrbls[grepl("foreign", indicator), .(indicator)] # no good
    dt_wbvrbls[grepl("migra", indicator), .(indicator_id, indicator)] %>% print(n=30)
    # SM.POP.TOTL.ZS international migrant stock (% of population)

    ## racial diversity
    dt_wbvrbls[grepl("race", indicator), .(indicator)] %>% print(n=80)

    ## check some data I found on datafinder.qog.gu.se
    div_vrbls <- c("fe_etfra", "al_ethnic2000", "hief_efindex")
    
    dt_dvrst <- fread(paste0(PROJECT_DIR, "data/diversity/qogdata_05_06_2024.csv")) %>%
        .[, .(iso3c = ccodealp, year, fe_etfra, al_ethnic2000, hief_efindex)] %>%
        .[, (div_vrbls) := lapply(.SD, \(x) as.numeric(gsub(",", ".", x))), .SDcols = div_vrbls]
        
    varying(dt_dvrst, ~iso3c)
    ## fe_etfra: stable
    ## al_ethnic2000: stable
    ## hief_efindex: varies over time
    

    dt_dvrst %>% melt(id.vars = c("iso3c", "year"), variable.name = "vrbl", value.name = "vlu") %>%
        ggplot(aes(x=year, y=vlu, group=iso3c)) + geom_line(show.legend = F) + facet_wrap(~vrbl, scales = "free")

    ## hief_efindex: suspiciously smooth..
    ## otoh this variable don't change that abruptly, but rather slowly through migration.. 


}



gd_diversity <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## migrant stock: fill up with LOCF
    dt_mist <- get_WB_data("SM.POP.TOTL.ZS") %>% adt %>%  
        .[, SM.POP.TOTL.ZS := nafill(SM.POP.TOTL.ZS, "locf"), by = iso3c] %>%
        .[, country := NULL]
    
    ## dt_mist[iso3c == "DEU"]

    ## ggplot(na.omit(dt_mist), aes(x=year, y = SM.POP.TOTL.ZS, group = iso3c)) + geom_line(show.legend = F)
    
    ## Historical index of ethnic fractionalization, fill up with LOCF
    dt_hief <- fread(paste0(PROJECT_DIR, "data/diversity/qogdata_05_06_2024.csv")) %>%
        .[, .(iso3c = ccodealp, year, hief = as.numeric(gsub(",", ".", hief_efindex)))] %>%
        .[, hief := nafill(hief, "locf"), by = iso3c] 

    dt_div <- join(dt_mist, dt_hief, on = c("iso3c", "year"), how = "full")
    
    
    return(atb(dt_div))

}

## gd_diversity()
