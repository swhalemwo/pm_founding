
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
    
    l_qog_vrbls <- c("etfra", "eth2k", "hief")

    ## Historical index of ethnic fractionalization, fill up with LOCF
    dt_hief_prep1 <- fread(paste0(PROJECT_DIR, "data/diversity/qogdata_05_06_2024.csv")) %>%
        .[, .(iso3c = ccodealp, year, etfra = fe_etfra, eth2k = al_ethnic2000, hief = hief_efindex)] %>%
        .[, c(l_qog_vrbls) := lapply(.SD, \(x) as.numeric(gsub(",", ".", x))), .SDcols = l_qog_vrbls] %>%
        .[order(iso3c, year)]

    ## fill up with LOCF, NOCB, and both (LFNB: last forwards, next backwards)
    dt_hief_prep2 <- dt_hief_prep1 %>% copy %>%
        .[, paste0(l_qog_vrbls, "_locf") := lapply(.SD, \(x) nafill(x, "locf")), iso3c, .SDcols = l_qog_vrbls] %>%
        .[, paste0(l_qog_vrbls, "_nocb") := lapply(.SD, \(x) nafill(x, "nocb")), iso3c, .SDcols = l_qog_vrbls] %>%
        .[, paste0(l_qog_vrbls, "_lfnb") := lapply(.SD, \(x) nafill(nafill(x, "nocb"), "locf")),
          iso3c, .SDcols = l_qog_vrbls] %>% # both locf and nocb
        setnames("hief", "hief_orig") # rename hief to hief_orig for naming consistency

    ## imputation hief: just use LM
    r_impt_hief <- lm(hief_orig ~ eth2k, dt_hief_prep2[year == 2000])

    dt_hief_prep3 <- dt_hief_prep2 %>% copy %>%
        .[, `:=`(hief_orig_imptd = hief_orig, hief_locf_imptd = hief_locf, hief_nocb_imptd = hief_nocb,
                 hief_lfnb_imptd = hief_lfnb,
                 imptd_hief_orig = 0, imptd_hief_locf = 0, imptd_hief_nocb = 0, imptd_hief_lfnb = 0)] %>%
        .[is.na(hief_orig) & !is.na(eth2k), `:=`(hief_orig_imptd = predict(r_impt_hief, .SD),
                                                 imptd_hief_orig = 1)] %>%
        .[is.na(hief_locf) & !is.na(eth2k), `:=`(hief_locf_imptd = predict(r_impt_hief, .SD),
                                                 imptd_hief_locf = 1)] %>%
        .[is.na(hief_nocb) & !is.na(eth2k), `:=`(hief_nocb_imptd = predict(r_impt_hief, .SD),
                                                 imptd_hief_nocb = 1)] %>%
        .[is.na(hief_lfnb) & !is.na(eth2k), `:=`(hief_lfnb_imptd = predict(r_impt_hief, .SD),
                                                 imptd_hief_lfnb = 1)]

    ## just look at different imputed
    ## complicated melting and casting
    dt_hief_prep4 <- dt_hief_prep3 %>% .[, .SD, .SDcols = keep(names(.), ~grepl("hief|iso3c|year", .x))] %>% 
        melt(id.vars = c("iso3c", "year"), variable.name = "vrbl", value.name = "vlu") %>%
        .[, status := fifelse(grepl("imptd$", vrbl), "imptd",
                           fifelse(grepl("^imptd", vrbl), "indi", "orig"))] %>% 
        .[, src := fifelse(substring(vrbl, 1, 4) == "hief", substring(vrbl, 6, 9),
                           substring(vrbl, 12, 15))] %>%
        .[, vrbl := NULL]

    ## look at all the lines
    dt_hief_prep4 %>% ggplot(aes(x=year, y=vlu, group=iso3c)) + geom_line(show.legend = F) +
        facet_grid(src ~ status, scales = "free")
    
    ## cast to wide
    dt_hief_prep5 <- dt_hief_prep4 %>% data.table::dcast(iso3c + year + src ~ status, value.var = "vlu")

    ## look only "imputed" (final) values (combination of present and imputed), imputationn indi-cated by color
    dt_hief_prep5 %>%
        .[year >= 1990] %>% 
        ggplot(aes(x=year, y=imptd, group = iso3c, color = factor(indi))) + geom_line() +
        facet_wrap(~src)
    

    ## xtsum
    dt_hief_prep5[src == "orig" &  year  > 1995 & !is.na(orig)] %>% atb %>% 
        xtsum(orig, iso3c) %>% adt
    
    
    
        
    
    

    dt_hief %>% summary
    

    dt_div <- join(dt_mist, dt_hief, on = c("iso3c", "year"), how = "full")
    
    
    return(atb(dt_div))

}

## gd_diversity()


exp_diversity_CYdiff <- function() {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## read RDS file
    cbn_dfs_rates_v27 <- readRDS(paste0(PROJECT_DIR, "data/diversity/cbn_dfs_rates_v27.rds"))
    
    ## map(cbn_dfs_rates_v27, fnrow)
    ## map(cbn_dfs_rates, fnrow)
    
    ## see how current (v31) differs from previous (v27): what inclusion of hief and migrantstock yeets
    dt_diff <- map2(cbn_dfs_rates, cbn_dfs_rates_v27,
         ~adt(.y)[!adt(.x), on = c("iso3c", "year")][, .(iso3c, year)]) %>%
        imap(~.[, src := .y]) %>% rbindlist %>%
        .[, region := rcd_iso3c_reg6(iso3c)]

    dt_diff[, .N, .(iso3c, src)][order(src, -N)] %>% print(n=300) #  by country 

    dt_diff[, .N, .(region, src)][order(src, -N)] %>% print(n=300) # by region


    ##  look at coverage
    dt_div <- gd_diversity() %>% adt

    dt_div %>% melt(id.vars = c("iso3c", "year"), variable.name = "vrbl", value.name = "vlu") %>%
        ggplot(aes(x=year, y=vlu, group=iso3c)) + geom_line(show.legend = F) + facet_wrap(~vrbl, scales = "free")

    ## check which countries are all NA: hief might just be super lacking
    l_qog_vrbls <- c("etfra", "eth2k", "hief")
    dt_div_cpltns <- dt_div[year >= 1990, map(.SD, ~all(is.na(.x))+0), iso3c, .SDcols = l_qog_vrbls]

    
    dt_div_cpltns[, map(.SD, sum), .SDcols = l_qog_vrbls]

    ## compare variables in 2000

    ## mostly agree, could sort out the handful of casese  that don't agree
    dt_div[year == 2000, .SD, .SDcols = c("iso3c", l_qog_vrbls)] %>% melt(id.vars = "iso3c") %>%
        .[., on = "iso3c", allow.cartesian = T] %>%
        ggplot(aes(x=value, y=i.value)) + geom_point() + facet_grid(variable ~ i.variable) 


    
    ## correlations stay high -> 

    
    



}

gp_div_accuracy <- function() {
    ## correlation by year
    ## evaluate how similar the different ethnic diversity measures are
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    dt_div <- gd_diversity() %>% adt
    l_qog_vrbls <- c("etfra", "eth2k", "hief")

    gd_cor <- function(dtx) {
        if (as.character(match.call()[[1]]) %in% fstd){browser()}
        #' make correlations of variables into list (to assign in dt group by)
        dt_prep <- dtx[, .SD, .SDcols = l_qog_vrbls] %>% cor(use= "pairwise.complete.obs") %>%
            adt(keep.rownames = "vrbl") %>%
            melt(id.vars = "vrbl", variable.name = "vrbl2", variable.factor = F, value.name = "cor")%>%
            .[vrbl > vrbl2, .(vrbl_str = paste0(vrbl, "_", vrbl2), cor)]
        setNames(dt_prep$cor, dt_prep$vrbl_str) %>% as.list
    }

    ## actually do per yea
    dt_cor_year <- dt_div[year %between% c(1980, 2013), gd_cor(.SD), by = year, .SDcols = l_qog_vrbls] %>%
        .[order(year)]
    print(dt_cor_year, n=40)

    p1 <- dt_cor_year %>% melt(id.vars = "year", variable.name = "vrbl", value.name = "cor") %>%
        ggplot(aes(x=year, y=cor, color = vrbl)) + geom_line()
    ## yikes, correlation actually  drops quite  a bit from 2000 onwards, also decline seems to increase
    ## is from a somewhat high position, but still concerning

    l_cor_vrbls <- keep(names(dt_cor_year), ~.x != "year")
    l_cor_vrbls_shift <- paste0(l_cor_vrbls, "_l1")
    
    ## change of decline: seems to increase from 2000 onwards
    p2 <- dt_cor_year %>% copy %>% # .[, .SD - shift(.SD)] %>%
        .[, c(l_cor_vrbls_shift) := map(.SD, ~.x-shift(.x)), .SDcols = l_cor_vrbls] %>% 
        melt(id.vars = "year", measure.vars = l_cor_vrbls_shift, variable.name = "vrbl", value.name = "d_cor") %>%
        ggplot(aes(x=year, y=d_cor, color = vrbl)) + geom_line() + geom_smooth()

    ## by 2010 decline is at 0.0025, i.e. 4 years will reduce correlation by 1 percentage point
    ## but even if it was 0.005 the entire 7 years, it would still only  reduce to ~0.8 -> should be good

    library(patchwork)
    p1 / p2

}
    

    
    
