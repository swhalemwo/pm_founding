## *** more optimization testing by grid.expanding optimization parameters

startx = 8
byx = 4
topx <- 17
tols <- expand.grid(tol = seq(startx,topx, by=byx),
            reltol = seq(startx,topx, by=byx),
            gradtol = seq(startx,topx, by=byx),
            steptol = seq(startx,topx, by=byx),
            lambdatol = seq(startx,topx, by=byx),
            qrtol = seq(startx,topx, by=byx)) %>% adt()

tols2 <- 1/10^tols


proc_tol_cfg <- function(tol_vec) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' the the grid.expand-ed tolerance configurations
    
    
    base_str_pglm <- 'pglm(nbr_opened ~ smorc_dollar_fxm_lag1 + NY.GDP.PCAP.CDk_lag1 + Ind.tax.incentives,
     index = "iso3c", data = cbn_dfs$cbn_all,  model = "random",  effect = "individual",
     family = negbin,
     tol = %s, reltol = %s, gradtol = %s, steptol = %s, lambdatol = %s, qrtol = %s)'

    str_procd <- sprintf(base_str_pglm, tol_vec$tol, tol_vec$reltol, tol_vec$gradtol, tol_vec$steptol,
                         tol_vec$lambdatol, tol_vec$qrtol)

    eval(parse(text = str_procd))
    
}

tols2.list <- setNames(split(tols2, seq(nrow(tols2))), rownames(tols2))

tol_res <- mclapply(tols2.list, proc_tol_cfg, mc.cores = 6)

tol_res[1:10]

tol_res_procd <- lapply(tol_res, \(x) as.list(coef(x))) %>% rbindlist()
names(tol_res_procd)[1] <- "intercept"

tol_res_procd[order(-intercept)]





