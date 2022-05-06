## * wid playground
## ** HWNI calculations

dbGetQuery(con, "show tables")

wid_wealth_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wid_v2 WHERE ilike(varx, '%weal%')"
wealth_vars <- as_tibble(dbGetQuery(con, wid_wealth_vars_cmd))

wealth_tbl <- table(wealth_vars$variable)
wealth_tbl[order(wealth_tbl)]

wealth_cmd <- "SELECT variable, percentile, value from wid_v2 where iso3c='DEU' and variable='thweal992j' and year=2000"

pctl_tbl <- table(wealth_vars$percentile)
pctl_tbl[order(pctl_tbl)]



wealth_data <- as_tibble(dbGetQuery(con, wealth_cmd))

wealth_data$pct_lo <- as.numeric(unlist(lapply(strsplit(wealth_data$percentile, split='p'), function(x) x[2])))
wealth_data$pct_hi <- as.numeric(unlist(lapply(strsplit(wealth_data$percentile, split='p'), function(x) x[3])))

wealth_data$pct_len <- wealth_data$pct_hi-wealth_data$pct_lo

ggplot(filter(wealth_data, pct_lo > 80, pct_len >=0.1), aes(xmin=pct_lo, xmax=pct_hi, ymin=0, ymax=value, alpha=0.01)) +
    geom_rect()

ggplot(filter(wealth_data), aes(x=pct_lo, y=log10(value))) +
    geom_point()

approx(wealth_data$pct_lo, wealth_data$value, xout = 98.12)
## approx can only calculate y given x, but need x given y -> RootLinearInterpolant


ggplot(df_wealth, aes(x=year, y=pct_cutoff_5M, group=iso3c, color=iso3c)) +
    geom_line()

ggplot(filter(df_wealth, pct_cutoff_5M < 5), aes(x=year, y=pct_cutoff_5M, group=iso3c, color=iso3c)) +
    geom_line()


## *** debug weird countries: leave for now


table(filter(df_wealth, pct_cutoff_5M > 2)$iso3c)
## AGO BLR COD UZB VEN 
##   4   3   5   1  25 
## much in values for vuvuzela -> maybe I have to lag the cur_df? 

filter(currency_df, iso3c=="VEN")$xlcusp999i

ggplot(filter(df_wealth, iso3c=="VEN"), aes(x=year, y=pct_cutoff, group=iso3c, color=iso3c)) +
    geom_line()


## *** try different cutoffs
df_wealth_list <- lapply(c(1e6, 2.5e6, 5e6, 10e6), function(x) get_wealth_cutoff_pct(wealth_cur_df, x))
df_wealth_cbn <- as_tibble(Reduce(function(x,y,...) merge(x,y, all=TRUE), df_wealth_list))

chart.Correlation(df_wealth_cbn[,c("pct_cutoff_1M", "pct_cutoff_2.5M", "pct_cutoff_5M", "pct_cutoff_10M")])
## quiet high correlations, idk if I have to control for time series characteristics tho 

## *** wealth inequality

chart.Correlation(wealth_ineq_df[,c("value_p90p100", "value_p95p100", "value_p99p100", "value_p99.9p100", "value_p99.99p100")])

## *** income inequality
wid_inc_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wid_v2 WHERE ilike(varx, '%inc%')"
wid_inc_vars <- as_tibble(dbGetQuery(con, wid_inc_vars_cmd))
wid_inc_vars_tbl <- table(wid_inc_vars$variable)
wid_inc_vars_tbl[order(wid_inc_vars_tbl)]

inc_ineq_vars <- c("sptinc992j", "sdiinc992j", "scainc992j")
ineq_tpls <- expand.grid(varx=inc_ineq_vars, pctl=pctls)
ineq_res <- apply(ineq_tpls, 1, function(x) get_inc_ineq(pctl=x['pctl'], varx=x['varx']))
ineq_res_anls <- rbindlist(lapply(ineq_res, function(x) list(name=names(x)[3], lenx= nrow(x))))
ineq_res_anls[order(ineq_res_anls$name),]
## -> only sptinc992j (pre-tax income) has good coverage

chart.Correlation(inc_ineq_df[,c("sptinc992j_p90p100", "sptinc992j_p95p100", "sptinc992j_p99p100", "sptinc992j_p99.9p100", "sptinc992j_p99.99p100")])
## huh correlations within income inequality less than within wealth ineqality


## *** compare income and wealth ineqality
chart.Correlation(all_ineqs[,c("value_p90p100", "value_p95p100", "value_p99p100", "value_p99.9p100", "value_p99.99p100", "sptinc992j_p90p100", "sptinc992j_p95p100", "sptinc992j_p99p100", "sptinc992j_p99.9p100", "sptinc992j_p99.99p100")])
## huh quite some correlations between income and wealth ineqality

## *** ginis

pdf(paste0(FIG_DIR, "inequalities.pdf"), width = 14, height = 8)
chart.Correlation(df_ineq[,3:ncol(df_ineq)])
dev.off()

x <- chart.Correlation(df_ineq[,3:5])


png(paste0(FIG_DIR, "inequalities.png"), width = 2000, height = 1200)
chart.Correlation(df_ineq[,3:ncol(df_ineq)])
dev.off()


png(paste0(FIG_DIR, "inequalities.png"), width = 1200, height = 800)
ggpairs(df_ineq[,3:ncol(df_ineq)])
dev.off()

pdf(paste0(FIG_DIR, "inequalities.pdf"), width = 14, height = 8)
ggpairs(df_ineq[,3:ncol(df_ineq)])
dev.off()

pca_ineq <- prcomp(na.omit(df_ineq[,c(3:ncol(df_ineq))]))
pca_inc <- prcomp(na.omit(df_ineq[,grep("ptinc", names(df_ineq))]))
pca_weal <- prcomp(na.omit(df_ineq[,grep("hweal", names(df_ineq))]))


pca_ineqall_plts <- pca_viz(pca.res = pca_ineq, title = "all")
pca_inc_plts <- pca_viz(pca.res = pca_inc, title = "income")
pca_weal_plts <- pca_viz(pca.res = pca_weal, title = "wealth")

pdf(paste0(FIG_DIR, "inequalities_pca.pdf"), width = 14, height = 10)
grid.arrange(grobs = c(pca_inc_plts, pca_weal_plts, pca_ineqall_plts), ncol=3, as.table=FALSE)
dev.off()

## *** change to market exchange rate, also use OECD FX rates to fill up
STARTING_YEAR <- 1995

currency_cmd_ppp <- paste0("select iso3c, year, value from wid_v2 where variable='xlcusp999i' and year>=", STARTING_YEAR)
currency_cmd_fx <- paste0("select iso3c, year, value from wid_v2 where variable='xlcusx999i' and year>=", STARTING_YEAR)

cur_df_ppp <- as_tibble(dbGetQuery(con, currency_cmd_ppp))
names(cur_df_ppp)[3] <- "xlcusp999i"

cur_df_fx <- as_tibble(dbGetQuery(con, currency_cmd_fx))
names(cur_df_fx)[3] <- "xlcusx999i"

cur_cpr <- as_tibble(merge(cur_df_ppp, cur_df_fx, all=T))
filter(cur_cpr, is.na(xlcusp999i))

filter(cur_cpr, is.na(xlcusx999i)) %>% pull(iso3c) %>% unique()
## 493 NAs of xlcusx on combination, 414 of xlcusp Sadge
## goes down to aroudn 138/304 when starting in 1995

cur_cbn <- as_tibble(merge(cur_cpr, oecd_cur_df, all.x = T))

## check that oecd exchange rates are same as WID
## calculate absolute, then relative difference to account for currencies with different magnitude
filter(cur_cbn, !is.na(oecd_fx)) %>%
    mutate(diff_abs = oecd_fx - xlcusx999i) %>%
    mutate(diff_rel_oecd = diff_abs/oecd_fx, diff_rel_wid = diff_abs/xlcusx999i) %>% 
    filter(diff_rel_oecd > 0.1 | diff_rel_wid > 0.1) %>%
    select(iso3c, year, diff_abs, xlcusx999i, oecd_fx, diff_rel_oecd, diff_rel_wid) 
## only 11 country-years: 1 ARG, 1 LTU, 9 SVK
## largest rel_diff is around 43%, most are 10-20% -> no order-of-magnitude diffs -> just mean? yeah why not

## *** integrate more currencies from FXTOP

##  CUB  SSD  TKM  IRQ  HND  SYR  MNE  PRK  BIH  SRB  GEO  UZB 
## 7182 4256 2394 2128 1330 1330 1064 1064  532  532  266  266 


filter(wealth_cur_df, is.na(xlcusx999i)) %>%
    group_by(iso3c, year) %>%
    select(iso3c, year) %>%
    unique() %>% as.data.frame()
## 84 country-years don't have conversion

FXTOP_DIR <- paste0(PROJECT_DIR, "data/fxtop/")

read_fxtop <- function(filename) {
    dfx <- read.csv(paste0(FXTOP_DIR, filename), sep = ";", header = F)
    dfx$iso3c <- substring(filename, 1, 3)
    dfx <- dfx[2:nrow(dfx),] ## skip first row with column names 
    dfx$value <- 1/as.numeric(dfx$V4)
    dfx$year <- as.numeric(dfx$V2)

    return(dfx %>% select(year = year, iso3c = iso3c, value = value))
}

fx_top_df <- as_tibble(Reduce(function(x,y) rbind(x,y), lapply(list.files(FXTOP_DIR), read_fxtop)))

cur_check <- rbind(
fx_top_df %>% mutate(orgn = "fxtop"),
filter(currency_df, iso3c %in% unique(fx_top_df$iso3c)) %>% select (iso3c =iso3c, year=year, value=xlcusx999i) %>% mutate(orgn = "wid"))

ggplot(cur_check, aes(x=year, y=value, color =  orgn, group = orgn)) +
    facet_wrap(~iso3c, scales = "free") +
    geom_point() + 
    geom_line()

filter(wealth_cur_df, iso3c == "TKM", percentile %in% c("p90p100", "p95p100", "p99p100")) %>%
    ggplot(aes(x=year, y=value, group = percentile, color = percentile)) +
    geom_line()



## **** check which currency Cuba uses: done

filter(fx_top_df, iso3c == "CUB") %>% as.data.frame()

p1 <- ggplot(filter(fx_top_df, iso3c == "CUB"), aes(x=year, y=as.numeric(value), color=name, group=name)) +
    geom_line()

filter(currency_df, iso3c=="CUB")
filter(wealth_cur_df, iso3c == "CUB")

p2 <- filter(wealth_cur_df, iso3c == "CUB", percentile %in% c("p90p100", "p95p100", "p99p100")) %>%
    ggplot(aes(x=year, y=value, group = percentile, color = percentile)) +
    geom_line()

grid.arrange(p1,p2)

## **** check completeness of new version of calculating wealth with price index
filter(wealth_cur_df, is.na(inyixx999i)) %>% pull(iso3c) %>% unique()
filter(wealth_cur_df, is.na(inyixx999i)) %>% select(iso3c, year) %>% unique() %>% as.data.frame()

filter(cur_df_wide, is.na(inyixx999i)) %>% select(iso3c, year) %>% unique()
filter(cur_df_wide, iso3c=="CHN") %>% as.data.frame()



filter(wealth_cur_df, is.na(xlcusx999i)) %>% pull(iso3c) %>% table() %>% sort(decreasing = T)

### **** compare new and old wealth constructions

get_wealth_df_old <- function() {

    #' get the basic wealth df (in USD PPP)
    #' store as global so I don't have to reload everything from CH if I want to change cutoff
    #' use threshold for now (easiest)

    wealth_cmd_all <- paste0("select iso3c, variable, percentile, year, value from wid_v2 where variable='thweal992j' and year >=", STARTING_YEAR)

    wealth_df <- as_tibble(dbGetQuery(con, wealth_cmd_all))

    currency_cmd <- paste0("select iso3c, year, value from wid_v2 where variable='xlcusx999i' and year>=", STARTING_YEAR)
    currency_df <- as_tibble(dbGetQuery(con, currency_cmd))
    names(currency_df)[3] <- "xlcusp999i"

    ## ggplot(filter(currency_df, xlcusp999i < 10), aes(x=year, y=xlcusp999i, color=iso3c)) +
    ##     geom_line()


    wealth_cur_df <- as_tibble(merge(wealth_df, currency_df, all.x = T))
    wealth_cur_df$wealth_cur <- wealth_cur_df$value/wealth_cur_df$xlcusp999i

    ## there are still quite some NAs
    ## table(filter(wealth_cur_df, is.na(wealth_cur))$iso3c)
    ## North Korea, South Sudan
    ## nobody cares bro 

    wealth_cur_df$pct_lo <- as.numeric(unlist(lapply(strsplit(wealth_cur_df$percentile, split='p'), function(x) x[2])))

    return(na.omit(wealth_cur_df))
}

wealth_df_old <- get_wealth_df_old()
wealth_df <- get_wealth_df()

dfx <- merge(
filter(wealth_df_old, percentile == "p90p100") %>% select(iso3c, year, wealth_cur),
filter(wealth_df, percentile == "p90p100") %>% select(iso3c, year, wealth_usd21 =wealth_usd21_wo_price_index)) %>%
    mutate(diff = wealth_cur - wealth_usd21) %>% as_tibble() %>%
    mutate(diff_cpr = wealth_usd21/wealth_cur)

filter(dfx, iso3c == "DEU") %>% as.data.frame()

wealth_df <- get_wealth_df()

filter(wealth_df, percentile == "p99p100") %>%
    mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>%
    filter(region == "Europe") %>% 
    viz_lines(facets = "region", grp = "iso3c", y="wealth_usd21", max_lines = 4)

## hmm NLD/DEU/SVK: still difference in 2021, there shouldn't be one tho: price index is 1, currency conversion is the same
## need to check that tomorrow


dfx %>% mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>% 
viz_lines(y="pct_cutoff_30M", facets = "region")


## *** also check uniqueness of WID country-years: done 
## cur_df_fx/ppp %>% mutate(x=1) %>% group_by(iso3c, year) %>% mutate(sumx = sum(x)) %>% pull(sumx) %>% max()

## ggplot(filter(wealth_cur_df, iso3c=="DEU", year==2000), aes(x=pct_lo, y=log10(value))) +
##     geom_point()


## ** visualizing cutoff lines, Russia weird


df_hwni_melt <- as_tibble(reshape2::melt(df_hwni, id=c("iso3c", "year")))

ggplot(df_hwni_melt, aes(x=year, y=value, color = interaction(iso3c, variable))) +
    geom_line(show.legend = FALSE, alpha=0.2)

table(filter(df_hwni_melt, value > 10)$iso3c)

filter(df_hwni_melt, iso3c=="RUS", value > 10)
get_wealth_cutoff_pct(filter(wealth_cur_df, iso3c=="RUS", year == 1995), cutoff = 1e6)

ggplot(filter(wealth_cur_df, iso3c=="RUS", year == 1995), aes(x=pct_lo, y=log10(wealth_cur))) +
    geom_line()

ggplot(filter(wealth_cur_df, iso3c=="RUS"), aes(x=pct_lo, y=log10(wealth_cur), group=year, color=year)) +
    geom_line() +
    scale_color_gradient(low="blue", high = "red")

ggplot(filter(df_hwni_melt, iso3c=="DEU"), aes(x=year, y=log10(value), group=variable, color=variable)) +
    geom_line() 



pdf(paste0(FIG_DIR, "russian_weirdness.pdf"), width = 9, height = 6)
ggplot(filter(df_hwni_melt, iso3c %in% c("DEU", "RUS", "USA", "CHN", "NLD", "CHE") & year >=1995),
       aes(x=year, y=log10(value))) +
    facet_wrap(~ variable) + 
    geom_line(aes(color=iso3c), size=1) + 
    labs(title = "logged percentage of people above cutoff, Russia declining everywhere") +
    scale_color_brewer(palette="Dark2")
dev.off()






## ** debugging spanish thweal992j
df_wealth_cur_esp <- filter(wealth_cur_df, iso3c=="ESP")

df_wealth_esp_re <- df_wealth_cur_esp %>%
    group_by(year) %>%
    do(wealth_cutoff(.$pct_lo, .$wealth_cur, cutoff_amt =cutoff))

df_wealth_cur_esp95 <- filter(df_wealth_cur_esp, year==1995)


ggplot(filter(df_wealth_cur_esp95, log10(value) > 4.7 & log10(value) < 5.5), aes(x=pct_lo, y=log10(value))) +
    geom_point()

df_wealth_esp <- filter(df_wealth, iso3c=="ESP")

## df_wealth
print(paste("ESP", cutoff, nrow(filter(df_wealth, iso3c=="ESP"))))

df_wealth_cur_deu95 <- filter(wealth_cur_df, iso3c=="DEU" & year==1995)
df_wealth_cur_usa95 <- filter(wealth_cur_df, iso3c=="USA" & year==1995)


as.data.frame(df_wealth_cur_deu95[order(df_wealth_cur_deu95$percentile),])
as.data.frame(df_wealth_cur_usa95[order(df_wealth_cur_usa95$percentile),])

wealth_cur_df_plt <- filter(wealth_cur_df, year==1995 & iso3c %in% c("DEU", "USA", "NLD", "PRT", "FRA", "ESP", "CHN", "BRA"))

ggplot(wealth_cur_df_plt, aes(x=pct_lo, y=log10(value), color=iso3c)) +
    geom_line()


## ** debugging high thresholds

wealth_cur_df <- get_wealth_df()


df_wealth_10m <- get_wealth_cutoff_pct(na.omit(wealth_cur_df), 10e6)
df_wealth_50m <- get_wealth_cutoff_pct(na.omit(wealth_cur_df), 50e6)
df_wealth_100m <- get_wealth_cutoff_pct(na.omit(wealth_cur_df), 100e6)

## hmm coverage is better but still not identical, which it should be
setdiff(unique(df_wealth_10m[,c("iso3c", "year", "pct_cutoff_10M")]), unique(df_wealth_100m[,c("iso3c", "year", "pct_cutoff_100M")]))

unique(df_wealth_10m[,c("iso3c", "year")])
unique(df_wealth_100m[,c("iso3c", "year")])

df_wealth_10m$cnt <- 1
df_wealth_100m$cnt <- 1

filter(as_tibble(aggregate(cnt ~ iso3c + year, df_wealth_10m, sum)), cnt > 1)
filter(as_tibble(aggregate(cnt ~ iso3c + year, df_wealth_100m, sum)), cnt > 1)



filter(wealth_cur_df, iso3c=="PRK")$wealth_cur
## PRK is full of NAs




## ** pretty printing/export function for WID completeness check tables
    wealth_res_df2 <- wealth_res_df2[,c('variable', 'variable_label', 'PMs_covered_raw', 'cry_cvrg_geq3', 'nbr_of_crys_geq3', 'nbr_of_crys_geq1pm')]

    names(wealth_res_df2) <- c("variable", "meaning", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")


xtbl <- xtable(
        wealth_res_df2,
        label="wid_cpltns",
        caption = "coverage variables for top decile in WID",
        align = c("p{2cm}", "l", "p{5.5cm}","p{1.5cm}", rep("p{2.25cm}", 3)),
        digits=0)



    print(
        xtbl,
        include.rownames = F,
        file = paste0(TABLE_DIR, "wid_wealth_cpltns.tex"),
        )

## ** variable completeness checks across percentiles




## **** middle classes
## ***** pre-checking the variables to do, not much decrease
wid_inc_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wid_v2 WHERE ilike(varx, '%inc%' )"
wid_inc_vars <- as_tibble(dbGetQuery(con, wid_inc_vars_cmd))
unique(wid_inc_vars)

perc_splits <- as_tibble(do.call(rbind.data.frame, strsplit(wid_inc_vars$percentile, "p")))[,c(2,3)]
names(perc_splits) <- c("perc_low", "perc_high")

wid_inc_vars$perc_low <- as.numeric(perc_splits$perc_low)
wid_inc_vars$perc_high <- as.numeric(perc_splits$perc_high)
wid_inc_vars

filter(wid_inc_vars, perc_high == 100)

filter(wid_inc_vars, perc_high < 100)$percentile

inc_table <- table(wid_inc_vars$percentile)
inc_table[rev(order(inc_table))][c(0:100)]

wid_inc_vars$diff <- mutate(wid_inc_vars, diff=perc_high-perc_low)$diff
hist(wid_inc_vars$diff, breaks=50)

ggplot(filter(wid_inc_vars, perc_high < 100 & diff < 30), aes(y=perc_high, x=perc_low)) +
    geom_jitter(width=3, height=3, size=0.2)

## most percentiles are about very small steps
## -> need even additional step of seeing how well variables are covered in terms of percentiles
##

list_of_ranges <- list(c(1,40), c(30,40), c(80,90))


x <- head(wid_inc_vars)$percentile

split_percs <- function(percentile){
    
    print('------')
    print(length(percentile))
    perc_splits2 <- do.call(rbind.data.frame, strsplit(percentile, "p"))[c(2,3)]

    
    names(perc_splits2) <- c("low", "high")
    perc_splits2 <- apply(perc_splits2, 2, as.numeric)

    print(perc_splits2)

    nrox <- nrow(perc_splits2)
    groups <- seq(nrox)
    print(nrox)
    print(groups)
    if (length(percentile) ==1) {
        list_of_ranges <- list(perc_splits2)
    } else {
        list_of_ranges <- split(perc_splits2, groups)
    }
    return (list_of_ranges)
    }
    
split_percs("p99p100")

split_percs(c("p99p100","p1p2"))



check_ranges <- function(list_of_ranges){
    #' see how well a variable is covered
    cvrg <- length(which(c(0:100) %in% unique(unlist(lapply(list_of_ranges, function(x) (c(x[1]:x[2])))))))
    ## print(cvrg)
    nbr_ranges <- length(list_of_ranges)
    ## print(nbr_ranges)
    avg_len <- mean(unlist(lapply(list_of_ranges, function(x) x[2]-x[1])))
    ## print(avg_len)
    return(list(cvrg = cvrg,
           nbr_ranges = nbr_ranges,
           avg_len = avg_len))
    }

list_of_ranges <- split_percs(wid_inc_vars$percentile[c(0:5)])
check_ranges(list_of_ranges)

        
check_cvrg <- function(percentile){
    list_of_ranges <- split_percs(percentile)
    cvrg_res <- check_ranges(list_of_ranges)
    ## print(list_of_ranges)
    print(cvrg_res)
    ## return(paste0('c', 'b'))
    return(paste0(cvrg_res, collapse="--"))
    
}

check_cvrg(list("p0p30"))


aggregate(percentile ~ variable, filter(wid_inc_vars, variable == "sptinc992j", diff < 20), check_cvrg)

wid_inc_var_cvrg <- as_tibble(aggregate(percentile ~ variable, filter(wid_inc_vars, diff < 20), check_cvrg))

## splitting results back, assigning to results
wid_inc_var_cvrg_split <- apply(do.call(rbind, strsplit(wid_inc_var_cvrg$percentile, '--')), 2, as.numeric)
wid_inc_var_cvrg$cvrg <- wid_inc_var_cvrg_split[,1]
wid_inc_var_cvrg$nbr_ranges <- wid_inc_var_cvrg_split[,2]
wid_inc_var_cvrg$avg_len <- wid_inc_var_cvrg_split[,3]
wid_inc_var_cvrg <- wid_inc_var_cvrg[,-c(2)]
filter(wid_inc_var_cvrg, cvrg > 80)$cvrg
## down from 62 to 39


filter(wid_inc_vars, variable %in% filter(wid_inc_var_cvrg, cvrg > 80)$variable)
## hmm ok not that great a reduction: from 12.4k to 10.7k

## fuck i'll still have to check all the subscale variables separately
## but can still help to do variable completeness first to reduce number of sub-variables I have to run
## also probably similar infrastructure

## ***** actual running the checks
    
combos_inc <- split(wid_inc_vars, seq(nrow(wid_inc_vars)))
inc_res <- mclapply(combos_inc, wid_cpltns_check_wrapper, mc.cores = 4)

inc_res_df <- as_tibble(apply(Reduce(function(x,y,...) rbind(x,y,...), inc_res), 2, unlist))
    ## sloppy converting numbers back to numeric

inc_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")] <- apply(inc_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")], 2, as.numeric)

## write.csv(inc_res_df, file = paste0(TABLE_DIR, "inc_res_df.csv"))

ggplot(inc_res_df, aes(x=variable, y=PMs_covered_raw)) +
    geom_boxplot() 
    ## geom_jitter(width = 0.1, size=0.5)
## ok there are like 11 variables that have good coverage
## hope they have good coverage in terms of percentiles covered

inc_res_tbl <- aggregate(PMs_covered_raw ~ variable, inc_res_df, mean)
ttl_cvrg_good <- filter(inc_res_tbl, PMs_covered_raw > 400)$variable

pctl_cvrg_good <- filter(wid_inc_var_cvrg, cvrg > 80)$variable

intersect(ttl_cvrg_good, pctl_cvrg_good)
## all about pre-tax income
## but there I have average, share and thresholds -> should be sufficient to get some estimates of middle class


check_wid_cpltns("sptinc992j", "p60p70")    

                    
## **** check whether coverage depends on percentile chosen, doesn't really

pctl_checker <- function(percentile){
    #' next level wrapper function: just use shr variables
    wdi_cpltns_x <- lapply(as.list(shr_variables), check_wdi_cpltns, percentile)
    wdi_res_df <- do.call(rbind.data.frame, wdi_cpltns_x)
    wdi_res_df$percentile <- percentile
    
    return(wdi_res_df)
}

## pctl_checker("p99.5p100")


if (REDO_WDI_CPLTNS_CHK){
    ## could have been that some variables are good at some exotic percentages, but they aren't
    ## sptinc992j still the only variable with decent coverage 

    percentiles <- as_tibble(dbGetQuery(con, "select percentile, count(percentile) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    pctls_cry <- as_tibble(dbGetQuery(con, "select percentile, count(distinct(country)) as cnt from wdi where first_letter='s' group by percentile order by cnt desc"))

    ## just select a bunch of percentiles for coverage checks?
    ## nah that would be stupid, there could be some good coverage in some that i'd miss
    ## use all that have first percentile above 80 and second == 100

    pctls_cry$pct1 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][2]})))
    pctls_cry$pct2 <- as.numeric(unlist(lapply(pctls_cry$percentile, function(x) {strsplit(x, "p")[[1]][3]})))

    pctls_relevant <- filter(pctls_cry, pct1 >=80 & pct2==100)$percentile



    pctl_cpltns_res <- mclapply(pctls_relevant, pctl_checker, mc.cores = 6)
    
    pctl_cpltns_df <- as_tibble(Reduce(function(x,y,...) rbind(x,y,...), pctl_cpltns_res))

    as.data.frame(filter(pctl_cpltns_df, PMs_covered_raw > 250))[,-c(which(names(wdi_df_res_90) == "most_affected_crys"))]

    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, max)
    aggregate(PMs_covered_raw ~ variable, pctl_cpltns_df, mean)

    as.data.frame(filter(pctl_cpltns_df, variable == "sptinc992j"))
}



## same with share data: only sufficient global coverage for ptinc992j, diinc992j second 

## dbDisconnect(con)

## dbListTables(con)

## checking whether i don't accidentally skip some country-years, apparently not
## aggregate(year ~ country, multi_inner, max)
## aggregate(year ~ country, multi_inner, min)
## aggregate(year ~ country, multi_inner, len)




## ** old WID completeness checks

## *** WDI completeness checks

## **** gini variables
if (REDO_WDI_CPLTNS_CHK == TRUE){
    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")
    gini_variables <- dbGetQuery(con, "select distinct(variable) from wdi where first_letter=='g'")

    gini_data <- as_tibble(dbGetQuery(con, "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where first_letter='g'"))

    table(gini_data$percentile)
    check_wdi_cpltns("gptinc992j", "p0p100")

    gini_res <- lapply(as.list(gini_variables$variable), check_wdi_cpltns, "p0p100")
    gini_res_df <- do.call(rbind.data.frame,gini_res)
}


## **** income

x <- tbl(con, "wdi") %>%
    filter(varx == "hweal") %>%
    group_by(percentile) %>%
    summarise(length(percentile))



## could add something like how complete the countries are that are not fully complete to assess how difficult imputation will be 




REDO_WID_CPLTNS_CHK <- FALSE
if (REDO_WID_CPLTNS_CHK){


    check_wid_cpltns("wealg")
    check_wid_cpltns("labsh")
    check_wid_cpltns("wealp")
    check_wid_cpltns("weali")
    check_wid_cpltns("wealc")
    check_wid_cpltns("wealh")
    check_wid_cpltns("sfiinc992i", "p90p100")
    check_wid_cpltns("sfiinc992j", "p90p100")

    con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

    shr_variables_p90 <- dbGetQuery(con, "select distinct(variable) from wid_v2 where percentile='p90p100' and first_letter='s'")[[1]]
    wid_cpltns_res_90 <- lapply(as.list(shr_variables_p90), check_wid_cpltns, "p90p100")
    wid_df_res_90 <- do.call(rbind.data.frame, wid_cpltns_res_90)
    wid_df_res_90_tbl <- wid_df_res_90[,-c(which(names(wid_df_res_90) == "most_affected_crys"))]
    names(wid_df_res_90_tbl)[c(2:5)] <- lapply(names(wid_df_res_90_tbl)[c(2:5)], function(x) paste0(x, "_90"))


    shr_variables_p99 <- dbGetQuery(con, "select distinct(variable) from wdi where percentile='p99p100' and first_letter='s'")[[1]]
    wid_cpltns_res_99 <- lapply(as.list(shr_variables_p99), check_wid_cpltns, "p90p100")
    wid_df_res_99 <- do.call(rbind.data.frame, wid_cpltns_res_99)
    wid_df_res_99_tbl <- wid_df_res_99[,-c(which(names(wid_df_res_99) == "most_affected_crys"))]
    names(wid_df_res_99_tbl)[c(2:5)] <- lapply(names(wid_df_res_99_tbl)[c(2:5)], function(x) paste0(x, "_99"))

    wid_tbl <- merge(wid_df_res_90_tbl, wid_df_res_99_tbl, by=c("variable"))


    ## manual check that 99% percentile is as good as 90% percentile 
    wid_tbl[,c("variable", "PMs_covered_raw_90", "PMs_covered_raw_99",
               "cry_cvrg_geq3_90", "cry_cvrg_geq3_99",
               "nbr_of_crys_geq3_90", "nbr_of_crys_geq3_99",
               "nbr_of_crys_geq1pm_90", "nbr_of_crys_geq1pm_99")]
    ## but don't go further into putting it into table
                  

    wid_df_res_90_tbl <- wid_df_res_90_tbl[rev(order(wid_df_res_90_tbl$PMs_covered_raw))[c(1:10)],]
    names(wid_df_res_90_tbl) <- c("variable", "PM foundings\n covered directly", "PM foundings in countries with data for at least 3 years", "number of countries with data for at least 3 years", "number of countries with data and at least 1 PM founding")

    wid_df_res_90_tbl$variable <- recode(wid_df_res_90_tbl$variable, 
           "sptinc992j" = "pretax income (equal-split adults = based on household)",
           "sdiinc992j" = "post-tax income (equal-split adults)",
           "scainc992j"= "post-tax disposable income (equal-split adults)",
           "sfiinc992t"= "fiscal income (threshold)",
           "sfiinc992j"= "fiscal income (equal-split adults)",
           "sfiinc992i"= "fiscal income (individuals)",
           "shweal992j"= "net wealth (equal-split adults)",
           "sptinc992i"= "pretax income (individuals)",
           "sdiinc992i"= "post-tax income (individuals)",
           "sfiinc999t"= "fiscal income (threshold)")
    
    xtbl <- xtable(
        wid_df_res_90_tbl,
        label="wid_cpltns",
        caption = "coverage variables for top decile in WID",
        align = c("l", "p{5.5cm}","p{2cm}", rep("p{3cm}", 3)),
        digits=0)

    

    print(
        xtbl,
        include.rownames = F,
        file = paste0(TABLE_DIR, "wid_cpltns.tex"),
        )

}

## **** wealth 

wid_cpltns_check_wrapper <- function(combo){
    ## print(combo)
    return(check_wid_cpltns(combo$variable, combo$percentile))
}


REDO_WID_WEALTH_CHECKS <- FALSE
if (REDO_WID_WEALTH_CHECKS){

    wid_wealth_vars_cmd <- "SELECT DISTINCT(variable), percentile FROM wdi WHERE ilike(varx, '%weal%' )"
    wid_wealth_vars <- as_tibble(dbGetQuery(con, wid_wealth_vars_cmd))
    unique(wid_wealth_vars)

    check_wid_cpltns("mgweal999i", "p0p100")

    combos <- split(wid_wealth_vars, seq(nrow(wid_wealth_vars)))


    check_wid_cpltns(combos[[1]][[1]][1], combos[[1]][[1]][2])
    wid_cpltns_check_wrapper(combos[[1]])

    wealth_res <- mclapply(combos, wid_cpltns_check_wrapper, mc.cores = 6)
    wealth_res_df <- as_tibble(apply(Reduce(function(x,y,...) rbind(x,y,...), wealth_res), 2, unlist))
    ## sloppy converting numbers back to numeric
    wealth_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")] <- apply(wealth_res_df[c("PMs_covered_raw", "cry_cvrg_geq3", "nbr_of_crys_geq3", "nbr_of_crys_geq1pm")], 2, as.numeric)


    wealth_res_df2 <- as.data.frame(filter(wealth_res_df, PMs_covered_raw > 250))[,-c(which(names(wealth_res_df) %in% c("most_affected_crys", "percentile")))]

    wealth_res_df2$variable_label <- recode(wealth_res_df2$variable,
                                            "apweal999i" = "average individual wealth of combined sector (households, NPISH)",
                                            "anweal999i" = "average individual wealth of national economy",
                                            "wwealn999i" = "wealth-to-income ratio of national economy",
                                            "mpweal999i" = "total wealth of combined sector",
                                            "apweal992i" = "average individual wealth of combined sector (above 20 y/o)",
                                            "mgweal999i" = "total net wealth of general government",
                                            "wwealg999i" = "wealth-to-income ratio of national economy",
                                            "wwealp999i" = "net private wealth to net national income ratio",
                                            "anweal992i" = "average individual wealth of national economy (above 20 y/o)",
                                            "agweal999i" = "average individual net wealth of general government",
                                            "mnweal999i" = "total individual wealth of national economy",
                                            "agweal992i" = "average individual net wealth of general government")

}
## ** previous tests

## **** done: check how DEU has shweal992j, but only p0p100 percentiles: is because metadata is for all countries even if they don't have that data, actual data only there for handful of countries
## wealth_check <- as_tibble(DBI::dbGetQuery(con, "select distinct(varx) as varx from wdi where country='DEU' and varx like '%weal%'"))


## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where varx like '%weal%' and country = 'DEU' and year >= 1985"))

## res <- as_tibble(DBI::dbGetQuery(con, "select variable, percentile, year, varx, value from wdi where first_letter='s' and country = 'DEU' and year >= 1985"))


## table(res$variable, res$percentile)



## **** average vs share check

## see if shares are always there when there are averages, turns out it's the other way around: there are shares, but not always the averages to compute them
## -> sticking to shares is fine: more complete and easier to handle

## con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", db = "org_pop")

## cmd_cplt_check <- "select country as countrycode, variable, percentile, year, first_letter, varx, value from wdi where (percentile='p90p100' or percentile='p0p90') and year>=1985"

## res_cplt_check <- as_tibble(dbGetQuery(con, cmd_cplt_check))

## res_s_p90p100 <- filter(res_cplt_check, first_letter=="s" & percentile=="p90p100")
## res_a_p0p90 <- filter(res_cplt_check, first_letter=="a" & percentile=="p0p90")
## res_a_p90p100 <- filter(res_cplt_check, first_letter=="a" & percentile=="p90p100")
## res_a_mrg <- as_tibble(merge(res_a_p0p90, res_a_p90p100, by=c("countrycode", "variable", "year")))
## res_a_mrg$ratio <- res_a_mrg$value.y/(9*res_a_mrg$value.x + res_a_mrg$value.y)

## res_a_mrg_cut <- res_a_mrg[,c("countrycode", "variable", "year", "ratio")]
## res_a_mrg_cut$varx <- res_a_mrg$varx.x


## ## there are only 16k with top 10, but 1.2m with entire population

## ## make ratio
## ## actually average is not good for making top 10% concentration ratios: average of p0p100 is lower than p90p100 -> that's why ratio was negative
## ## would either need total of p90p100 and p0p100
## ## or average of p0p90 -> can calculate top10 ratio as p90p100/(9*p0p90 + p90p100)
## ## maybe first casting is better? then column filtering is easier 

## table(res_s_p90p100$variable, res_s_p90p100$varx)
## table(res_a_mrg_cut$variable, res_a_mrg_cut$varx)
## ## varx only differ from variables in that variables sometimes are measured on different levels of analysis
## ## -> can do varx2: only remove the first letter
## res_s_p90p100$varx2 <- substring(res_s_p90p100$variable, 2, 100)
## res_a_mrg_cut$varx2 <- substring(res_a_mrg_cut$variable, 2, 100)


## cpr_shrs <- as_tibble(merge(res_s_p90p100, res_a_mrg_cut, by=c("countrycode", "varx2", "year")))
## cor(cpr_shrs$ratio, cpr_shrs$value, use = "complete.obs")
## ## nice now correlation is pretty much 1
## ## res_s_p90p100 has more observations (14.7k) than res_a_mrg_cut (11.9k), res_a_mrg_cut also has 90 NAs
## ## which observations (countries, variables, years) have shares but not complete averages?
## ## multiple aggregations

## agg_s_year <- as.data.frame(table(res_s_p90p100$year))
## names(agg_s_year) <- c("year", "share_cnt")
## agg_r_year <- as.data.frame(table(res_a_mrg_cut$year))
## names(agg_r_year) <- c("year", "ratio_cnt")

## year_comparison <- merge(agg_s_year, agg_r_year, using = c("year"))
## year_cprn_melt <- melt(year_comparison, id="year")

## ggplot(year_cprn_melt, aes(x=year, y=value, group=variable, color=variable)) +
##     geom_line()
## ## tbh differences are not that big, maybe 20-30 per year  -> 10% difference

## res_s_p90p100[,c("countrycode", "variable", "year")]

## ## could combine, remove all that occur twice

## s_r_cbin <- rbind(res_s_p90p100[,c("countrycode", "varx2", "year")], res_a_mrg_cut[,c("countrycode", "varx2", "year")])
## s_r_cbin$ctr <- 1

## sr_cbin_agg <- as_tibble(aggregate(ctr ~ countrycode + varx2 + year, s_r_cbin, sum))
## plot(table(filter(sr_cbin_agg, ctr==1)$year), type='l')
## ## year: seems kinda stable, but decreaes in recent years
## table(filter(sr_cbin_agg, ctr==1)$varx2)

## ## most differences in variables about "fiinc": fiscal income: there in share, but not in average
## ## fiinc992i
## ## fiinc992j
## ## fiinc992t

## ## also some in others
## ## diinc992i
## ## diinc992t
## ## fiinc999t
## ## hweal992i
## ## ptinc992j 
## table(filter(sr_cbin_agg, ctr==1)$countrycode)

## cry_cnt_a <- as.data.frame(table(res_a_mrg_cut$countrycode))
## names(cry_cnt_a) <- c("countrycode", "count")
## cry_cnt_a$countrycode <- as.character(cry_cnt_a$countrycode)
## cry_cnt_a$condition <- "ratio"

## cry_cnt_s <- as.data.frame(table(res_s_p90p100$countrycode))
## names(cry_cnt_s) <- c("countrycode", "count")
## cry_cnt_s$countrycode <- as.character(cry_cnt_s$countrycode)
## cry_cnt_s$condition <- "share"

## cry_cnt_cbn <- rbind(cry_cnt_a, cry_cnt_s)

## ggplot(cry_cnt_cbn, aes(x=countrycode, fill=condition, y=count)) +
##     geom_bar(position = "dodge", stat="identity")

## ## most countries have just 35 observations, differences is only for the countries that have more: more shares than ratios
