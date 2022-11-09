## * pooled descriptives based on df_reg



## df_reg$smorc_dollar_fx <- df_reg$smorc_dollar_fx/1e6

rel_vars <- c("nbr_opened" = "Number of Private Museums opened",
              "sum_core" = "Tax incentives",
              "ti_tmitr_interact" = "Marginal Income Tax Rate * Tax Incentives",
              "tmitr_approx_linear20step" = "Marginal Income Tax Rate (%)",
              "hnwi_nbr_1M" = "# HNWIs with net worth >= 1M",
              "hnwi_nbr_5M" = "# HNWIs with net worth >= 5M",
              "hnwi_nbr_30M" = "# HNWIs with net worth >= 30M",
              "hnwi_nbr_200M" = "# HNWIs with net worth >= 200M",
              "hnwi_nbr_1B" = "# HNWIs with net worth >= 1B",
              "sptinc992j_p90p100" = "Income share of top 10%",
              "sptinc992j_p99p100" = "Income share of top 1%",
              "gptinc992j" = "Gini of pre-tax income",
              "ghweal992j"= "Gini of net wealth",
              "shweal992j_p90p100" = "Wealth share of top 10%",
              "shweal992j_p99p100" = "Wealth share of top 1%",
              "smorc_dollar_fxm" = "Gvt cultural spending (millions)",
              "NY.GDP.PCAP.CDk" = "GDP per capita (thousands)",
              "SP.POP.TOTLm" = "Population (millions)",
              "cnt_contemp_1985" = "# Museums of contemporary art in 1985",
              "clctr_cnt_cpaer" = "# Collectors in Artnews collector list",
              "ln_s" = "ln(s)",
              "cons" = "cons",
              "ln_r" = "ln(r)",
              "cnt_contemp_1995" = "# of modern/contemp. art museums in 1995",
              "nbr_opened_cum" = "cumulative openings (legitimacy)",
              "nbr_opened_cum_sqrd" = "cumulative openings squared (competition)"
              )





get_more_vrbl_info <- function(x) {

    df_reg %>% select(iso3c, year, x) %>% na.omit() %>%
        mutate(vrbl = x) %>% 
        summarize(nbr_crys = n_distinct(iso3c),
                  min_time = min(year), max_time = max(year),
                  time_range = paste0(min_time, "-", max_time),
                  vrbl = unique(vrbl)) %>%
        select(vrbl, nbr_crys, time_range) %>% as.list()
}
## get_more_vrbl_info("smorc_dollar_fx")

more_vrbl_info <- rbindlist(lapply(
    intersect(names(vvs$vrbl_lbls), names(df_reg)),
    get_more_vrbl_info))


 
var_table <- describe(df_reg[,names(rel_vars)]) %>% select("Country-years" = n, mean, sd, median, min, max)
rownames(var_table) <- recode(rownames(var_table), !!!rel_vars)
var_table$`# Countries` <- more_vrbl_info$nbr_crys

var_xtbl <- xtable(var_table, caption = "main variables (all monetary variables are in or based on 2021 constant US dollars)", label = "var_desc", digits = c(0, 0, rep(2,6)))

print(var_xtbl, file = paste0(TABLE_DIR, "var_desc.tex"), include.rownames = T, hline.after =c(-1,0,7,11))

## ** some SO inspired testing of rates vs counts

## https://stats.stackexchange.com/questions/522275/controlling-for-population-size-using-per-capita-or-including-a-variable-for-po

test_df <- data.table(name = c("nebraska", "kansas", "idaho"), mort = c(5, 1, 3), sales = c(10,10, 5), pop = c(10, 1, 5)) %>%
    .[, `:=`(mort_pcap = mort/pop,
             sales_pcap = sales/pop)]


lm(mort ~ sales + pop, test_df[1:2])
lm(mort_pcap ~ sales_pcap, test_df[1:2])
lm(mort ~ sales_pcap, test_df[1:2])


ggplot(test_df, aes(x=sales_pcap, y=mort_pcap)) + geom_point()
ggplot(test_df, aes(x=sales, y=mort, size = pop)) + geom_point()

set.seed(5)
# create the variance covariance matrix
sigma<-rbind(c(1,-0.8,-0.7), c(-0.8,1, 0.4), c(-0.7,0.4,1))
# create the mean vector
mu<-c(10, 5, 2) 
# generate the multivariate normal distribution
dfx <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma)) %>% adt()
names(dfx) <- c("whatever", "dvx", "ivx")
## generate population 
dfx$pop <- rnorm(n=1000, mean = 50, sd = 10)

cor(dfx$ivx, dfx$dvx)
cor(dfx$pop, dfx$dvx)
cor(dfx$pop, dfx$ivx)

dfx[, `:=`(iv_cnts = ivx * pop, dv_cnts = dvx * pop)]

cor(dfx$iv_cnts, dfx$dv_cnts)

r_rts <- lm(dvx ~ ivx, dfx)
r_cnt <- lm(dv_cnts ~ iv_cnts + pop, dfx)

r_rts_scld <- lm(scale(dvx) ~ scale(ivx), dfx)
r_cnt_scld <- lm(scale(dv_cnts) ~ scale(iv_cnts) + scale(pop), dfx)

r_rts_pop <- lm(dvx ~ ivx + pop, dfx)

r_rts_scld_pop <- lm(scale(dvx) ~ scale(ivx) + scale(pop), dfx)

screenreg(list(r_rts, r_cnt, r_rts_scld, r_cnt_scld, r_rts_scld_pop, r_rts_pop), digits = 4)

predict(r_rts)

## ** try some poisson data
library(texreg)
library(glmmTMB)
library(ggplot2)
cor(rpois(1000, lambda = 1), rpois(1000, lambda = 1))
library(RNGforGPD)

cor_mat <- rbind(c(1,0.7, 0.3),c(0.7,1, 0), c(0.3, 0, 1))
x <- GenMVGpois(sample.size = 1000, theta.vec = c(1,1,1), no.gpois = 3, lambda.vec = c(0.5, 0.5, 0.5), cmat.star = cor_mat)
poi_dt <- adt(x$data)
setnames(poi_dt, c("dv_rt", "iv1_rt", "iv2_rt"))
cor(poi_dt$dv_rt, poi_dt$iv1_rt)

ggplot(poi_dt, aes(x=iv1_rt, y=dv_rt)) +
    geom_jitter()

poi_dt2 <- poi_dt %>% copy() %>%
    .[, pop := sample(1:10, .N, replace = T)] %>%
    .[, `:=`(dv_cnt = dv_rt*pop, iv1_cnt = iv1_rt*pop, iv2_cnt = iv2_rt*pop)]
    
poi_dt2_scld <- poi_dt2[, c("dv_cnt", lapply(.SD, scale_wo_attr)),
                        .SDcols = c("iv1_cnt", "iv2_cnt", "pop", "iv1_rt", "iv2_rt")]
poi_dt2_scld$dv_cnt <- poi_dt2$dv_cnt
poi_dt2_scld$pop_uscld <- poi_dt2$pop


r_cnts <- glm(dv_cnt ~ iv1_cnt + iv2_cnt + pop, poi_dt2, family = poisson)
r_rts <- glm(dv_cnt ~ iv1_rt + iv2_rt + offset(pop), poi_dt2, family = poisson)


r_cnts_scld <- glm(dv_cnt ~ iv1_cnt + iv2_cnt + pop, adf(poi_dt2_scld), family = poisson)
r_rts_scld <- glm(dv_cnt ~ iv1_rt + iv2_rt + offset(pop_uscld), poi_dt2_scld, family = poisson)

mdl_names <- c("r_cnts", "r_rts", "r_cnts_scld", "r_rts_scld")
mdl_list <- lapply(mdl_names, get)

screenreg(mdl_list, custom.model.names = mdl_names)



chart.Correlation(poi_dt2)

library(texreg)
screenreg(r_rts)
summary(r_cnts)
summary(r_rts)

## ** see what happens with rates
poi_dt3 <- poi_dt2

## poi_dt4 <- 

poi_dt4 <- poi_dt3 %>% copy() %>% .[, iv1_rt_sml := iv1_rt/5] %>% # add small rate
    .[, `:=`(iv1_rt_sml_sqrd = iv1_rt_sml^2, iv1_rt_sqrd = iv1_rt^2)] # add small and normal rate squared
    

poi_dt4_scld <- poi_dt4[, lapply(.SD, scale_wo_attr),
                        .SDcols = c("iv1_rt", "iv1_rt_sqrd", "iv1_rt_sml", "iv1_rt_sml_sqrd")] %>%
    cbind(poi_dt4[, .(pop, dv_cnt)])



r_rt_nol <- glm(dv_cnt ~ iv1_rt + iv1_rt_sqrd + offset(pop), poi_dt4, family = poisson) # normal rate
r_rt_sml <- glm(dv_cnt ~ iv1_rt_sml + iv1_rt_sml_sqrd + offset(pop), poi_dt4, family = poisson) # small rate

r_rt_nol_scld <- glm(dv_cnt ~ iv1_rt + iv1_rt_sqrd + offset(pop), poi_dt4_scld, family = poisson) # normal rate
r_rt_sml_scld <- glm(dv_cnt ~ iv1_rt_sml + iv1_rt_sml_sqrd + offset(pop), poi_dt4_scld, family = poisson) # small rate

mdl_names <- c("r_rt_normal", "r_rt_sml")
lapply(mdl_names, get)

mdl_list <- c(get("r_rt_nol"), get("r_rt_sml"))


screenreg(list(r_rt_nol, r_rt_sml, r_rt_nol_scld, r_rt_sml_scld))





## ** debugging lack of private museum openings even in cluster 4 (developed countries)
## kinda fixed by first summing by cluster, then dividing by population
df_reg_clstrd %>% filter(cluster == 4) %>% select(iso3c, year, nbr_opened, nbr_opened_pcap) %>%
    ## pull(nbr_opened) %>% table()
    summarize(mean_nbr_opened = mean(nbr_opened),
              mean_nbr_opened_pcap = mean(nbr_opened_pcap),
              median_nbr_opened = median(nbr_opened),
              median_nbr_opened_pcap = median(nbr_opened_pcap))

## see which countries have many contemporary art collectors per capita

## arrange(df_reg_clstrd, -clctr_cnt_cpaer_pcap) %>% select(iso3c, year, clctr_cnt_cpaer_pcap, cluster) %>%
##     filter(cluster != 4) %>% head(30) %>% distinct(iso3c)

## countrycode(c("BRB", "BHS", "CRI"), "iso3c", "country.name")

## ** earlier summary tables

## get_vlus_long <- function(cbn_dfs, df_reg, cbnx) {
##     #' 

##     dt_cys <- cbn_dfs[[cbnx]] %>% select(iso3c, year) %>% adt()

##     dtx <- df_reg %>% adt() %>% .[on=dt_cys]


##     dtx_mlt <- dtx[, c("iso3c", "year", vvs$all_rel_vars), with = F] %>%
##         melt(id.vars = c("iso3c", "year")) %>%
##         .[, cbn_name := cbnx]

##     return(dtx_mlt)
## }

## dtx_cbn <- lapply(names(cbn_dfs_rates)[1:3], \(x) get_vlus_long(cbn_dfs_rates, df_reg_rts, x)) %>%
##     Reduce(\(x,y) rbind(x,y), .)

## dtx_cbn[!dtx_cbn2, on =.NATURAL]




gen_mean_sd <- function(vlu, variable) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate mean and SD
    ## print(variable)
    
    if (any(is.na(vlu))) {
        vlu_sum <- "--"
    } else {
        vlu_sum <- sprintf("%s (%s)", nicely_fmt_number(mean(vlu)), nicely_fmt_number(sd(vlu)))
    }

    list(stat = "mean_sd", value = vlu_sum)
    
}


gen_min_max <- function(vlu) {
    #' generate min and max
    
    if (any(is.na(vlu))) {
        vlu_sum <- "--"
    } else {
        vlu_sum <- sprintf("[%s, %s]", nicely_fmt_number(min(vlu)), nicely_fmt_number(max(vlu)))
    }

    list(stat = "min_max", value = vlu_sum)
        
}

gen_placeholder <- function() {
    #' generate placeholder to keep line empty
    list(stat = "a_placeholder", value = "")}


## rbind the different statistics together 
sumry_rbinded <- rbind(
    dtx_cbn[, gen_placeholder(), by = c("variable", "cbn_name")],
    dtx_cbn[, gen_mean_sd(value), by=c("variable", "cbn_name")],
    dtx_cbn[, gen_min_max(value), by=c("variable", "cbn_name")])

## sumry_cast_long: columns are the combination
sumry_cast_long <- dcast.data.table(sumry_rbinded, variable + stat ~  cbn_name)

## sumry_cast_wide: stats are in the columns, combinations in vertical "facets"
sumry_cast_wide <- dcast.data.table(sumry_rbinded, cbn_name + variable ~ stat)


## *** wide table 
## have stats sub-column for each combination column


wide_tbl <- dcast.data.table(sumry_rbinded[stat!="a_placeholder"], variable ~ cbn_name + stat)
wide_tbl$variable

wide_tbl_lbld <- wide_tbl[dt_vrbl_lbls, variable := vrbl_lbl, on = c("variable" = "vrbl_name")]

clm_names <- list()
clm_names$pos <- list(-1, -1)
## make the headers: some hacking with hlines
clm_names$command <- c(
    paste0(paste0(c("\\hline \n ",
                    map_chr(names(cbn_dfs_rates)[1:3],
                            ~sprintf("\\multicolumn{2}{c}{%s}", .x))), collapse = " & "), " \\\\ \n"),
    paste0(paste0(c("\\hline \n ", rep(stat_dt$stat_lbl, 3)), collapse = " & "), " \\\\ \n"))


xtable(wide_tbl_lbld) %>%
    print(include.rownames = F, include.colnames = F,
          file = paste0(TABLE_DIR, "summary_stats3.tex"),
          add.to.row = clm_names,
          hline.after = c(0))


## *** long variable names


## extra_rows = list()
## extra_rows$pos <- list(0,5)
## extra_rows$command <- c("row1\n", "row2\n")

## xt_test <- xtable(adt(mtcars)[1:6, .(mpg, cyl)])
## print(xt_test, add.to.row = extra_rows, tabular.environment = 'longtable', hline.after = c(1),
##       include.colnames = F)

dt_sumry_long_names <- sumry_cast_long %>% copy() %>% .[stat != "a_placeholder"]

stat_dt <- data.table(stat_name = c("mean_sd", "min_max"), stat_lbl = c("Mean (SD)", "[Min, Max]"))
## stat_dt <- data.table(stat_name = c("mean_sd", "min_max"), stat_lbl = c("", ""))

dt_sumry_long_names[stat_dt, stat := paste0("\\hspace{3mm}", stat_lbl), on = c("stat" = "stat_name")]


## update variable labels 


vrbl_lbls <- sumry_cast_long[stat == "a_placeholder", .(variable)] %>% 
    .[extra_row_rename, vrbl_lbl := vrbl_lbl, on = c("variable" = "vrbl_name")] %>%
    .[, latexTranslate(vrbl_lbl)] # hope this doesn't break latex now


extra_rows <- list()
extra_rows$pos <- as.list(seq(0, nrow(dt_sumry_long_names)-1, by = 2))
extra_rows$command <- sprintf("\\multicolumn{4}{l}{%s} \\\\ \n", vrbl_lbls)



len(extra_rows$pos)
len(extra_rows$command)

x <- xtable(copy(dt_sumry_long_names)[,variable := NULL])


xtable(copy(dt_sumry_long_names)[,variable := NULL]) %>%
    print(add.to.row = extra_rows, hline.after = NULL,
          file = paste0(TABLE_DIR, "summary_stats2.tex"),
          tabular.environment = "longtable", include.rownames = F,
          sanitize.text.function = identity)



## *** printing 

## separate row for variable name, as in https://i0.wp.com/thatdatatho.com/wp-content/uploads/2018/08/...
x_proc1 <- sumry_cast_long %>% copy() %>% .[stat != "a_placeholder", variable := stat] %>%
    .[, stat := NULL]

## separate columns for variable name and stat
x_proc2 <- sumry_cast_long %>% copy() %>% .[stat != "a_placeholder"] %>%
    .[order(variable)] %>% .[stat != "mean_sd", variable := ""]

## try to mimic traditional summary stats layout, not working well 
x_proc3 <- sumry_cast_wide %>% copy() %>% .[, a_placeholder := NULL] %>% .[order(cbn_name, variable)] %>%
    .[variable != "Ind.tax.incentives", cbn_name := ""]




footnote <- "\\footnotesize{regular expression processing: \"!\" = NOT, \",\" = OR, \"\\$\" = end of file}"

comment <- list()
comment$pos <- list()
comment$pos[[1]] <- nrow(x_proc2)
comment$command <- c(paste0("\\hline \n \\multicolumn{2}{l}", "{", footnote, "} \n"))


print(xtable(x_proc1), 
      file = paste0(TABLE_DIR, "summary_stats.tex"), include.rownames = F,
      tabular.environment = 'longtable'
      )




## * cleaning cleaning


## ** generate the descriptives tables/figures for the update

### *** tax incentives

get_taxinc_descriptives <- function() {

    df_taxinc <- read_in_tax_incentives()


    tax_inc_descriptives <- describe(df_taxinc[tax_vars_caf], skew = F)

    tax_inc_xtbl <- xtable(
        tax_inc_descriptives[names(tax_inc_descriptives) != "vars"],
        label = "tax_inc_descriptives",
        caption = "Tax Incentive Descriptives"
    )

    print(tax_inc_xtbl,
          include.rownames = T,
          ## floating = FALSE,
          file = paste0(TABLE_DIR, "tax_inc_descriptives.tex")
          )

    res.pca.caf <- prcomp(na.omit(df_taxinc[,tax_vars_caf]),scale = T)
    df_taxinc$region <- countrycode(df_taxinc$iso3c, "iso3c", "un.regionsub.name")

    p.scree <- fviz_eig(res.pca.caf, title = "scree plot")
    p.arrows <- viz_pca_arrows(res.pca.caf, title = "factor loadings")

    pdf(paste0(FIG_DIR, "tax_inc_pca_caf.pdf"), height = 5, width = 10)
    grid.arrange(grobs = list(p.scree, p.arrows), ncol=2, as.table=F)
    dev.off()

    
    
    ind_plt <- create_ind_plot(df_taxinc, res.pca.caf, label_col = "country", title = "CAF country plot", color_col = "region")
    pdf(paste0(FIG_DIR, "tax_inc_pca_caf_ind.pdf"), height = 11, width = 19)
    plot(ind_plt)
    dev.off()
}

## get_taxinc_descriptives() 


## *** mow

get_mow_descriptives <- function() {
    
    mow_res <- get_mow_dfs()

    mow_df <- mow_res$mow_crssctn

    
    mow_cntns <- mow_res$mow_cntns

    cry_df <- as_tibble(unique(df_anls$iso3c), .name_repair = ~c("iso3c"))
    mow_df <- as_tibble(merge(cry_df, mow_df, all.x = T))

    mow_df[is.na(mow_df)] <- 0
    
    
    mow_descs <- describe(mow_df[names(mow_df) != "iso3c"], skew = F)
    mow_tbl <- xtable(mow_descs[names(mow_descs) %!in% c("vars", "se")],
                      label = "mow_descriptives",
                      caption = "Museum of the World Descriptives")

    print(mow_tbl, include.rownames=T, file = paste0(TABLE_DIR, "mow_descriptives.tex"))


    ## also add the longitudinal measures here for the argument that in recent years coverage has declined
    ## also add that many recently opened private museums are not included, such as Voorlinden, Inhotim, Long Museum


    ## maybe facets
    expand_df <- tidyr::expand(mow_cntns, year=year, iso3c=iso3c)
    mow_lines <- as_tibble(merge(expand_df, mow_cntns, all.x = T))
    mow_lines[is.na(mow_lines)] <- 0

    mow_lines_melt <- as_tibble(reshape2::melt(mow_lines, id=c("year", "iso3c")))

    max_crys <- aggregate(value ~ iso3c, mow_lines_melt, sum) %>%
        arrange(value) %>%
        top_n(8) %>%
        pull(iso3c) 
    

    library(tidyquant)
    
    mow_facets <- ggplot(filter(mow_lines_melt, iso3c %in% max_crys), aes(x=year, y=value, color = iso3c)) +
        facet_wrap(~ variable, scales = "free") + 
        geom_ma(n=5, linetype="solid") +
        scale_color_brewer(palette = "Paired") +
        labs(y="count of museum openings, 5 year rolling avg")

    
    pdf(paste0(FIG_DIR, "mow_facets.pdf"), width = 8, height = 4)
    plot(mow_facets)
    dev.off()
    
}

## get_mow_descriptives()
    
## *** HWNI

get_hnwi_descriptives <- function() {

## the table, some columns in scientific notation

    df_hwni <- get_hnwi_pcts()

    cutoff_vlus <- c(1e6, 2.5e6, 5e6, 10e6, 50e6, 100e6, 250e6,500e6)
    col_names <- unlist(lapply(cutoff_vlus, function(x) paste0("pct_cutoff_", sanitize_number(x))))

    df_hwni2 <- unique(df_hwni[which(df_hwni[,col_names] < 10),])
    hwni_descs <- describe(df_hwni2[,names(df_hwni2) %!in% c("iso3c", "region", "label", "year")], skew = F)

    hnwi_tbl <- xtable(hwni_descs[names(hwni_descs) %!in% c("vars", "se", "range")],
                       caption = "HNWI descriptives (excluding largest outliers)",
                       label = "hnwi_descs",
                       digits = c(2,2,-2, -2,2,-2)
                       )

    print(hnwi_tbl, include.rownames = T, file = paste0(TABLE_DIR, "hnwi_descs.tex"))


## want random label position
## ideally would want location on graph that places the label at a spot where there is little overlap

    df_hwni2 <- filter(df_hwni, pct_cutoff_10M < 1)

    df_hwni2$region <- countrycode(df_hwni2$iso3c, "iso3c", "un.regionsub.name")

    ## distribute labels across plot 

    label_df <- df_hwni2 %>%
        group_by(iso3c) %>%
        summarise(year = sample(year, size = 1), label = sample(iso3c,1))

    df_hwni2 <- as_tibble(merge(df_hwni2, label_df, all.x = T))

    ## spread lines across facets

    max_lines <- 12

    df_hwni2$colr <- 0

    for (i in unique(df_hwni2$region)) {
        print(i)
        ctr <- 1
        while (TRUE) {
            crys <- unique(filter(df_hwni2, region==i)$iso3c)
            ## print(len(crys))
            crys_sel <- crys[1:min(max_lines, len(crys))]
            df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$region <- paste0(i, "_", ctr)
            df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$colr <- as.numeric(factor(df_hwni2[c(df_hwni2$iso3c %in% crys_sel),]$iso3c))
            ctr <- ctr+1
            if (len(crys) <= max_lines) {
                break
            }
        }
    }



    hwni_plt <- ggplot(filter(df_hwni2, region != "Melanesia_1"), aes(x=year, y=pct_cutoff_10M, group = iso3c, color = factor(colr))) +
        facet_wrap(~ region, scales = "free", ncol = 4) +
        geom_line(size = 1, show.legend = F) +
        geom_label_repel(aes(label = label), show.legend = F, size=3) +
        scale_color_manual(values = colors_manual)
    ## scale_color_brewer(palette = "Paired")

    pdf(paste0(FIG_DIR, "hwni_curves.pdf"), width = 18, height = 13)
    plot(hwni_plt)
    dev.off()

}

## get_hnwi_descriptives()


## *** controls 
get_control_descriptives <- function() {


    controls_descs <- describe(df_reg[,c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm", "sptinc992j_p99p100", "shweal992j_p99p100", "ghweal992j", "gptinc992j", "nbr_opened_cum", "nbr_opened_cum_sqrd")], skew = F)

    controls_tbl <- xtable(controls_descs[names(controls_descs) %!in% c("vars", "se", "range")],
                           caption = "Descriptives for controls",
                           label = "controls_desc")

    print(controls_tbl, file = paste0(TABLE_DIR, "controls_desc.tex"), include.rownames=T)
}

get_all_descriptives <- function(){
    get_taxinc_descriptives()
    get_mow_descriptives()
    get_hnwi_descriptives()
    get_control_descriptives()
}

## get_all_descriptives()

## some help functions

plt_to_pdf2 <- function(plt, width, height) {


    pdf(paste0(FIG_DIR, fig_name, ".pdf"), width = width, height = height)
    plot(plt)
    dev.off()
}


## ** cluster-based analysis 

get_df_clust <- function(df_reg, vvs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate the dataframe used for clustering 

    df_clust_prep <- df_reg %>%
        filter(year >= 1995) %>% 
        select(iso3c, year, NY.GDP.PCAP.CDk, sptinc992j_p99p100, shweal992j_p99p100,
               NPO.tax.exemption, Ind.tax.incentives, cnt_contemp_1995,
               hnwi_nbr_30M, SP.POP.TOTLm) %>%
        mutate(cnt_contemp_1995 = cnt_contemp_1995/SP.POP.TOTLm,
               hnwi_nbr_30M = hnwi_nbr_30M/SP.POP.TOTLm) %>%
        select(-SP.POP.TOTLm) 


    ## pivot the columns into wider
    df_clust <- df_clust_prep %>%
        pivot_wider(id_cols = iso3c, names_from = year,
                    values_from = setdiff(names(df_clust_prep), vvs$base_vars))

    
    ## select the cross-sectional variables separately, yeet them from main df, then re-add them once
    ## (otherwise get added for every year)
    crscn_vrbls <- c("NPO.tax.exemption", "Ind.tax.incentives", "cnt_contemp_1995")
    crscn_vrbls_to_keep <- c("NPO.tax.exemption_2014", "Ind.tax.incentives_2014", "cnt_contemp_1995_1995")

    crscn_data <- df_clust %>% select(crscn_vrbls_to_keep)

    df_clust2 <- df_clust %>% select(-starts_with(crscn_vrbls)) %>%
        bind_cols(crscn_data)

    
    ## NA investigation
    ## rows: countries
    ## na.omit(df_clust2)

    ## df_clust2 %>%
    ##     mutate(nbr_nas = rowSums(is.na(.))) %>%
    ##     select(iso3c, nbr_nas) %>%
    ##     arrange(-nbr_nas) %>%
    ##     print(n=100)
    ## ## hmm there are around 20 with missing where it's worth considering to keep them (nbr_nas < 30)

    ## columns: variables

    ## colsums_na <- colSums(is.na(df_clust2))
    ## colsums_na_dt <- data.table(vrbl = names(colsums_na), nbr_na = colsums_na)
    ## colsums_na_dt[order(-nbr_na)] %>% print(n=50)
    ## colsums_na_dt[order(nbr_na)] %>% print(n=500)
    ## seems to go high quite quickly: sptinc992j, hnwi, shweal all have at least 40 missing
    ## yeeting all them would basically mean only using GDP and cross-sectional variables to cluster

    ## -> unless there's a comfy option to adjust distances to missing values, ~50 countries have to be yeeted



    return(df_clust2)
}

get_df_clust_lame <- function(df_reg, cutoffs = seq(0, 1, 0.25)) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' super lame way of clustering based on 2020 HDI cutoffs

    hdi_prep <- df_reg %>%
        filter(year == 2020) %>%
        select(iso3c, year, hdi) %>% adt()
        
    
    ## prepare for rolling join: remove last number (max)
    qntls <- quantile(hdi_prep$hdi, probs = cutoffs, na.rm = T) %>% adt() %>% .[1:.N-1]
    names(qntls) <- "hdi"
    qntls$cluster <- seq(1,nrow(qntls))
    ## qntls$asdf <- qntls$qntl
    
    ## do rolling joins work? yup
    dt_hdi_clustered <- qntls[na.omit(hdi_prep), on = "hdi", roll = Inf]

    ## test that rolling worked
    ## dt_hdi_clustered[, paste(paste0(iso3c, ":", hdi), collapse = ""), by = cluster] %>% .[order(cluster)]
    ## dt_hdi_clustered[, .(min_hdi = min(hdi), max_hdi = max(hdi)), cluster]

    return(dt_hdi_clustered)

}

get_df_clust_lame(df_reg)

    


run_cluster <- function(dists, method, nbr_clusts, na_rm) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' evaluate the clustering solution

    clusts <- hclust(dists, method = method)
    ## plot(clusts)

    ## incplt_cases <- which(!complete.cases(as.matrix(dists)))
    ## df_clust[incplt_cases,] %>% adf()

    ## as.matrix(dists)[incplt_cases,]
    
    
    tree_cutted <- cutree(clusts, k=nbr_clusts)

    clust_tbl <- table(tree_cutted)

    skew <- skewness(clust_tbl)
    mean_nbr_crys <- mean(clust_tbl)
    min_nbr_crys <- min(clust_tbl)
    max_nbr_crys <- max(clust_tbl)

    ## number of clusters with only one entry
    nbr_clusters_w_one <- len(which(clust_tbl == 1))


    return(
        list(
            nbr_clusts = nbr_clusts,
            method = method,
            skew = skew,
            mean_nbr_crys = mean_nbr_crys,
            min_nbr_crys = min_nbr_crys,
            max_nbr_crys = max_nbr_crys,
            nbr_clusters_w_one = nbr_clusters_w_one,
            na_rm = na_rm)) 
}


## just yeet Channel Islands and South Sudan (don't have anything in common)
df_clust <- get_df_clust(filter(df_reg, iso3c %!in% c("CHI", "SSD")), vvs)

dists_wna <- dist(df_clust)
dists_wona <- dist(na.omit(df_clust))

dist_options <- list(
    wna = dists_wna,
    wona = dists_wona)
    

## nrow(dists)
## ncol(dists)
## as.matrix(dists)[,2]
run_cluster(dist_options[["wna"]], "ward.D", 8, "wna")

clust_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
nbr_clusts <- seq(3,8)
na_rm <- c("wna","wona")
clust_cfg_df <- expand.grid(clust_method = clust_methods, nbr_clusts = nbr_clusts, na_rm = na_rm) %>% adt()

clust_res <- apply(clust_cfg_df, 1, \(x) run_cluster(dists = dist_options[[x[["na_rm"]]]],
                                                     method = x[["clust_method"]],
                                                     nbr_clusts = x[["nbr_clusts"]],
                                                     na_rm = x[["na_rm"]])) %>%
    rbindlist()


ggplot(clust_res, aes(x=skew, y = mean_nbr_crys, color = na_rm, size = max_nbr_crys)) +
    geom_point() +
    facet_wrap(~nbr_clusts)

## 8 clusters, wna, smallest skew

## clust_res[nbr_clusts == 8 &na_rm == "wna"]



world <- ne_countries(scale = "medium", returnclass = "sf") %>% atb() %>%
    select(iso3c = iso_a3, geometry)


assign_clusters <- function(df_clust, tree_cutted, na_rm) {
    #' assign the clustering solution to df_clust: IDK IF WORKS, not tested
    ## yeet NAs if necessary 
    if (na_rm == "wona") {
        df_clust2 <- na.omit(df_clust)
    } else {
        df_clust2 <- df_clust
    }
        
    df_clust2$clust <- tree_cutted
}



plot_world_clustered <- function(df_clustrd) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' plot visualization on map 


    world_clstrd <- left_join(world, 
                              select(df_clustrd, iso3c, cluster))
    
    plt_world_clstrd <- ggplot(world_clstrd, aes(geometry = geometry, fill = factor(cluster))) +
        geom_sf() +
        coord_sf(ylim = c(-55, 83)) +
        scale_fill_brewer(palette = "Set3")

    return(plt_world_clstrd)
}

## get_df_clust_lame(df_reg) %>% # , cutoffs = c(0, 0.4, 0.6, 0.8,1)) , cutoffs = c(0,0.5, 0.7, 0.82,1)
##     plot_world_clustered()


## construct custom versions of base functions that by default yeet nas
mean_wo_na <- function(...) {
    mean(..., na.rm = T)}
median_wo_na <- function(...) {
    median(..., na.rm = T)}
sd_wo_na <- function(...) {
    sd(..., na.rm = T)}
sum_wo_na <- function(...) {
    sum(..., na.rm = T)}


sumrz_clusters <- function(df_reg_clstrd, mean_vrbls, sum_vrbls, sum_vrbls_pure) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' summarize the clustered df_reg
    #' calculate mean and sd of interval (mean) variables
    #' convert the sum_vrbls into rates per capita (per million capita)
    #' aggregate the sum_vrbls_pure into sums

    names(mean_vrbls) <- paste0(mean_vrbls, "_mean")

    ## vector funcs 
    vc_funcs <- c(mean_wo_na, sd_wo_na, median_wo_na)
    names(vc_funcs) <- c("mean", "sd", "median")

    ## use across() to process multiple interval variables: mean and sd (custom versions)
    df_reg_clstr_smrzd <- df_reg_clstrd %>%
        group_by(cluster, year) %>%
        summarize(across(all_of(unname(mean_vrbls)), .fns = vc_funcs, .names = "{.col}_{.fn}")) %>%
        adt()
    
    
    ## need to construct manual df to rename variables back to strings 
    melt_vrbl_fctrs_dt <- data.table(vrbl_label = mean_vrbls, variable = factor(seq(1:len(mean_vrbls))))

    ## multi-column melting; for some reason converts variable names to numeric factor
    # use patterns to melt several colmns
    clstr_melt_mean_sd <- melt(df_reg_clstr_smrzd, measure = patterns("mean$", "sd$", "median$"),
                               value.name = c("mean", "sd", "median")) %>%
        ## have to recode variable with update join
        .[melt_vrbl_fctrs_dt, variable := vrbl_label, on = "variable"] %>% 
        .[, `:=`(high = mean + sd, low = mean - sd, type = "intvl")] # calculate ribbons 

    ## process count variables: into rates per capita: first shape into long, then aggregate
    clstr_melt_rates <- df_reg_clstrd %>% select(iso3c, cluster, year, sum_vrbls) %>%
        pivot_longer(cols = c(sum_vrbls), names_to = "variable") %>%
        inner_join(select(df_reg_clstrd, iso3c, year, SP.POP.TOTLm)) %>%
        na.omit() %>% # yeet countries that miss either population or value to get good rates
        group_by(year, cluster, variable) %>%
        summarize(value = sum(value), SP.POP.TOTLm = sum(SP.POP.TOTLm)) %>% # sum by cluster
        mutate(rate = value/SP.POP.TOTLm) %>% # then calculate rates
        ## rename rate to mean to fit with clstr_melt_mean_sd, also assign rate to median to have option to use it
        select(year, cluster, variable, mean=rate, median = rate) %>% 
        mutate(type = "cnt")

    ## process sum_vrbls_pure variables: just aggregate without calculating rates
    clstr_melt_cnts <- df_reg_clstrd %>% select(iso3c, cluster, year, all_of(sum_vrbls_pure)) %>%
        pivot_longer(cols = c(sum_vrbls_pure), names_to = "variable") %>%
        group_by(year, cluster, variable) %>%
        summarize(value = sum(value, na.rm = T)) %>%
        select(year, cluster, variable, mean = value, median = value) %>%
        mutate(variable = paste0(variable, "_pure"), type = "cnt_pure")
        

    clstrd_melt_cbn <- bind_rows(clstr_melt_mean_sd, clstr_melt_rates, clstr_melt_cnts)

    

    return(clstrd_melt_cbn)
}


render_cluster_means <- function(df_reg, rates) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' generate and plot with rollmean_custom some cluster means/medians
    #' should be split up later into separate functions tho:
    #' - clustering
    #' - summarizing (kinda own function, but variable specification should be argument)
    #' - plotting

    if (!rates) {
        sum_vrbls <- c("nbr_opened", "clctr_cnt_cpaer", "cnt_contemp", "smorc_dollar_fxm", "NY.GDP.TTL",
                       "hnwi_nbr_30M")
        sum_vrbls_pure <- c("SP.POP.TOTLm", "nbr_opened", "hnwi_nbr_30M")
        mean_vrbls <- c("NY.GDP.PCAP.CD", "gptinc992j", "ghweal992j", "hdi", 
                        "tmitr_approx_linear20step")

    } else if (rates) {
        sum_vrbls <- c("nbr_opened", "NY.GDP.TTL")
        sum_vrbls_pure <- c("SP.POP.TOTL", "nbr_opened")
        mean_vrbls <- c("NY.GDP.PCAP.CD", "gptinc992j", "ghweal992j", "hdi", 
                        "tmitr_approx_linear20step", "clctr_cnt_cpaer", "cnt_contemp", "smorc_dollar_fxm",
                        "hnwi_nbr_30M")

    }
    
    
    ## slice_max(df_reg_rts, order_by = clctr_cnt_cpaer, n=50) %>% select(iso3c, year, clctr_cnt_cpaer) %>%
    ##     left_join(df_reg_clstrd %>% select(iso3c, cluster)) %>% adf()
    


    df_reg_clstrd <- get_df_clust_lame(df_reg) %>%
        select(iso3c, cluster) %>%
        inner_join(df_reg, .) %>%
        filter(year >= 1995) %>%
        mutate(NY.GDP.TTL = NY.GDP.PCAP.CD * SP.POP.TOTLm)
    
    clstr_melt_mean_sd <- sumrz_clusters(df_reg_clstrd, mean_vrbls, sum_vrbls, sum_vrbls_pure)

    ## add some manual labels -> should be made more elegant, also the distinction between counts (pure) and rates 
    
    cluster_sumry_addgns <- c(
        "hdi" = "Human Development Index", 
        "hnwi_nbr_30M_pure" = "HNWI with net worth 30M USD (count)",
        "nbr_opened_pure" = "private museums openings (count)",
        "NY.GDP.PCAP.CD" = "country-avg. GDP per cap.",
        "NY.GDP.TTL" = "individual-avg. GDP per cap.",
        "SP.POP.TOTL_pure" = "population")

    plt_clstr_means <- clstr_melt_mean_sd %>%
        group_by(cluster, variable) %>%
        arrange(cluster, variable, year) %>%
        mutate(mean_ra = rollmean_custom(mean, win_len = 6)) %>%
        ggplot(aes(x=year, y=mean_ra, color = factor(cluster), fill = factor(cluster))) +
        geom_line() +
        ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
        facet_wrap(~variable, scales = "free", labeller = as_labeller(
                                                   c(cluster_sumry_addgns, vvs$vrbl_lbls))) +
        theme(legend.position = c(0.7,0.08))

    pdf(paste0(FIG_DIR, "plt_clstr_means.pdf"), width = 12, height = 7)
    plot(plt_clstr_means)
    dev.off()
}

render_cluster_means(df_reg_rts, rates = T)


## ## compare difference between median and mean (for interval variables)
## clstr_melt_mean_sd[, .(cluster, year, variable, mean, median)] %>%
##     melt(measure.vars = c("mean", "median"), variable.name = "summary_type") %>%
##     ggplot(aes(x=year, y=value, color = factor(cluster), fill = factor(cluster), linetype = summary_type)) +
##     geom_line() +
##     ## geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
##     facet_wrap(~variable, scales = "free")


## ## basic spaghetti line plotting
## ggplot(df_reg_clstrd, aes(x=year, y=gptinc992j, group = iso3c, color = factor(cluster))) +
##     geom_line()



## ** kernel 

vrbls_to_log <- c("hnwi_nbr_1M", "hnwi_nbr_5M", "hnwi_nbr_30M", "hnwi_nbr_200M",
                  "smorc_dollar_fxm", "smorc_dollar_fxm_sqrd", "NY.GDP.PCAP.CDk",
                  "SP.POP.TOTLm", "clctr_cnt_cpaer", "nbr_opened_cum", "nbr_opened_cum_sqrd",
                  "cnt_contemp_1990", "cnt_contemp_1990_squared")

## test all kind of data transformations that make 
dtx_cbn2 <- dt_splong %>% copy() %>% 
    ## .[, z := scale_wo_attr(value), by = .(variable, cbn_name)] %>%
    ## .[z > - & z < 2 & value != 0] %>% ## yeet outliers
    .[variable %in% vrbls_to_log, value := log(value+0)]


dtx_cbn2[is.na(value), .N, variable]
dtx_cbn[is.na(value), .N, variable]

library(magrittr)
library(ggridges)


ridge_years <- dtx_cbn2[variable == "gptinc992j" & cbn_name == "cbn_all"] %>% copy() %>%
    .[, .(krnl_x = density(value, na.rm = T)$x,
          krnl_y = density(value, na.rm = T)$y), by = year]
    
dt_splong[year >= 2000
          ## & variable == "smorc_dollar_fxm"
          & cbn_name == "cbn_all"
          ] %>% copy() %>% 
    .[, year := factor(year)] %>% 
    ggplot(aes(x = value, y = year, fill = NULL)) +
    geom_density_ridges() +
    facet_wrap(~variable, scales = "free")


ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges()



## raw kernels? 
dtx_cbn2 %>%
    ggplot(aes(x=value, y=..density.., group = interaction(variable, cbn_name), color = cbn_name)) +
    ## geom_histogram(color = "black", fill = "lightgrey", lwd = 0.2, bins = 30) +
    geom_density(position = "identity", show.legend = F) + 
    facet_wrap(~variable , scales = "free") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

## first calculate the kernel multipliers
krnl_mltplrs <- dtx_cbn %>% .[, .(hist_counts = hist(value, plot = F, breaks = 1)$counts[1],
                  hist_densty = hist(value, plot = F, breaks = 1)$density[1]), by=c("cbn_name", "variable")] %>%
    .[, mltplr := hist_counts/hist_densty] %>% 
    .[, .(cbn_name, variable, mltplr)]

## then calculate the kernels (with density), then scale them with joined-on multipliers
krnl_prep <- dtx_cbn %>%
    .[, .(krnl_x = density(value, na.rm = T, from = min(value, na.rm = T), to = max(value, na.rm = T))$x,
          vlu_krnl = density(value, na.rm = T, from = min(value, na.rm = T), to = max(value, na.rm = T))$y),
      by=c("cbn_name", "variable")]

krnl_mltpld <- krnl_prep %>% 
    krnl_mltplrs[on=.] %>%
    .[, vlu_mltpld := vlu_krnl * mltplr]


## original kernels
krnl_prep %>%
    ggplot(aes(x=krnl_x, y = vlu_krnl, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")



## kernels corresponding to counts (probably don't)
krnl_mltpld %>%
    ggplot(aes(x=krnl_x, y=vlu_mltpld, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")


cbn_dtn <- lapply(names(cbn_dfs)[1:3], \(x) list(cbn_name =x, cbn_n = nrow(cbn_dfs[[x]]))) %>% rbindlist()

krnl_n_scaled <- cbn_dtn[on=krnl_mltpld] %>%
    .[, vlu_n_scaled := vlu_mltpld/cbn_n]

## kernels corresponding to ratio (probably don't)
krnl_n_scaled %>%
    ggplot(aes(x=krnl_x, y=vlu_n_scaled, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")

## check whether kernel values add up properly across combinations -> THEY DON'T!!!!!
## neither original nor n-rescaled ones
krnl_prep[order(variable) ,sum(vlu_krnl), by=.(variable, cbn_name)] %>% adf()
krnl_n_scaled[order(variable) ,sum(vlu_n_scaled), by=.(variable, cbn_name)] %>% adf()


filter(krnl_prep, variable == "sptinc992j_p99p100") %>%
    ggplot(aes(x=krnl_x, y=vlu_krnl, color = cbn_name)) +
    geom_line(show.legend = F) +
    facet_wrap(~variable, scales = "free")


## gptinc992j

filter(dtx_mlt, variable == "hnwi_nbr_1M") %>% ungroup() %>% 
    ggplot(aes(x=value)) +
    ## geom_histogram(aes(y=..count..), color = "black", fill = "lightgrey") +
    geom_density(aes(y=..count..), bw = "nrd0", n=10, trim=T)

x <- stats::density(filter(dtx_mlt, variable == "hnwi_nbr_1M")$value, from = 0, bw = 0.1)
tv <- c(1e8, 2e8, 3e8, 1e7, 1e6)
tv <- c(1e3, 2e3, 3e3, 1e2, 1e1)
tv <- c(1e-2, 2e-2, 3e-2, 1e-3, 1e-4)
x <- stats::density(tv, from = 0)
plot(x[["x"]], x[["y"]], type = "l")
hist(tv, breaks = 100)



    geom_density((), color = "red")
    ## geom_histogram(aes(y=..density..), color = "black", fill = "lightgrey") +
    ## geom_density(aes(y=..density..), color = "red") 

myhist <- hist(mtcars$mpg)
multiplier <- myhist$counts / myhist$density
mydensity <- density(mtcars$mpg)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

ggplot(mtcars, aes(x=mpg, y=..count..)) +
    geom_histogram(bins=10) +
    geom_density()

