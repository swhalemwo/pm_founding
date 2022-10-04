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
