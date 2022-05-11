all_lngtd_vars <- c("tmitr_approx_linear_2020step",
                    "hnwi_nbr_30M",
                    "gptinc992j",
                    "ghweal992j",
                    "smorc_dollar_fx",
                    "NY.GDP.PCAP.CDk",
                    "SP.POP.TOTLm",
                    "clctr_cnt_cpaer"
                    )

base_vars <- c("iso3c", "year")
crscn_vars <- c("sum_core", "cnt_contemp_1995")

reg_vars <- c(base_vars, all_lngtd_vars, crscn_vars)

vrbl_cnbs <- list(
    all_vars = all_lngtd_vars,
    no_cult_spending = all_lngtd_vars[all_lngtd_vars != "smorc_dollar_fx"],
    no_cult_spend_and_mitr = all_lngtd_vars[all_lngtd_vars %!in% c("smorc_dollar_fx", "tmitr_approx_linear_2020step")],
    controls = c("NY.GDP.PCAP.CDk", "SP.POP.TOTLm"))

## sketching functionalization


hnwi_vars <- sapply(hnwi_cutoff_vlus, \(x) paste0("hnwi_nbr_", sanitize_number(x)))
inc_ineq_vars <- c("sptinc992j_p90p100", "sptinc992j_p99p100", "gptinc992j")
weal_ineq_vars <- c("shweal992j_p90p100", "shweal992j_p99p100", "ghweal992j")



vrbl_choices <- expand.grid(hnwi_var = hnwi_vars,
            inc_ineq_var = inc_ineq_vars,
            weal_ineq_var = weal_ineq_vars, stringsAsFactors = F) %>%
    atb()

## could also just sample here 
x <- vrbl_choices[1,]

## select variable, generate random lag

lngtd_vars <- c("tmitr_approx_linear_2020step",
               x$hnwi_var,
               x$inc_ineq_var,
               x$weal_ineq_var,
               "smorc_dollar_fx",
               "NY.GDP.PCAP.CDk",
               "SP.POP.TOTLm",
               "clctr_cnt_cpaer") %>% atb() %>%
    select(vrbl = value) %>%
    group_by(vrbl) %>% 
    mutate(lag = sample(seq(1,5),1))


gen_lag <- function(vrbl, lag) {
    #' lag vrbl by lag years

    lag <- as.numeric(lag)
    lag_name <- paste0(vrbl, "_lag", lag)

    df_lag <- df_reg %>% select(iso3c, year, !!vrbl) %>%
        group_by(iso3c) %>%
        mutate(!!lag_name := lag(get(vrbl), lag)) %>%
        select(iso3c, year, !!lag_name)
    return(df_lag)
}

## gen_lag(vrbl = "SP.POP.TOTLm", lag=4)

lag_df_list <- apply(lngtd_vars, 1, \(x) gen_lag(vrbl=x[["vrbl"]], lag=x[["lag"]]))

## lag TI*TMITR interaction with same lag as TMITR
lag_df_list[[len(lag_df_list)+1]] <- gen_lag("ti_tmitr_interact", filter(lngtd_vars, vrbl == "tmitr_approx_linear_2020step")$lag)


lag_df <- Reduce(\(x,y) merge(x,y), lag_df_list) %>% atb()


df_reg_lags <- atb(merge(df_reg, lag_df))

# can skip base vars here, are provided by lag_df
all_var_names <- c(crscn_vars, names(lag_df))

## generate the combinations with a bunch of grepling 

vrbl_cnbs <- list(
    all=all_var_names,
    no_cult_spending = all_var_names[!grepl("smorc_dollar_fx", all_var_names)],
    no_cult_spending_and_mitr = all_var_names[!grepl("smorc_dollar_fx|tmitr_approx_linear_2020step|ti_tmitr_interact", all_var_names)],
    controls = c(base_vars, all_var_names[grepl("NY.GDP.PCAP.CD|SP.POP.TOTL", all_var_names)]))



cbn <- vrbl_cnbs[["no_cult_spending_and_mitr"]]

## right hand side of formula

f_rhs <- paste0(cbn[cbn %!in% c("iso3c", "year")], collapse = " + ") %>%
    paste0(., " + (1|iso3c)")
                                                                           
f <- paste0("nbr_opened ~ ", f_rhs)

screenreg(glmer.nb(f, data = df_reg_lags))
