## * reg_anls_playground

## ** sqlite test 

## needed packages for this script
## install.packages("sqldf")  # install this package if necessary
library(sqldf)




## connection to the database TestDB.sqlite
db=dbConnect(SQLite(), dbname="TestDB.sqlite")

dbExecute(conn = db, "PRAGMA foreign_keys=ON")

## create the first table of the database
dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS artists
        (ID INTEGER,
        name TEXT,
        PRIMARY KEY (ID))")

## create the second table
dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS tracks
        (track_ID INTEGER,
        title TEXT,
        artist INTEGER,
        FOREIGN KEY(artist) REFERENCES artists(ID),
        PRIMARY KEY (track_ID))")

## filling the artist table with two rows
dbSendQuery(conn = db,
            paste0("INSERT INTO artists
        VALUES (1,'Tom Chapin')"))
dbSendQuery(conn = db,
            paste0("INSERT INTO artists
        VALUES (2,'Harry Chapin')"))

## filling the tracks table
dbSendQuery(conn = db,
            paste0("INSERT INTO tracks
        VALUES (1,'Cats in the Cradle',1)"))
## with the following tracks filling order there must occur an error
### but how to switch on the 'FOREIGN KEY'
dbSendQuery(conn = db,
            paste0("INSERT INTO tracks
        VALUES (2,'Cats in the Cradle',2)"))

## list the tables of the database
print(dbListTables(db))

## list the columns of a specific table
print(dbListFields(db,"artists"))  # of artists
print(dbListFields(db,"tracks"))   # of tracks

## show the data ...
print(dbReadTable(db,"artists"))  # of artists
print(dbReadTable(db,"tracks"))   # of tracks

dbDisconnect(db)





## ** sqlite reg_res_objs

library(RSQLite)

db_regres <- dbConnect(RSQLite::SQLite(), "/home/johannes/reg_res.sqlite")
dbExecute(conn = db_regres, "PRAGMA foreign_keys=ON")

## gof_df_cbn

prep_sqlitedb(dbx=db_regres, dfx=df_reg_anls_cfgs_wide, table_title = "df_reg_anls_cfgs_wide",
              constraints = c("PRIMARY KEY (mdl_id)"), insert_data = T)

## gof_df_cbn
prep_sqlitedb(dbx=db_regres, dfx=gof_df_cbn, table_title = "gof_df_cbn", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, gof_names)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

## df_anls base
prep_sqlitedb(dbx = db_regres, dfx = df_anls_base, table_title = "df_anls_base", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

dbRemoveTable(db_regres, "df_anls_within")

prep_sqlitedb(dbx = db_regres, dfx = df_anls_within, table_title = "df_anls_within", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              ## could add link to df_reg_anls_cfgs_wide, but don't think I need to?
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)"))
## "FOREIGN KEY (mdl_id) REFERENCES gof_df_cbn (mdl_id)"))


prep_sqlitedb(dbx = db_regres, dfx = df_anls_all, table_title = "df_anls_all", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)"))
                              ## "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))


prep_sqlitedb(dbx = db_regres, dfx = df_best_mdls, table_title = "df_best_mdls", insert_data = T,
              constraints = c("PRIMARY KEY (mdl_id, vrbl_name)",
                              "UNIQUE(cbn_name, vrbl_name_unlag)",
                              "FOREIGN KEY (mdl_id, vrbl_name) REFERENCES df_anls_base (mdl_id, vrbl_name)",
                              "FOREIGN KEY (mdl_id) REFERENCES df_reg_anls_cfgs_wide (mdl_id)"))

adt(df_best_mdls)[, .N, .(vrbl_name_unlag, cbn_name)][, .N, N]
adt(mdl_summary)[, .N, .(vrbl_name_unlag, cbn_name)][, .N, N]



prep_sqlitedb(dbx = db_regres, dfx = mdl_summary, table_title = "mdl_summary", insert_data = T,
              constraints =c(
                  "PRIMARY KEY (cbn_name, vrbl_name_unlag)",
                  "FOREIGN KEY (cbn_name, vrbl_name_unlag) REFERENCES df_best_mdls (cbn_name, vrbl_name_unlag)"
                  ))


## ** one-out visualization that failed at life
## *** boxplot with dodged cbns
ou_anls %>%
        ggplot(aes(x=log_likelihood_diff, y = ou_set_title_unlag, shape = cbn_name, color = cbn_name)) +
        ## geom_point() +
        geom_boxplot() 
        ## facet_grid(ou_set_title_unlag ~ ., scales = "free", space = "free")

## *** pointplot 

ou_anls %>% copy() %>%
        .[, .(mean_gof_diff = mean(log_likelihood_diff)), by = .(ou_set_title_unlag, cbn_name)] %>%
        ggplot(aes(x=mean_gof_diff, y = ou_set_title_unlag, shape = cbn_name, color = cbn_name)) +
        geom_point(size =3)


## ** retire kernel plots, gen_top_coefs2 (lame manual variable-hypothesis coding)

gen_top_coefs2 <- function(top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' move coefficient grouping by hypothesis into own function to allow access in gen_plt_coef_krnls_dev
    
    top_coefs2 <- rbind(
        top_coefs[vrbl_name_unlag == "NPO.tax.exemption",
                  .(hyp_id = "h1a", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[cbn_name != "cbn_no_cult_spending_and_mitr" & vrbl_name_unlag == "ti_tmitr_interact", 
                  .(hyp_id = "h1b", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[cbn_name == "cbn_no_cult_spending_and_mitr" & vrbl_name_unlag == "Ind.tax.incentives", 
                  .(hyp_id = "h1b", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[vrbl_name_unlag == "smorc_dollar_fxm",
                  .(hyp_id = "h2", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[vrbl_name_unlag %in% c("gptinc992j", "sptinc992j_p90p100", "sptinc992j_p99p100"),
                  .(hyp_id = "h3a", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[vrbl_name_unlag %in% c("ghweal992j", "shweal992j_p90p100", "shweal992j_p99p100"),
                  .(hyp_id = "h3b", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[vrbl_name_unlag %in% sprintf("hnwi_nbr_%sM", c(1,5,30,200)),
                  .(hyp_id = "h4", vrbl_name_unlag, cbn_name, coef)],
        top_coefs[vrbl_name_unlag %in% c("cnt_contemp_1990", "cnt_contemp_1990_squared", "NY.GDP.PCAP.CDk",
                                         "clctr_cnt_cpaer", "pm_density", "pm_density_sqrd",
                                         "pm_density_global", "pm_density_global_sqrd",
                                         "nbr_closed_cum_global"),
                  .(hyp_id = "zcontrols", vrbl_name_unlag, cbn_name, coef)]
    )

    return(top_coefs2)
}


gen_plt_coef_krnls_dev <- function(top_coefs) {
    #' experimental version of having a separate density for each variable
    #' doesn't look good so far: densities vary in spread, labelling of line difficult

    # could also add color

    top_coefs2 <- gen_top_coefs2(top_coefs)


    ## calculate densities (to get variable labels in plot)
    top_coefs_dens <- top_coefs2 %>% copy() %>%
        .[hyp_id != "h1a"] %>% 
        .[, .(x = density(coef)$x,
              y = density(coef)$y), by = .(vrbl_name_unlag, cbn_name, hyp_id)]

    ## actually get labels
    top_coefs_lbls <- top_coefs_dens %>% copy() %>%
        .[hyp_id %in% c("h3a", "h3b", "h4")] %>%
        .[, max_coef := max(y), by = .(vrbl_name_unlag, cbn_name, hyp_id)] %>%
        .[y == max_coef] %>%
        .[, .(hyp_id, cbn_name, x, y, label = vrbl_name_unlag, force = 1)]

    ## , label := vrbl_name_unlag] %>%
    ##         .[is.na(label), label := ""]


    ## investigate labels
    top_coefs_lbls[!is.na(label)]
    top_coefs_lbls[!is.na(label), .N, by = .(hyp_id, vrbl_name_unlag, cbn_name)][, max(N)]

    ## .[, .SD[which.max(y)], by = .(vrbl_name_unlag, cbn_name, hyp_id)]

    lbl_pts <- top_coefs_dens %>% copy() %>%
        .[hyp_id %in% c("h3a", "h3b", "h4")] %>%
        .[, gen_lbl_raster(.SD, method = "raster"), by = .(hyp_id, cbn_name)] %>%
        .[, `:=`(label = "", force = 0)] %>%
        rbind(top_coefs_lbls)

    ## testing
    ## top_coefs_dens[hyp_id == "h4" & cbn_name == "cbn_no_cult_spending_and_mitr"] %>% 
    ##     gen_lbl_raster(method = "random")

    
    ggplot() + 
        geom_point(lbl_pts, mapping = aes(x=x, y=y), alpha = 0.1) +
        geom_text_repel(lbl_pts, mapping = aes(x=x, y=y, label = label), 
                        min.segment.length = 0, # test
                        point.size = 3,
                        verbose = T,
                        max.iter = 2e6,
                        max.time = 5
                        ## point.padding = 0,
                        ## box.padding = 0,
                        ## force = 5
                        ) +
        geom_line(top_coefs_dens, mapping = aes(x=x, y=y, group = vrbl_name_unlag)) +
        facet_grid(hyp_id ~ cbn_name, scales = "free", switch = "y", space = "free", 
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x="coefficient")

    
    top_coefs2 %>%
        .[hyp_id != "h1a"] %>% 
        ggplot(aes(x=coef, y=..density.., fill = cbn_name, group = vrbl_name_unlag, alpha = vrbl_name_unlag)) +
        geom_density() +
        ## geom_line(stat = "density") + 
        facet_grid(hyp_id ~ cbn_name, scales = "free_y", switch = "y", space = "free_y", 
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x="coefficient")
    
    ## preliminary labelling: label for every point on density line
    ggplot() +
         geom_line(top_coefs_dens, mapping = aes(x=x, y=y, group = vrbl_name_unlag)) +
         ## geom_point(top_coefs_lbls, mapping = aes(x=x, y=y)) + 
         geom_text_repel(top_coefs_lbls, mapping = aes(x=x, y=y, label = label)) +
         facet_grid(hyp_id ~ cbn_name, scales = "free", switch = "y", space = "free",
                    labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
         geom_vline(xintercept = 0, linetype = "dashed") +
         theme(strip.text.y.left = element_text(angle = 0),
               legend.position = "bottom") +
         labs(x="coefficient")

    
    plt_coef_krnls <- top_coefs2 %>%
        ggplot(aes(x=coef, y=..density.., fill = cbn_name)) +
        geom_density() +
        facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free_y", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x="coefficient")
    plt_coef_krnls

}




gen_plt_coef_krnls <- function(top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' kernel distribution of coefficients of main variables

    top_coefs2 <- gen_top_coefs2(top_coefs)[hyp_id != "zcontrols"]

    
    ## working plot, grouped by hypothesis
    plt_coef_krnls <- top_coefs2 %>%
        ggplot(aes(x=coef, y=..density.., fill = cbn_name)) +
        geom_density() +
        facet_grid(hyp_id ~ cbn_name, scales = "free_y", switch = "y",
                   labeller = as_labeller(c(vvs$krnl_lbls, vvs$cbn_lbls, vvs$vrbl_lbls))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme(strip.text.y.left = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x="coefficient")

    
    

    


    return(plt_coef_krnls)

}

eval_h1a <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef>hyp_thld] %>%
        .[, hyp_id := "h1a"] %>%
        .[, coef := NULL]
}

eval_h1b <- function(rows, hyp_thld) {
    #' hypothesis test: in cbn_no_cult_spending_and_mitr: whether Ind.tax.incentives > hyp_thld
    #' other combinations: whether interaction between tax deductibility of donations and TMITR > hyp_thld
        
    rows[cbn_name == "cbn_no_cult_spending_and_mitr" , hyp_eval := coef > hyp_thld] %>% 
        .[cbn_name != "cbn_no_cult_spending_and_mitr" & vrbl_name_unlag == "ti_tmitr_interact",
          hyp_eval := coef > hyp_thld] %>%
        .[!is.na(hyp_eval)] %>% # deleting tax deductibility of donations when interaction is there
        .[, `:=`(coef = NULL, vrbl_name_unlag = NULL)] %>%
        .[, hyp_id := "h1b"]
}


eval_h2a <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef < -hyp_thld] %>%
        .[, hyp_id := "h2a"] %>%
        .[, coef := NULL]
}

eval_h2b <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef > hyp_thld] %>%
        .[, hyp_id := "h2b"] %>%
        .[, coef := NULL]
}


eval_h3a <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef > hyp_thld] %>%
        .[, hyp_id := "h3a"] %>%
        .[, coef := NULL]
}

eval_h3b <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef > hyp_thld] %>%
        .[, hyp_id := "h3b"] %>%
        .[, coef := NULL]
}

eval_h4 <- function(rows, hyp_thld) {
    rows[, hyp_eval := coef > hyp_thld] %>%
        .[, hyp_id := "h4"] %>%
        .[, coef := NULL]
}



eval_all_hyps <- function(top_coefs, hyp_thld) {

    hyp_res <- rbind(
        top_coefs[vrbl_name_unlag == "NPO.tax.exemption", .(coef, cbn_name)] %>% eval_h1a(hyp_thld),
        top_coefs[vrbl_name_unlag %in% c("Ind.tax.incentives", "ti_tmitr_interact"),
                  .(cbn_name, vrbl_name_unlag, coef)] %>% eval_h1b(hyp_thld),
        top_coefs[vrbl_name_unlag == "smorc_dollar_fxm", .(coef, cbn_name)] %>% eval_h2a(hyp_thld),
        top_coefs[vrbl_name_unlag == "smorc_dollar_fxm", .(coef, cbn_name)] %>% eval_h2b(hyp_thld),
        top_coefs[vrbl_name_unlag %in% c("gptinc992j", "sptinc992j_p90p100", "sptinc992j_p99p100"),
                  .(coef, cbn_name)] %>% eval_h3a(hyp_thld),
        top_coefs[vrbl_name_unlag %in% c("ghweal992j", "shweal992j_p90p100", "shweal992j_p99p100"),
                  .(coef, cbn_name)] %>% eval_h3b(hyp_thld),
        top_coefs[vrbl_name_unlag %in% sprintf("hnwi_nbr_%sM", c(1,5,30,200)),
                  .(coef, cbn_name)] %>% eval_h4(hyp_thld))

    ## evaluate the hyps per hyp_thld
    hyp_res[, hyp_thld := hyp_thld]

    hyp_res[, .(mean_hyp_eval = mean(hyp_eval)), by = .(hyp_id, cbn_name, hyp_thld)]
        
}


gen_plt_hyp_thld_res <- function(top_coefs) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}


    ## eval_all_hyps(top_coefs, 0.2)

    hyp_thld_res <- future_map_dfr(seq(-1,1, by = 0.05), ~eval_all_hyps(top_coefs, hyp_thld = .x))

    plt_hyp_thld_res <- hyp_thld_res %>% 
        ggplot(aes(x=hyp_thld, y=mean_hyp_eval, color = cbn_name)) +
        geom_line() + 
        facet_wrap(~hyp_id, ncol = 2, labeller = labeller(hyp_id = vvs$hyp_lbls)) +
        labs(x="Threshold (variable SD)", y = "proportion of coefficients above threshold") +
        theme(legend.position = c(0.7, 0.1)) +
        geom_vline(xintercept = 0, linetype = "dashed")

    return(plt_hyp_thld_res)
}


genxtbl_regres <- function(wcptbl, groups, vrbl_lbls, mdl_lbls) {
    
    
    dt_coefs_fmtd <- top_coefs_prepd %>% copy() %>%
        .[, .(cell_fmtd = fmt_cell(coef, se, pvalue, wcptbl)), by = .(vrbl, cbn_name, hyp)] %>%
        dcast.data.table(vrbl ~ cbn_name, value.var = "cell_fmtd") %>%
        .[dt_texreg_order[, .(vrbl)], on = "vrbl"] %>% # order coefs
        data.table(lbl = latexTranslate(addline_format(unname(vvs$vrbl_lbls))),
                   vrbl = names(vvs$vrbl_lbls))[., on = "vrbl"] %>%
        .[, vrbl := NULL] %>% ## yeet variable column
        cbind(grp = "", .) ## add group column for indented variable labels
        
    ## dt_coefs_fmtd[grepl("Marginal Income", lbl)]

    ## generate group add.to.row stuff
    xtbl_grps <- dt_texreg_order[, .(pos = min(nbr)-1), hyp] %>%
        .[data.table(hyp = names(vvs$krnl_lbls),
                     lbl = gsub("\n", " ", unname(vvs$krnl_lbls))), on = "hyp"] %>%
        na.omit() %>%
        .[, lbl2 := sprintf("\\multicolumn{%s}{l}{\\textbf{%s}} \\\\ \n", ncol(dt_coefs_fmtd), lbl)]
        
        
    
    ## generate the vector of model names
    v_mdl_names <- map_chr(names(dt_coefs_fmtd)[3:ncol(dt_coefs_fmtd)], # 3 for when using groups
                           ## loop over cbn_lbls, get their label
                           ~sprintf("\\multicolumn{1}{c}{%s}", pluck(vvs$cbn_lbls, .x)))
    ## collapse v_mdl_names (and hline + newline + space before for empty cell) to single string
    chr_mdl_names <- paste0(c("\\hline \n ", " ", v_mdl_names), collapse = " & ") %>%
        paste0(" \\\\ \n") # add linebreak at the end (needs separate command to not have & added), and newline
    
    ## clm_names <- list()
    ## clm_names$pos <- list(-1)
    ## clm_names$command <- chr_mdl_names
    
    l_add_to_row <- list()
    l_add_to_row$pos <- c(list(-1), as.list(xtbl_grps$pos))
    l_add_to_row$command <- c(chr_mdl_names, xtbl_grps$lbl2)
    
    xtable(dt_coefs_fmtd,
           align = c("l ","p{1mm}", "p{6cm} ", "D{)}{)}{8)3} ", "D{)}{)}{8)3} ", "D{)}{)}{8)3}")
           ## align = c("l ","p{1mm}", "p{6cm} ", rep("L ", 3))
           ) %>%
        pvlt(santitize.text.function = identity,
             add.to.row = l_add_to_row,
             include.colnames = F,
             hline.after = 0, crop = T)

    
    xtable(dt_coefs_fmtd,
           ## align = c("l ", "l ", "D{)}{)}{8)3} ", "D{)}{)}{8)3} ", "D{)}{)}{8)3}") %>%
           align = c("l ","p{1mm}", "p{6cm} ", rep("L ", 3))
           ) %>% 
        ## pvlt(santz.txt.f = identity)
        print.xtable(include.rownames = F,
                     include.colnames = F,
                     file = paste0(TABLE_DIR, "xtbl_regrslts.tex"),
                     sanitize.text.function = identity,
                     add.to.row = l_add_to_row,
                     hline.after = c(0))
                     
    
    
    
    

        
    

    
    ## huxtablereg(custom.co list_coefs$cbn_all)

    ## library(huxtable)

    ## carat_coef_map <- list(depth = "Depth", carat = "Carat but also a bunch\n of other text for linebreak")

    ## lm3 <- lm(log(price) ~ carat + depth, diamonds)

    ## huxtablereg(lm3, custom.coef.map = carat_coef_map) %>% 
    ##     print_latex(tabular_only = F) %>%
    ##     capture.output(file = paste0(TABLE_DIR, "huxtable_test.tex"))
    
    ## carat_vec <- setNames(names(carat_coef_map), unname(carat_coef_map))

    ## huxreg(lm3, coefs = carat_vec, error_pos = "same") %>% print_latex() %>%
    ##     capture.output(file = paste0(TABLE_DIR, "huxtable_test.tex"))
    
    ## huxtable::set_row_height

    ## tidy(lm3)

    ## class(lm3)
    ## tidy(list_coefs$cbn_all)
    

    ## install.packages("gt")
    ## install.packages("juicyjuice")
    ## library(gt)
    ## install.packages("gtsummary")
    ## library(gtsummary)

    



}
gentbl_regtbl_old <- function(top_coefs, gof_df_cbn, df_best_mdls) {
    #' generate the regression result table
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    ## split coefs into list to feed them to createTexreg
    top_coefs_prepd <- top_coefs %>% copy() %>%
        .[, .SD[which.max(log_likelihood)], by = .(vrbl_name_unlag, cbn_name)] %>%
        copy(vvs$hyp_mep_dt)[., on = .(vrbl = vrbl_name_unlag)] %>% # original
        .[vrbl %!in% c("ln_s", "ln_r")] %>% 
        .[, .(vrbl, hyp, cbn_name, coef, se, pvalue = pvalues)]

    list_coefs <- top_coefs_prepd %>% split(.$cbn_name)
        
    ## df_best_mdls[, .SD[which.max(log_likelihood)

    ## set up gof label dts
    dt_gof_lbls <- list(
        list(gof_name = "N"              , gof_lbl = "N"              , decimal = 0),
        list(gof_name = "N_g"            , gof_lbl = "No. countries"  , decimal = 0),
        list(gof_name = "log_likelihood" , gof_lbl = "Log likelihood" , decimal = 2)) %>%
        rbindlist() %>% 
        .[, gof_lbl := factor(gof_lbl, levels = gof_lbl)] 
        ## .[order(gof_lbl)] %>% 
        ## .[, gof_name := factor(gof_name, levels = gof_name)]
             
    ## split GOFS into list to feed to createTexreg
    dt_gofs_prepd <- adt(gof_df_cbn)[gof_names == "log_likelihood"] %>%
        .[, .SD[which.max(gof_value), .(mdl_id)], cbn_name] %>%
        .[adt(gof_df_cbn), on = "mdl_id", nomatch = NULL] %>%
        .[gof_names %in% c("N", "N_g", "log_likelihood"), .(cbn_name, gof_names, gof_value)] %>% 
        dt_gof_lbls[., on = .(gof_name = gof_names)] %>%
        .[order(gof_lbl)] %>% 
        .[, .(cbn_name, gof_names = gof_lbl, gof_value, decimal)]

    list_gofs <- dt_gofs_prepd %>% split(as.character(.$cbn_name))
    ## list_gofs
        

    list_texreg <- map2(list_coefs, list_gofs,
         ~createTexreg(coef.names = as.character(.x$vrbl), coef = .x$coef, se = .x$se, pvalues = .x$pvalue,
                       gof.names = as.character(.y$gof_names), gof = .y$gof_value, gof.decimal = .y$decimal))
    names(list_texreg) <- vvs$cbn_lbls

    ## screenreg(list_texreg, single.row = T)

    ## screenreg(list_texreg[[1]], custom.coef.map = as.list(vvs$vrbl_lbls), single.row = T)
    
    ## get variable labels and hypothesis membership for ordering
    dt_texreg_order <- data.table(vrbl = names(vvs$vrbl_lbls), lbl = unname(vvs$vrbl_lbls)) %>%
        ## use only those vvs entries that are in any list_coef
        .[data.table(vrbl = intersect(names(vvs$vrbl_lbls), rbindlist(list_coefs)[, unique(achr(vrbl))])),
          on="vrbl"] %>% 
        vvs$hyp_mep_dt[., on= "vrbl"] %>% 
        .[order(hyp)] %>% copy() %>%
        .[, nbr := 1:.N]
    
            
    ## create texreg grouping of variables by hypothesis
    texreg_groups <- dt_texreg_order[, .(list_id = list(nbr)), hyp] %>%
        data.table(hyp_id = names(vvs$krnl_lbls),
                   hyp_lbl = gsub("\n", " ", unname(vvs$krnl_lbls)))[., on = .(hyp_id = hyp)] %$%
        setNames(list_id, hyp_lbl)

    texreg_coefmap <- dt_texreg_order %$% setNames(trimws(addline_format(lbl)), vrbl) %>% as.list()

    texreg(list_texreg,
           custom.coef.map = texreg_coefmap, # reorder variables into hypotheses
           groups = texreg_groups[2:len(texreg_groups)], # don't put intercept into separate group
           single.row = T,
           leading.zero = F,
           dcolumn = F, 
           use.packages = F,
           ## threeparttable = T,
           caption = "Negative binomial models of private museum founding rate",
           label = "tbl_regres",
           ## custom.columns = list(1:26),
           ## custom.col.pos = 1,
           file = paste0(TABLE_DIR, "tbl_regres.tex"))

    trl_regrslts <- list(l = list_texreg, # list of models
                         custom.coef.map = texreg_coefmap, # order/labels of coefs
                         groups = texreg_groups[2:len(texreg_groups)]) # groups

    c.screenreg(trl_regrslts)
    
    ## library(kableExtra)
    ## kbl(list_coefs[[1]], "latex") %>% kable_styling(latex_options = c("hold_position"))

    ## kableExtra::kable_styling


}

## ** rprofiling
    ## data.table(asdf=1, yyy= 2)
    ## lm(mpg ~ cyl + hp + disp, mtcars)
    ## lm(cyl ~ hp + mpg, mtcars)
    
    Rprof(memory.profiling = T, append = F, gc.profiling = T, line.profiling = T, filter.callframes = T)
    format("asdf")
    ## lm(asdf ~ jj)
    ## paste0("jj", "kk")
    Rprof(memory.profiling = T, append = T, gc.profiling = T, line.profiling = T, filter.callframes = T)

    fmt <- format

    
    ## ggplot(iris, aes(x=Sepal.Width, y = Petal.Width, color = Species)) + geom_point()

    ## lm(mpg ~ cyl + hp + disp + drat + hp + krappa + matrix + gigachad + caitlin + little dragon)
    ## plus some other stuff

    ## lm(mpg ~ cyl + hp + disp + drat + hp + krappa + giga + fucking nooobs)
    ## lm(mpg ~ cyl + hp + disp + drat + hp + krappa + giga + wtf + is + wrong + with + this + program)
    ## ok(this, looks, already, somewhat, better, but , not-by , much)

    ## what(happens, if, i disable, font, lock, mode, completely?)
    ## even(then, the, R, process 



## ** some regression test

library(glmmTMB)

r1 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
        cbn_dfs_rates$cbn_all, family = nbinom2)

r2 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) +
            offset(log(SP_POP_TOTLm_lag0_uscld)),
        cbn_dfs_rates$cbn_all, family = nbinom2)

r3 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r4 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 + 
                  sptinc992j_p99p100_lag0 + offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r5 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 + offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r6 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  pm_density_lag0 + pm_density_sqrd_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r7 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  pm_density_lag0 + pm_density_sqrd_lag0 +
                  pm_density_global_sqrd_lag0 + pm_density_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r8 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  pm_density_lag0 + pm_density_sqrd_lag0 +
                  cnt_contemp_1990 + cnt_contemp_1990_sqrd + clctr_cnt_cpaer_lag0 + 
                  pm_density_global_sqrd_lag0 + pm_density_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r9 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  pm_density_lag0 + pm_density_sqrd_lag0 +
                  cnt_contemp_1990 + cnt_contemp_1990_sqrd + clctr_cnt_cpaer_lag0 + 
                  pm_density_global_sqrd_lag0 + pm_density_global_lag0 +
                  nbr_closed_cum_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)

r10 <- glmmTMB(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0  + (1 | iso3c) + hnwi_nbr_30M_lag0 +
                  Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  smorc_dollar_fxm_lag0 + smorc_dollar_fxm_sqrd_lag0 + 
                  pm_density_lag0 + pm_density_sqrd_lag0 +
                  cnt_contemp_1990 + cnt_contemp_1990_sqrd + clctr_cnt_cpaer_lag0 + 
                  pm_density_global_sqrd_lag0 + pm_density_global_lag0 +
                  nbr_closed_cum_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
            cbn_dfs_rates$cbn_all, family = nbinom2)


screenreg(list(r1,r2,r3, r4, r5, r6, r7, r8, r9, r10))

rp1 <- pglm(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0 + offset(log(SP_POP_TOTLm_lag0_uscld)),
     cbn_dfs_rates$cbn_all,
     family = poisson, model = "within", effect = "individual", index = "iso3c")

rp2 <- pglm(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0 +hnwi_nbr_30M_lag0 +
                  # Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  smorc_dollar_fxm_lag0 + smorc_dollar_fxm_sqrd_lag0 + 
                  pm_density_lag0 + pm_density_sqrd_lag0 +
                   clctr_cnt_cpaer_lag0 + 
                  pm_density_global_sqrd_lag0 + pm_density_global_lag0 +
                  nbr_closed_cum_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
     cbn_dfs_rates$cbn_all,
     family = negbin, model = "within", effect = "individual", index = "iso3c")

rp3 <- pglm(nbr_opened ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0 +hnwi_nbr_30M_lag0 +
                  # Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  smorc_dollar_fxm_lag0 + smorc_dollar_fxm_sqrd_lag0 + 
                  ## pm_density_lag0 + pm_density_sqrd_lag0 +
                  ## pm_density_global_sqrd_lag0 + pm_density_global_lag0 +
                   clctr_cnt_cpaer_lag0 + 
                   nbr_closed_cum_global_lag0 + 
                  offset(log(SP_POP_TOTLm_lag0_uscld)),
     cbn_dfs_rates$cbn_all,
     family = negbin, model = "within", effect = "individual", index = "iso3c")


## dfx_krappa <- adt(cbn_dfs_rates_uscld$cbn_all) %>%
##     ## .[, lapply(.SD, \(x) scale(x))]
##     melt(id.vars = c("iso3c", "year")) %>%
##     .[, vlu_scaled := scale(value), variable] %>%
##     dcast.data.table(iso3c + year ~ variable, value.var = "vlu_scaled") %>%
##     .[, dens_uscld := pm_density_lag0*SP.POP.TOTLm_lag0]

## some attempt to use FE with density rather than nbr_opened, doesn't work at all atm 
rp4 <- pglm(dens_uscld ~ shweal992j_p90p100_lag0 + NY.GDP.PCAP.CDk_lag0 + hnwi_nbr_30M_lag0 +
                  # Ind.tax.incentives + tmitr_approx_linear20step_lag0 + ti_tmitr_interact_lag0 + 
                  sptinc992j_p99p100_lag0 +
                  smorc_dollar_fxm_lag0 + smorc_dollar_fxm_sqrd_lag0 + 
                  ## pm_density_lag0 + pm_density_sqrd_lag0 +
                  ## pm_density_global_sqrd_lag0 + pm_density_global_lag0 +
                  clctr_cnt_cpaer_lag0 + nbr_closed_cum_global_lag0 + offset(log(SP.POP.TOTLm_lag0)),
            dfx_krappa,
            ## mtcars,
            family = negbin, model = "within", effect = "individual", index = "iso3c")


screenreg(list(rp1, rp2, rp3, rp4))


## ** variance debug

dt_vrnc <- data.table(id = sort(rep(c("a", "b", "c", "d"),3)),
                      dens = c(31,32,33, 10,10,10, 20,21,22, 5,4,3)) %>%
    .[, dens_lag := shift(dens), id] %>%
    .[is.na(dens_lag), dens_lag := dens-1] %>%
    .[, nbr_opnd :=  dens - dens_lag]


dt_vrnc[, .(mean_dens = mean(dens)), id][, sd(mean_dens)]


## overall
xtsum(dt_vrnc, dens, id) %>% adt() %>% .[Comparison %in% c("Overall", "Within"), .(Comparison, sd)] %>%
    dcast.data.table(.~Comparison, value.var = "sd") %>%
    .[, .(dens = Within/Overall)]
## within
xtsum(dt_vrnc, nbr_opnd, id) %>% adt() %>% .[Comparison %in% c("Overall", "Within"), .(Comparison, sd)] %>%
    dcast.data.table(.~Comparison, value.var = "sd") %>%
    .[, .(nbr_opnd = Within/Overall)]

xtsum(dt_vrnc, nbr_opnd, id) %>% adt() %>% .[Comparison != "Between", .(Comparison, sd)]



## ** figure out VIF setup

## *** test different functions: see whether offset, random intercept, both neither differ -> yup
library(car, include.only = "vif")

## cbn_dfs_rates$cbn_all
f_base <- nbr_opened ~ hnwi_nbr_1M_lag0 + gptinc992j_lag4 + NY.GDP.PCAP.CDk_lag1
## random intercept
f_ri <- update.formula(f_base, ~ . + (1 | iso3c))
## offset
f_ofst <- update.formula(f_base, ~ . + offset(SP.POP.TOTLm_lag0))
## both offset and random intercept
f_ri_ofst <- update.formula(f_base, ~ . + (1 | iso3c) + offset(SP.POP.TOTLm_lag0))

library(glmmTMB)
library(MASS)
res_base <- glm.nb(f_base, cbn_dfs_rates$cbn_all)
res_base2 <- glmmTMB(f_ri_ofst, cbn_dfs_rates$cbn_all, family = nbinom2)

## compare glmmtmb and glmer.nb
## use glmmtmb for more flexible handling

glmer.nb(f_ri_ofst, cbn_dfs_rates$cbn_all) %>% vif()

glmmTMB(f_ofst, cbn_dfs_rates$cbn_all, family = nbinom2) %>% vif()

check_collinearity(res_base2) %>% names()


glm(f_base, data = cbn_dfs_rates$cbn_all)


fncs <- list(f_base     = f_base,   
             f_ri       = f_ri,     
             f_ofst     = f_ofst,   
             f_ri_ofst  =f_ri_ofst)

## *** check whether link function matters, seems to 
df_vifres <- imap_dfr(fncs, ~glmmTMB(.x, cbn_dfs_rates$cbn_all, family = nbinom2) %>%
                                check_collinearity() %>% adt() %>% .[, f := .y])

df_vifres_ols <- imap_dfr(fncs, ~glmmTMB(.x, cbn_dfs_rates$cbn_all) %>%
                                    check_collinearity() %>% adt() %>% .[, f := .y])

ggplot(df_vifres, aes(x=VIF, y=Term, fill = f)) +
    geom_col(position = position_dodge2())

## vif(res_base)

rbind(copy(df_vifres)[, family := "nbinom2"], copy(df_vifres_ols)[, family := "ols"]) %>%
    ggplot(aes(x=VIF, y=f, group = family, fill = family)) + 
    geom_col(position = position_dodge2()) +
    facet_grid(~Term)

## *** compare xtnbreg and glmmtmb again to see if they're similar enough -> they are


## model_performance(res_base2)
## check_model(res_base2)

## some_mdl <- get_reg_spec_from_id(reg_res_objs$top_coefs$mdl_id[1], fldr_info)
## res_xtnbreg <- run_vrbl_mdl_vars(some_mdl, vvs, fldr_info, verbose = T, wtf = F, return_objs = c("res_parsed"))


## mdl_glmmTMB <- some_mdl %>% copy()
## pluck(mdl_glmmTMB, "cfg", "regcmd") <- "glmmTMB"

## res_glmmtmb <- run_vrbl_mdl_vars(mdl_glmmTMB, vvs, fldr_info, wtf = F, return_objs = "res_parsed")

## dt_vif <- vif_tester(mdl_glmmTMB)

## ggplot(dt_vif, aes(x = VIF, y=Term, fill = vrblset)) +
##     geom_col(position = position_dodge())

## jj
## ## xtnbreg and glmmtmb are pretty similar
## res_glmmtmb$res_parsed$gof_df %>% adt()

## rbind(adt(res_xtnbreg$res_parsed$coef_df)[, src := "xtnbreg"],
##       adt(res_glmmtmb$res_parsed$coef_df)[, src := "glmmtmb"]) %>%
##     ggplot(aes(x=coef, y = vrbl_name, fill = src)) +
##     geom_col(position = position_dodge2())

## probably similar enough to use glmmTMB for VIF 

## glmmTMB::fixef


## ** splines test
gen_knts <- function(v, sec_prop) {
    #' construct knots: use quantiles
    qnt_end <- 1 - sec_prop

    quantile(v, probs = seq(sec_prop, qnt_end, sec_prop)) %>%
        unique() # only use unique values after all
}

test_splines <- function(cbn_dfs_rates) {

    ## testing spline for nbr_opened ~ density 
    r_dens_sqrd <- glmmTMB(nbr_opened ~ pm_density_lag1 + pm_density_sqrd_lag1 +
                               offset(log(SP_POP_TOTLm_lag0_uscld)) + (1 | iso3c), cbn_dfs_rates$cbn_all)


    knts_spline <- gen_knts(cbn_dfs_rates$cbn_all$pm_density_lag1, 0.2)

    knts_spline <- quantile(cbn_dfs_rates$cbn_all$pm_density_lag1, probs = seq(0.2, 0.8, 0.2))

    
    r_dens_spline <- glmmTMB(nbr_opened ~ bs(pm_density_lag1, knots = knts_spline, degree = 3) +
                                 offset(log(SP_POP_TOTLm_lag0_uscld)) + (1 | iso3c), cbn_dfs_rates$cbn_all)

    compare_performance(r_dens_sqrd, r_dens_spline)
    ## not much difference, splines little better

    dt_pred_dens <- expand.grid(iso3c = "ZAF",
                                pm_density_lag1 = seq(min(cbn_dfs_rates$cbn_all$pm_density_lag1),
                                                      max(cbn_dfs_rates$cbn_all$pm_density_lag1), 0.1),
                                SP_POP_TOTLm_lag0_uscld = c(1,5,10,50)) %>% adt() %>%
        .[, pm_density_sqrd_lag1 := pm_density_lag1^2]
                                
    dt_pred_dens$pred_spline <- predict(r_dens_spline, dt_pred_dens)
    dt_pred_dens$pred_sqrd <- predict(r_dens_sqrd, dt_pred_dens)

    dt_pred_dens %>% copy() %>% .[, pm_density_sqrd_lag1 := NULL] %>% # yeet sqrd (not needed)
        melt(id.vars = c("iso3c", "pm_density_lag1", "SP_POP_TOTLm_lag0_uscld")) %>%
        ggplot(aes(x=pm_density_lag1, y = exp(value), group = interaction(variable, SP_POP_TOTLm_lag0_uscld),
                   color = variable, linetype = factor(SP_POP_TOTLm_lag0_uscld))) +
        geom_line()
                          
    
    
    ## testing spline for nbr_opened ~ density

    r_hnwi_line <- glmmTMB(nbr_opened ~ hnwi_nbr_30M_lag1 +
                               offset(log(SP_POP_TOTLm_lag0_uscld)) + (1 | iso3c),
                           filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <= 2))
    
    r_hnwi_sqrd <- glmmTMB(nbr_opened ~ hnwi_nbr_30M_lag1 + I(hnwi_nbr_30M_lag1^2) +
                               offset(log(SP_POP_TOTLm_lag0_uscld)) + (1 | iso3c),
                           ## cbn_dfs_rates$cbn_all)
                           filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <= 2))


    knts_spline <- gen_knts(cbn_dfs_rates$cbn_all$hnwi_nbr_30M_lag1, 0.2)
    knts_spline <- gen_knts(filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <=2)$hnwi_nbr_30M_lag1, 0.2)

   
    r_hnwi_spline <- glmmTMB(nbr_opened ~ bs(hnwi_nbr_30M_lag1, knots = knts_spline, degree = 3) +
                                 offset(log(SP_POP_TOTLm_lag0_uscld)) + (1 | iso3c),
                             filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <= 2))
                             ## cbn_dfs_rates$cbn_all)

    compare_performance(r_hnwi_sqrd, r_hnwi_spline, r_hnwi_line)
    ## not much difference, splines little better

    dt_pred_hnwi <- expand.grid(iso3c = "zulul",
                                hnwi_nbr_30M_lag1 = seq(
                                    min(filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <= 2)$hnwi_nbr_30M_lag1),
                                    max(filter(cbn_dfs_rates$cbn_all, hnwi_nbr_30M_lag1 <= 2)$hnwi_nbr_30M_lag1),
                                    0.1),
                                SP_POP_TOTLm_lag0_uscld = c(1,5,10,50)) %>% adt()
        ## .[, pm_density_sqrd_lag1 := pm_density_lag1^2]
    
    dt_pred_hnwi$pred_spline <- predict(r_hnwi_spline, dt_pred_hnwi)
    dt_pred_hnwi$pred_sqrd <- predict(r_hnwi_sqrd, dt_pred_hnwi)
    dt_pred_hnwi$pred_line <- predict(r_hnwi_line, dt_pred_hnwi)

    dt_pred_hnwi %>% copy() %>% .[, pm_density_sqrd_lag1 := NULL] %>% # yeet sqrd (not needed)
        melt(id.vars = c("iso3c", "hnwi_nbr_30M_lag1", "SP_POP_TOTLm_lag0_uscld")) %>%
        ggplot(aes(x=hnwi_nbr_30M_lag1, y = value, group = interaction(variable, SP_POP_TOTLm_lag0_uscld),
                   color = variable, linetype = factor(SP_POP_TOTLm_lag0_uscld))) +
        geom_line()

}

explr_within_between_decomposition <- function(cbn_dfs_rates) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    ## within/between for actual analysis
    dfx <- cbn_dfs_rates$cbn_all
    

    dtx_dcpsd <- dfx %>% copy() %>% adt() %>% 
        melt(id.vars = c("iso3c", "year")) %>%
        .[, between := mean(value), .(iso3c, variable)] %>%
        .[, within := value - between] %>%
        melt(id.vars = c("iso3c", "variable", "year"), variable.name = "form") %>%
        dcast.data.table(iso3c + year ~ variable + form)

    r_dcpsd1 <- glmmTMB(nbr_opened_value ~ NY.GDP.PCAP.CDk_lag1_within + NY.GDP.PCAP.CDk_lag1_between +
                            hnwi_nbr_30M_lag1_within + hnwi_nbr_30M_lag1_between + (1 | iso3c) +
                            offset(log(SP_POP_TOTLm_lag0_uscld_value)),
                        family = nbinom2, dtx_dcpsd)
    sumry(r_dcpsd1)

    r_dcpsd2 <- glmmTMB(nbr_opened_value ~ NY.GDP.PCAP.CDk_lag1_within + NY.GDP.PCAP.CDk_lag1_between +
                            hnwi_nbr_30M_lag1_within + hnwi_nbr_30M_lag1_between + (1 | iso3c) +
                            offset(log(SP_POP_TOTLm_lag0_uscld)),
                        family = nbinom2, dtx_dcpsd)
    
    dtx_dcpsd[, .SD, .SDcols = c("iso3c", "year", keep(names(dtx_dcpsd), ~grepl("SP_POP", .x)))] %>%
        summary()
    


    r_orig <- glmmTMB(nbr_opened_value ~ NY.GDP.PCAP.CDk_lag1_value + hnwi_nbr_30M_lag1_value + (1 | iso3c) +
                          offset(log(SP_POP_TOTLm_lag0_uscld_value)),
                      family = nbinom2, dtx_dcpsd)

    sumry(r_orig)

    r_fe <- glmmTMB(nbr_opened_value ~ NY.GDP.PCAP.CDk_lag1_within + hnwi_nbr_30M_lag1_within + (1 | iso3c) +
                          offset(log(SP_POP_TOTLm_lag0_uscld_value)),
                      family = nbinom2, dtx_dcpsd)

    compare_models(r_dcpsd1, r_fe, r_orig)
    compare_performance(r_dcpsd1, r_fe, r_orig)


    set.seed(123)
    n <- 5
    b <- seq(1, 1.5, length.out = 5)
    x <- seq(2, 2 * n, 2)

    d <- do.call(rbind, lapply(1:n, function(i) {
        data.frame(
            x = seq(1, n, by = 0.2),
            y = 2 * x[i] + b[i] * seq(1, n, by = 0.2) + rnorm(21),
            grp = as.factor(2 * i)
        )
    }))

    d <- d %>%
        group_by(grp) %>%
        mutate(x = rev(15 - (x + 1.5 * as.numeric(grp)))) %>%
        ungroup()

    labs <- c("very slow", "slow", "average", "fast", "very fast")
    levels(d$grp) <- rev(labs)

    d <- cbind(d, datawizard::demean(d, c("x", "y"), group = "grp")) %>% adt()

    ggplot(d, aes(x=x, y=y)) + geom_point()
    
    rt1 <- glmmTMB(y ~ x_between + x_within + (1 | grp), d)

    ## convert y to count, some pretty random population
    d2 <- d %>% copy() %>% .[, y := as.integer(y)] %>%
        .[, pop := sample(1e5:1e6,1), grp] %>%
        .[, intx := 1:.N] %>% 
        .[, pop := pop*runif(1, 0.1, 10), intx] %>% 
        ## .[, pop := as.integer(grp)] %>%
        .[, y_adj := as.integer(pop * y)] %>% # to integer
        .[, pop_mean := mean(pop), grp] %>%
        .[, `:=`(y_sqrd = y^2, x_sqrd = x^2)]

    ggplot(d2, aes(x=x, y =y_adj, color = grp)) + geom_point()

    ## goal is to get same coefs with original and population adjusted data
    ggplot(d2, aes(x=x, y =y)) + geom_point()

    ## basic NB model with orignal data
    r_nb1 <- glmmTMB(y ~ x_between + x_within + (1 | grp), d2, family = nbinom2)
    ## offseted
    r_nb2 <- glmmTMB(y_adj ~ x_between + x_within + (1 | grp) + offset(log(pop)), d2, family = nbinom2)
    ## offseted also with between pop? leads to changed coef
    r_nb3 <- glmmTMB(y_adj ~ x_between + x_within + (1 | grp) + offset(log(pop)) + offset(log(pop_mean)),
                     d2, family = nbinom2)
    ## only between offset
    r_nb4 <- glmmTMB(y_adj ~ x_between + x_within + (1 | grp) + offset(log(pop_mean)),
                   d2, family = nbinom2)
    
    compare_models(r_nb1, r_nb2, r_nb3, r_nb4)


    ## check HNWI skew: there are especially some outliers on 30M for some reason
    dfx %>% adt() %>% .[, .SD, .SDcols = keep(names(.), ~grepl("hnwi", .x))] %>%
        .[, map(.SD, max)] %>% melt() %>% 
        .[, thld := str_extract(variable, "[0-9]+")] %>% print(n=200) %>%
        ggplot(aes(x=thld, y=value, grp = thld)) + geom_col(position = position_dodge2())


    ## check squares/interactions
    ggplot(d2, aes(x=x, y=y_sqrd)) + geom_point()

    ## melt(id.vars = c("iso3c", "year")) %>%
    ##     .[, between := mean(value), .(iso3c, variable)] %>%
    ##     .[, within := value - between] %>%
    ##     melt(id.vars = c("iso3c", "variable", "year"), variable.name = "form") %>%
    ## dcast.data.table(iso3c + year ~ variable + form)


    d3 <- data.table(grp = seq(4,20, by = 2)) %>% # set starting points
        .[, `:=`(xmin = grp - 4, xmax = grp + 4)] %>% # set edge points
        .[, .(x = seq(xmin, xmax)), grp] %>% # expand x-range
        .[, y := -(x^2) + 2*grp*x + (10-grp)*20 + runif(.N, -2, 2)] %>% # awkward square calculation + noise
        .[, xx := x] %>% 
        melt(id.vars = c("grp", "xx")) %>% 
        .[, between := mean(value), .(grp, variable)] %>%
        .[, within := value - between] %>%
        melt(id.vars = c("grp", "variable", "xx"), variable.name = "form") %>% ## 
    ## d3 %>% copy() %>%
    ##     .[, grp := factor(letters[grp])] %>% # unique() %>% 
        dcast.data.table(grp + xx ~ variable + form)
    ggplot(d3, aes(x=x_value, y=y_value, color = factor(grp))) + geom_line()
    
    # linear model: within effect insignificant
    r_sq1 <- glmmTMB(y_value ~ x_within + x_between + (1 | grp), d3)
    ## model_parameters(r_sq1)

    r_sq2 <- glmmTMB(y_value ~ x_within + I(x_within^2) + x_between + (1 | grp), d3)

    ## comparing linear coefs for the different lines
    map_dfr(unique(d3$grp), ~lm(y_value ~ x_value + I(x_value^2), d3[grp==.x]) %>% summary() %>% coef() %>%
                            adt(keep.rownames = "vrbl") %>% .[, .(vrbl, Estimate, grp = .x)])
    # linear coef is slope at x=0

    ## also add between squared
    r_sq3 <- glmmTMB(y_value ~ x_within + I(x_within^2) + x_between + I(x_between^2) + (1 | grp),d3)

    ## only between
    r_sq4 <- glmmTMB(y_value ~ x_between + I(x_between^2) + (1 | grp), d3)

    compare_models(r_sq1, r_sq2, r_sq3, r_sq4)

    ## ggplot(d3, aes(x=x_within, y=y_value, color = factor(grp))) + geom_line()

    ## plot between non-linear relationship
    data.table(x=seq(0,20, by = 0.2)) %>%
        .[, y := -18.9 * x + 0.9*x^2] %>%
        ggplot(aes(x=x, y=y)) + geom_line()

    ## try interactions with actual data: txdctblt and tmitr

    ## first improve .SD handling for demeaning etc

    ## separate within and between calcs: works good enough
    ## vrbls <- c("x", "j")
    ## dtt %>% copy() %>% .[, paste0(vrbls, "_between") := map(.SD, mean), id, .SDcols = vrbls] %>%
    ##     .[, paste0(vrbls, "_within") := map(.SD, ~.x - mean(.x)), id, .SDcols = vrbls]

    ## for now use manual specified interactions

    vrbls <- setdiff(names(dfx), "iso3c")
    dtx_dmnd <- adt(dfx) %>% .[, paste0(vrbls, "_between") := map(.SD, mean), iso3c, .SDcols = vrbls] %>%
        .[, paste0(vrbls, "_within") := map(.SD, ~.x - mean(.x)), iso3c, .SDcols = vrbls]

    
    ## check value for cross-sectional variables: 
    dtx_dmnd[, .SD, .SDcols = keep(names(dtx_dmnd), ~grepl("Ind.tax.incentives", .x))] %>% copy() %>% 
        .[, `:=`(check_between = (Ind.tax.incentives == Ind.tax.incentives_between),
                 check_within = (Ind.tax.incentives == Ind.tax.incentives_within))]
        ## .[, .N, Ind.tax.incentives_within] # within of cross-sectional always 0
        ## .[, summary(.SD), .SDcols = keep(names(.), ~grepl("check", .x))]
        
    ## between: always same as original variables: for cross-sectional, mean(variable) = var
    ## within: not always value (but always 0)
        
    
    
    dfx2 <- cbn_dfs_rates$cbn_no_cult_spending
    vrbls <- setdiff(names(dfx2), "iso3c")
    dtx_dmnd2 <- adt(dfx2) %>% .[, paste0(vrbls, "_between") := map(.SD, mean), iso3c, .SDcols = vrbls] %>%
        .[, paste0(vrbls, "_within") := map(.SD, ~.x - mean(.x)), iso3c, .SDcols = vrbls]

    ## interaction test 1: base model 
    r_xx1 <- glmmTMB(nbr_opened ~ Ind.tax.incentives + (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd)

    ## add TMITR overall 
    r_xx2 <- glmmTMB(nbr_opened ~ Ind.tax.incentives + tmitr_approx_linear20step_lag1 + 
                         (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd)

    ## differentiate tmtir into within/between
    r_xx3 <- glmmTMB(nbr_opened ~ Ind.tax.incentives + tmitr_approx_linear20step_lag1_within +
                         tmitr_approx_linear20step_lag1_between + 
                         (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd)

    ## add interactions
        
    r_xx4 <- glmmTMB(nbr_opened ~ Ind.tax.incentives*tmitr_approx_linear20step_lag1_within +
                         Ind.tax.incentives*tmitr_approx_linear20step_lag1_between + 
                         (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd)

    ## add interactions: cbn2
    r_xx5 <- glmmTMB(nbr_opened ~ Ind.tax.incentives*tmitr_approx_linear20step_lag1_within +
                         Ind.tax.incentives*tmitr_approx_linear20step_lag1_between + 
                         (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd2)

    compare_models(r_xx1, r_xx2, r_xx3, r_xx4, select = "{estimate}{stars}")
    compare_models(r_xx4, r_xx5, select = "{estimate}{stars} ({se}, {p})")


    ## how to easily generate all the dts to predict?
    ## would be best if I could reuse some
    ## dt_pred <-
    l_to_expand <- list(iso3c = "lul", SP_POP_TOTLm_lag0_uscld = 100, Ind.tax.incentives = c(0,1))

    
    l_tpred_within <- list(tmitr_approx_linear20step_lag1_within = dtx_dmnd2 %>% copy() %>%
                               .[, seq(min(tmitr_approx_linear20step_lag1_within),
                                       max(tmitr_approx_linear20step_lag1_within), by = 0.3)]) %>%
        c(l_to_expand, list(tmitr_approx_linear20step_lag1_between = 0))

    dt_pred_tmitr_within <- do.call("expand.grid", l_tpred_within) %>% adt() %>% 
        .[, c("pred", "se") := predict(r_xx5, newdata = . , se.fit = T, type = "link")] %>% 
        .[, .(tmitr = tmitr_approx_linear20step_lag1_within, Ind.tax.incentives, pred, se, src = "within")]

    
    dt_pred_tmitr_between <- list(tmitr_approx_linear20step_lag1_between = dtx_dmnd2 %>% copy() %>%
             .[, seq(min(tmitr_approx_linear20step_lag1_between),
                     max(tmitr_approx_linear20step_lag1_between), by = 0.3)]) %>%
        c(l_to_expand, list(tmitr_approx_linear20step_lag1_within = 0)) %>%
        do.call("expand.grid", .) %>% adt() %>% 
        .[, c("pred", "se") := predict(r_xx5,newdata = ., se.fit = T, type = "link")] %>%
        .[, .(tmitr = tmitr_approx_linear20step_lag1_between, Ind.tax.incentives, pred, se, src = "between")]

    rbind(dt_pred_tmitr_within, dt_pred_tmitr_between) %>%
        .[, `:=`(hi = pred + se, lo = pred - se, Ind.tax.incentives = factor(Ind.tax.incentives))] %>%
        ggplot(aes(x=tmitr, y=pred, color = Ind.tax.incentives, fill = Ind.tax.incentives)) +
        ## geom_point() +
        geom_line() +
        geom_ribbon(mapping = aes(ymin = lo, ymax = hi), alpha = 0.2) + 
        facet_grid(~src)


    ## try overall model: manually add all variables, use lag 1
    t1 <- Sys.time()
    r_xx6 <- glmmTMB(nbr_opened ~ Ind.tax.incentives*tmitr_approx_linear20step_lag1_within +
                Ind.tax.incentives*tmitr_approx_linear20step_lag1_between +
                smorc_dollar_fxm_lag1_within + I(smorc_dollar_fxm_lag1_within^2) +
                smorc_dollar_fxm_lag1_between + I(smorc_dollar_fxm_lag1_between^2) +
                sptinc992j_p99p100_lag1_within + sptinc992j_p99p100_lag1_between +
                shweal992j_p99p100_lag1_within + shweal992j_p99p100_lag1_between +
                hnwi_nbr_5M_lag1_within + hnwi_nbr_5M_lag1_between +
                NY.GDP.PCAP.CDk_lag1_within + NY.GDP.PCAP.CDk_lag1_between +
                pm_density_lag1_within + I(pm_density_lag1_within^2) +
                pm_density_lag1_between + I(pm_density_lag1_between^2) +
                pm_density_global_lag1 + I(pm_density_global_lag1^2) +
                clctr_cnt_cpaer_lag1_within + clctr_cnt_cpaer_lag1_between +
                cnt_contemp_1990 + cnt_contemp_1990_sqrd +
                nbr_closed_cum_global_lag1 + 
                (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                family = nbinom2, dtx_dmnd)
    t2 <- Sys.time()
    t2-t1

    compare_models(r_xx4, r_xx6, select = "{estimate}{stars} ({se}, {p})")

    ## 
    dtx_dmnd %>% copy() %>% .[, .SD, .SDcols = c("iso3c", "year", "pm_density_global_lag1",
                                                 "pm_density_global_lag1_within", "pm_density_global_lag1_between")]

    dtx_dmnd[, .N, pm_density_global_lag1_between]
            
    r_xx7 <- glmmTMB(nbr_opened ~ Ind.tax.incentives*tmitr_approx_linear20step_lag1 +
                         smorc_dollar_fxm_lag1 + I(smorc_dollar_fxm_lag1^2) +
                         sptinc992j_p99p100_lag1 + 
                         shweal992j_p99p100_lag1 + 
                         hnwi_nbr_5M_lag1 + 
                         NY.GDP.PCAP.CDk_lag1 + 
                         pm_density_lag1 + I(pm_density_lag1^2) +
                         pm_density_global_lag1 + I(pm_density_global_lag1^2) +
                         clctr_cnt_cpaer_lag1  + 
                         cnt_contemp_1990 + cnt_contemp_1990_sqrd +
                         nbr_closed_cum_global_lag1 + 
                         (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)),
                     family = nbinom2, dtx_dmnd)
    
    compare_models(r_xx6, r_xx7, select = "{estimate}{stars} ({se}, {p})")
    
    


    

    
}

explr_within_between_decomposition(cbn_dfs_rates)

pred_w_densfdbk <- function(dtxx, mdl, vrbl) {
    ## generate predictions while considering density feedback

    ## use new dataframe: this will get written into
    ## set density 0s
    dtx_fdbk <- copy(dtx_consgdp) %>%
        .[year_id != 1, `:=`(pm_density_lag1 = NA, pm_density_sqrd_lag1 = NA)]

    dtxx <- adt(cbn_dfs_rates$cbn_all)
    dtxx[, mean(nbr_closed_cum_global_lag1)]

    ## cbn_dfs_counts_uscld$cbn_all$nbr_closed_cum_global_lag0
    ## df_open$nbr_closed
    


    for (yearx in sort(unique(dtx_fdbk$year))) {
        ## first get the slice to predict

        
        dtx_year <- copy(dtx_fdbk)[year == yearx] %>%
            .[, pred_consgdp_fdbk := exp(predict(r1, .))] # then predict nbr_opened
        
        ## check values 
        dtx_year[, .(iso3c, year, pm_density_lag1, pred_r1, pred_consgdp_fdbk)]

        ## udpate values that are to be added to next year
        dtx_dens_updtd <- dtx_year %>% copy() %>%
            .[, .(iso3c, year, pred_consgdp_fdbk, pm_density_lag1, SP_POP_TOTLm_lag0_uscld)] %>% 
            .[, dens_rate_uscld := (attr(scale_pm_density, "scaled:center") + # convert density into nbr
                                    attr(scale_pm_density, "scaled:scale")*pm_density_lag1)] %>%
            ## prediction is number (offset gone), density is in per million -> need to
            .[, opnd_rate_uscld := pred_consgdp_fdbk/SP_POP_TOTLm_lag0_uscld] %>% 
            .[, dens_rate_plus_opng_rate := dens_rate_uscld + opnd_rate_uscld] %>%
            .[, dens_new_std := (dens_rate_plus_opng_rate - attr(scale_pm_density, "scaled:center"))/
                    attr(scale_pm_density, "scaled:scale")] # restandardize density

        ## collect predicted values for current year
        dtx_pred_fdbk <- dtx_dens_updtd[, .(iso3c, year, pred_consgdp_fdbk_new = pred_consgdp_fdbk)]
        ## and assignn them to current year with update join
        dtx_fdbk[dtx_pred_fdbk, pred_consgdp_fdbk := pred_consgdp_fdbk_new, on = .(iso3c, year)]


        ## collect values to be assigned for next year
        dtx_dens_new_vlus <- dtx_dens_updtd[, .(iso3c, dens_new_std, year = yearx+1)]
        
        ## update density and density squared for next year, with update join urg
        dtx_fdbk[dtx_dens_new_vlus, `:=`(pm_density_lag1 = dens_new_std,
                                         pm_density_sqrd_lag1 = dens_new_std^2), on = .(iso3c, year)]
        
    }
    
}

explr_clsngs <- function(df_open, cbn_dfs_rates) {
    #' explore model for closings: no point atm 
    
    dtx <- adt(cbn_dfs_rates$cbn_all)[, .(iso3c, year, pm_density_lag1, pm_density_sqrd_lag1, nbr_opened,
                                          NY.GDP.PCAP.CDk_lag1, SP_POP_TOTLm_lag0_uscld)]

    ## model for closings
    dt_clsngs <- df_open %>% adt() %>% .[!is.na(nbr_closed), .(iso3c, year, nbr_closed)] %>% 
        .[copy(dtx), on = .(iso3c, year)] %>%
        .[is.na(nbr_closed), nbr_closed := 0]
    
    rc <- glmmTMB(nbr_closed ~ pm_density_lag1 + pm_density_sqrd_lag1 + NY.GDP.PCAP.CDk_lag1 + (1 | iso3c),
                  dt_clsngs, family = nbinom2)
    compare_parameters(rc)
}

## explr_clsngs(df_open, cbn_dfs_rates)





explr_cntrfctl <- function(cbn_dfs_rates) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;

    dtx <- adt(cbn_dfs_rates$cbn_all)[, .(iso3c, year, pm_density_lag1, pm_density_sqrd_lag1, nbr_opened,
                                          NY.GDP.PCAP.CDk_lag1, SP_POP_TOTLm_lag0_uscld)]

    r1 <- glmmTMB(nbr_opened ~ pm_density_lag1 + pm_density_sqrd_lag1 + NY.GDP.PCAP.CDk_lag1 + 
                      (1 | iso3c) + offset(log(SP_POP_TOTLm_lag0_uscld)), dtx, family = nbinom2)

 
    dtx$pred_r1 <- exp(predict(r1, dtx))
    
    ## ranef(r1) %>% adt() %>% ggplot(aes(x=condval, y=grp)) + geom_col()
    ## fixef(r1)

    margins::margins(r1)

    marginal_effects(r1, type = "link") %>% adt() %>% .[, map(.SD, mean)]
   
    ## model 2 with DV pm_density_lag0
    ## r2 <- glmmTMB(pm_density_lag0 ~ pm_density_lag1 + pm_density_sqrd_lag1 + NY.GDP.PCAP.CDk_lag1 +
    ##                   (1 | iso3c), dtx)

    ## dtx$pred_r2 <- predict(r2, dtx)
    
    ## gdp stays constant, not yet feedback into density
    dtx_consgdp <- copy(dtx) %>%
        .[, year_id := 1:.N, iso3c] %>% # set up year_ids, 
        .[year_id != 1, NY.GDP.PCAP.CDk_lag1 := NA] %>% # use those year_ids to keep stuff constant
        .[,  NY.GDP.PCAP.CDk_lag1 := nafill(NY.GDP.PCAP.CDk_lag1, type = "locf")]

    ## ggplot(dtx_consgdp, aes(x= year, y = NY.GDP.PCAP.CDk_lag1, color = iso3c)) + geom_line()
    dtx$pred_consgdp_nofdbk <- exp(predict(r1, dtx_consgdp))

    ## gdp stays constant, feedback into density
    
    scale_pm_density <- scale(cbn_dfs_rates_uscld$cbn_all$pm_density_lag1)
    
    ## 
    attr(scale_pm_density, "scaled:center") + attr(scale_pm_density, "scaled:scale")*-0.6

    ## 0.0043022 is now our unstandardzized count

    (0.0043022 - attr(scale_pm_density, "scaled:center"))/attr(scale_pm_density, "scaled:scale")
    ## -0.5842529 is now the restandardized value

    
    copy(dtx) %>%
        .[, year_id := 1:.N, iso3c] %>% # set up year_ids,
        .[year_id %in% c(1,2), .(iso3c, year, nbr_opened, pm_density_lag1, pred_r1, pred_consgdp_nofdbk)]

    ## use new dataframe: this will get written into
    dtx_fdbk <- copy(dtx_consgdp) %>%
        .[year_id != 1, `:=`(pm_density_lag1 = NA, pm_density_sqrd_lag1 = NA)]


    for (yearx in sort(unique(dtx_fdbk$year))) {
        ## first get the slice to predict

        
        dtx_year <- copy(dtx_fdbk)[year == yearx] %>%
            .[, pred_consgdp_fdbk := exp(predict(r1, .))] # then predict nbr_opened
        
        ## check values 
        dtx_year[, .(iso3c, year, pm_density_lag1, pred_r1, pred_consgdp_fdbk)]

        ## udpate values that are to be added to next year
        dtx_dens_updtd <- dtx_year %>% copy() %>%
            .[, .(iso3c, year, pred_consgdp_fdbk, pm_density_lag1, SP_POP_TOTLm_lag0_uscld)] %>% 
            .[, dens_rate_uscld := (attr(scale_pm_density, "scaled:center") + # convert density into nbr
                                    attr(scale_pm_density, "scaled:scale")*pm_density_lag1)] %>%
            ## prediction is number (offset gone), density is in per million -> need to
            .[, opnd_rate_uscld := pred_consgdp_fdbk/SP_POP_TOTLm_lag0_uscld] %>% 
            .[, dens_rate_plus_opng_rate := dens_rate_uscld + opnd_rate_uscld] %>%
            .[, dens_new_std := (dens_rate_plus_opng_rate - attr(scale_pm_density, "scaled:center"))/
                    attr(scale_pm_density, "scaled:scale")] # restandardize density

        ## collect predicted values for current year
        dtx_pred_fdbk <- dtx_dens_updtd[, .(iso3c, year, pred_consgdp_fdbk_new = pred_consgdp_fdbk)]
        ## and assignn them to current year with update join
        dtx_fdbk[dtx_pred_fdbk, pred_consgdp_fdbk := pred_consgdp_fdbk_new, on = .(iso3c, year)]


        ## collect values to be assigned for next year
        dtx_dens_new_vlus <- dtx_dens_updtd[, .(iso3c, dens_new_std, year = yearx+1)]
        
        ## update density and density squared for next year, with update join urg
        dtx_fdbk[dtx_dens_new_vlus, `:=`(pm_density_lag1 = dens_new_std,
                                         pm_density_sqrd_lag1 = dens_new_std^2), on = .(iso3c, year)]
        
    }
    

    dtx_fdbk[year %in% c(1995, 1996, 1997), .(iso3c, year, pm_density_lag1, pred_consgdp_fdbk)]
    dtx_fdbk[, map(.SD, ~sum(is.na(.x)))]
    
    ## compare observed and feedback-predicted densities
    dt_denscprn <- rbind(dtx[, .(iso3c, year, pm_density_lag1, src = "obs")], 
                         dtx_fdbk[, .(iso3c, year, pm_density_lag1, src = "fdbk")])

    dt_denscprn %>% .[iso3c %in% c("DEU", "USA", "CHE", "POL", "CYP", "CHN")] %>% 
        ggplot(aes(x=year, y=pm_density_lag1, color = iso3c, linetype = src)) + geom_line()
    ## feedback density is super smooth

    ## check how often real density decreases
    dt_denscprn %>% copy() %>% .[src == "obs"] %>%
        .[, pm_density_lag2 := shift(pm_density_lag1), iso3c] %>% 
        .[, diff := pm_density_lag1 - pm_density_lag2] %>% 
        ## .[, .N, fifelse(diff < 0, T, F)]
        ggplot(aes(x=diff)) + geom_density() +
        xlim(c(-0.05, 0.05)) # huh there are actually quite some decreases.. 
        
        


    dtx$pred_consgdp_fdbk <- dtx_fdbk$pred_consgdp_fdbk

    dtx_nocons <- copy(dtx)[, iso3c := paste0(iso3c, "lul")]
    dtx$pred_nocons <- exp(predict(r1, dtx_nocons))


    
    ## also need smooth predictions for actual number of openings
    dtx_predvs <- dtx[iso3c %in% c("DEU", "USA", "CHE", "POL", "CYP", "CHN")] %>% copy() %>%
        .[, nbr_opened_smooth := predict(loess(nbr_opened ~ year),year), iso3c] %>%
        melt(id.vars = c("iso3c", "year"),
             measure.vars = c("nbr_opened_smooth", "pred_r1", "pred_consgdp_nofdbk", "pred_consgdp_fdbk",
                              "pred_nocons"))
    
    ggplot(dtx_predvs, aes(x=year, y=value, color = iso3c, linetype = variable)) +
        geom_line() +
        facet_wrap(~variable, scales = "free")

    ## ggplot() +
    ##     geom_smooth(dtx_predvs, mapping = aes(x=year, y=pm_density_lag0, color = iso3c), se=F, span = 1) + 
    ##     geom_line(dtx_predvs, mapping = aes(x=year, y=pred_r2, color = iso3c), linetype = "dashed")

    ## seeing how many have opened at all:
    ## by variable 
    dtx_predvs[, .(nbr_opnd_ttl = sum(value)), variable]

    ## by country*variable
    dtx_predvs[, .(opngs = sum(value)), .(variable, iso3c)] %>%
        dcast.data.table(iso3c ~ variable, value.var = "opngs")

}



explr_cntrfctl(cbn_dfs_rates)




explr_dt_SD_handling <- function() {
    fx <- function(x) {
        if (as.character(match.call()[[1]]) %in% fstd){browser()}
        1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
        list(y=x, z=x)}

    ## test data
    dtt <- data.table(id = c("a", "a", "b", "b"), x= c(1,2,3,4), j = c(5,6,7,8))
 
    ## doesn't work 
    dtt[, map(.SD, ~c(y=.x, z=.x)), id]

    ## works with single column 
    dtt[, fx(x), id] 
    
    ## but not with .SD
    dtt[, map(.SD, fx), id]

    ## even when only specifying one column
    dtt[, map(.SD, fx), id, .SDcols = "x"]

    ## kinda works with passing .SD entirely to fx: then renames columns into y.id, y.x, y.j
    dtt[, fx(.SD)]
    ## works with .SDcols
    dtt[, fx(.SD), .SDcols = c("x", "j")]
    
    ## but not with by=id: All items in j=list(...) should be atomic vectors or lists
    dtt[, fx(.SD), by=id, .SDcols = c("x", "j")]
    dtt[, fx(.SD), by=id]
    
    ## do I need to group by something?
    ## yeah would be nice to be able to group by country: return original, demeaned and between value
    dtt[, map(.SD, ~sum(is.na(.x)))]

    ## try again with map(.SD)
    dtt[, map(.SD, ~list(m=.x, k=.x))]

    ## unlist
    dtt[, map(.SD, ~list(m=.x, k=.x)), id, .SDcols = c("x", "j")] %>%
        .[1, x]
        ## .[, map(.SD, unlist)]
    ## idk even the names are just gone.. 

    ## try some unlisting/splitting result of mapping call:
    ## "All items in j=list(...) should be atomic vectors or lists"
    ## -> maybe if I just create enough lists it'll work? 

    dtt[, c(map(.SD, ~list(m=.x, k=.x))), id, .SDcols = c("x", "j")]

    ## dtt %>% copy() %>% .[, oo := "oo"] %>% 
    ##     split(by = "oo", flatten = T)
    ## split:

    ## separate within and between calcs: works good enough
    vrbls <- c("x", "j")
    dtt %>% copy() %>% .[, paste0(vrbls, "_between") := map(.SD, mean), id, .SDcols = vrbls] %>%
        .[, paste0(vrbls, "_within") := map(.SD, ~.x - mean(.x)), id, .SDcols = vrbls]

}
## ** parallelization tests
## ## create separate dataframes
## l_dtx <- imap_dfr(vrbl_cbns[1:3], ~data.table(cbn_name = .y, vrbl = paste0(.x, "_lag0"))) %>%
##     .[vrbl %in% vrbl_tdesc] %>%         
##     split(by = c("cbn_name", "vrbl")) %>%
##     map(~dtx[cbn_name == .x$cbn_name, .SD, .SDcols = c(vvs$base_vars, .x$vrbl, "cbn_name")])

## regres <- map(l_dtx, glmmTMB()


## cl <- makeCluster(4)
## clusterEvalQ(cl, {library(glmmTMB)})

## t1 = Sys.time()
## res_clusterlapply <- clusterApply(cl, l_dtx[1:12], \(x) glmmTMB(get(names(x)[3]) ~ year + (1 | iso3c), x))
## stopCluster(cl)

## t2 = Sys.time()
## res_mclapply <- mclapply(l_dtx, \(x) glmmTMB(get(names(x)[3]) ~ year + (1 | iso3c), x), mc.cores = 4)
## t3 = Sys.time()
## dtx_yearreg <- dtx_melt[, reg_helpr(.SD), .(variable, cbn_name)]
## t4 = Sys.time()


## ** test different hnwi ~ year model configurations
## https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet different model specifications
## slope and intercept deviations allowed to be correlated
r_rs1 <- dtx_melt[cbn_name == "cbn_all" & variable == "hnwi_nbr_1M_lag0"] %>% copy() %>%
    .[, year := year - 2000] %>% 
    glmmTMB(value ~  year + (1 + year | iso3c), .)

coef(r_rs1) %>% chuck("cond", "iso3c") %$% cor(`(Intercept)`, year)
ranef(r_rs1) %>% chuck("cond", "iso3c") %$% cor(`(Intercept)`, year)

coef(r_rs1) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>%
    adt(cbn_dfs_rates_uscld$cbn_all)[, .(mean_hnwi = mean(hnwi_nbr_1M_lag0)), iso3c][., on = "iso3c"] %>% 
    ggplot(aes(x=`(Intercept)`, y=year, label = iso3c, color = mean_hnwi)) + geom_point() + geom_text_repel()
## ggsave(paste0(SKETCH_DIR, "hnwi_1M_vrbl~year_year~cons.pdf"))

adt(cbn_dfs_rates_uscld$cbn_all)[iso3c %in% c("ITA", "AUT", "GBR", "NZL", "DEU", "CZE", "CHE")] %>%
    ggplot(aes(x=year, y= hnwi_nbr_1M_lag0, color = iso3c)) + geom_line()



r_rs2 <- dtx_melt[cbn_name == "cbn_all" & variable == "hnwi_nbr_1M_lag0"] %>% copy() %>%
    glmmTMB(value ~ year + (1 | iso3c) + (0 + year | iso3c), .)


## compare different offsets -> not that much difference
## r_rs2_m2k <- dtx_melt[cbn_name == "cbn_all" & variable == "hnwi_nbr_1M_lag0"] %>% copy() %>%
##     .[, year := year - 2000] %>% 
##     glmmTMB(value ~ year + (1 | iso3c) + (0 + year | iso3c), .)

## r_rs2_m2k10 <- dtx_melt[cbn_name == "cbn_all" & variable == "hnwi_nbr_1M_lag0"] %>% copy() %>%
##     .[, year := year - 2010] %>% 
##     glmmTMB(value ~ year + (1 | iso3c) + (0 + year | iso3c), .)

## library(performance)
## library(parameters)
## compare_models(r_rs2, r_rs2_m2k, r_rs2_m2k10, exponentiate = T)

## compare slopes between different year configurations
rbind(
    coef(r_rs2) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>% .[, src := "rs2"],
    coef(r_rs2_m2k) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>% .[, src := "m2k"],
    coef(r_rs2_m2k10) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>% .[, src := "m2k10"]) %>%
    ## ggplot(aes(x=year, y=iso3c, fill = src)) + geom_col(position = position_dodge2())
    xtsum(year, iso3c)

## compare different model configurations: "zero"- cor enforced or not
rbind(
    coef(r_rs1) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>% .[, src := "rs1"],
    coef(r_rs2) %>% chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>% .[, src := "rs2"]) %>%
    xtsum(year, iso3c)


coef(r_rs2) %>% chuck("cond", "iso3c") %$% cor(`(Intercept)`, year)
ranef(r_rs2) %>% chuck("cond", "iso3c") %$% cor(`(Intercept)`, year)

r_rs2 %>% coef() %>% chuck("cond", "iso3c") %$% cor(`(Intercept)`, year)

r_rs2_m2k10 %>% coef() %>% chuck("cond", "iso3c") %>%
    adt(keep.rownames = "iso3c") %>% .[, setnames(.SD, "(Intercept)", "cons")] %>% #  %$% cor(cons, year)
    ggplot(aes(x=cons, y=year)) + geom_point()


## *** old functions to assess bivariate variable ~ year relations
reg_helpr <- function(dtxx) {
    #' collect constant/year coef/se
    ## glmmTMB(value ~ year + (1 | iso3c), dtxx) %>% summary() %>% coef() %>% chuck("cond") %>%
    ##     adt(keep.rownames = "vrbl") %$% setNames(Estimate, c("cons", "year")) %>% as.list()
    glmmTMB(value ~ year + (1 | iso3c), dtxx) %>% summary() %>% coef() %>% chuck("cond") %>%
        adt(keep.rownames = "vrbl") %>% .[, .(vrbl, coef = Estimate, se = `Std. Error`)] %>%
        .[vrbl == "(Intercept)", vrbl := "cons"] %>% melt(id.vars = "vrbl") %$%
        setNames(value, paste0(vrbl, "_", variable)) %>% as.list()
}

reg_helper_rs <- function(dtxx) {
    #' collect random slopes
    ## dtx_melt[variable == "gptinc992j_lag0" & cbn_name == "cbn_all"] %>% 
    glmmTMB(value ~ year + (1 + year | iso3c), dtxx) %>% coef() %>% chuck("cond", "iso3c") %>%
        adt(keep.rownames = "iso3c") %>% .[, setnames(.SD, "(Intercept)", "cons")]
}

## *** old plotting function of country slopes
## plot slope distribution
    dtx_yearreg_rs %>% .[, vrbl := gsub("_lag[0-5]", "", variable)] %>%
        .[!grepl("closed_cum", vrbl) & !grepl("pm_density_global", vrbl)] %>% 
        vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))] %>%
        ## ggplot(aes(x=year)) + geom_density()
        ggplot(aes(x=year, y=variable , fill = cbn_name)) +
        geom_violin(bw = 0.005, alpha = 0.2, linewidth = 0.1) +
        geom_jitter(mapping = aes(color = cbn_name), size = 0.5,
                    position = position_jitterdodge(jitter.height = 0)) + 
        facet_grid(hyp ~ ., scales = "free", space = "free") 
## coord_cartesian(xlim = c(-0.5, 0.5))

## *** distribution of slopes, super complicated tho, also kinda pointless, histogram shows unevenness better
## measures of distribution of random slopes: 
    dt_crycoefs[, .(variance = var(year), sd = sd(year), gini = Gini(year - min(year))), .(variable, cbn_name)] %>%
        melt(id.vars = c("variable", "cbn_name"), variable.name = "measure", value.name = "size") %>%
        ggplot(aes(x=size, y=variable, fill = cbn_name)) +
        geom_col(position = position_dodge2()) +
        facet_grid(~measure, scales = "free")
    ## actually looks a lot like cbn_all is weird for all HNWI variables


## *** some kind of early tests for value ~ year developments, not replaced by more systematic approach
gen_res_velp_tests <- function(cbn_dfs_rates) {
    

    ## dfx <- cbn_dfs_rates$cbn_all

    ## r_smorc <- glmmTMB(smorc_dollar_fxm_lag0 ~ year + (1 | iso3c), dfx)
    
    ## r_rs_smorc <- glmmTMB(smorc_dollar_fxm_lag0 ~ year + ( year | iso3c), dfx)
    
    ## r_tmitr <- glmmTMB(tmitr_approx_linear20step_lag0 ~ year + (1 | iso3c), dfx)
    ## ## summary(
    
    ## summary(r_rs_smorc)
    ## summary(r_tmitr)

    
    ## stargazer(r_smorc)
    ## library(huxtable)
    ## huxreg(r_smorc)
    
    ## fixef(r_smorc)

    ## summary(r_smorc) %>% coef() %>% .[["cond"]] %>% adt()

    ## summary(r_rs_smorc) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl")
    ## ## %$% createTexreg(coef.names = vrbl, coef = Estimate, se = `Std. Error`, pvalues = `Pr(>|z|)`) %>%
    ## ##     screenreg()
    ## get_r_gof(r_rs_smorc, summary(r_rs_smorc))
    ## get_r_gof(r_smorc, summary(r_smorc))
    

    dtx_yearres_vis <- dtx_yearreg %>% copy() %>% 
        .[, vrbl := gsub("_lag[0-5]", "", variable)] %>%
        .[!grepl("closed_cum", vrbl) & !grepl("pm_density_global", vrbl)] %>% 
        vvs$hyp_mep_dt[., on = "vrbl"] %>% 
        .[, vrbl := factor(vrbl, levels = rev(names(vvs$vrbl_lbls)))]

    ## plots of random intercept year coefficient
    dtx_yearres_vis %>% 
        ggplot(aes(x=year_coef, y=variable, color = cbn_name)) +
        geom_point() +
        geom_errorbarh(mapping = aes(xmin = year_coef - 1.96*year_se, xmax = year_coef + 1.96*year_se)) + 
        facet_grid(hyp ~ ., scales = "free", space = "free")
    

    map(l_dtx, ~glmmTMB(value ~ year + (1 | iso3c), .x))




    ## dfx %>% adt() %>%
    ##     .[, .(iso3c, year, tmitr = tmitr_approx_linear20step_lag0, shweal992j_p90p100_lag0)] %>% 
    ##     ## .[, .(imap(.SD, ~list(paste0(.y, "asdf") = .x))), iso3c]
    ##     ## .[, .(lapply(.SD, \(x) setNames(x, sample(letters, 1))))]
    ##     ## .[, .(map(.SD, ~.x + 1)), .SDcols = c("year", "tmitr")]
    ##     ## .[, map(.SD, ~sum(is.na(.x)))] # werks 
    ##     ## .[, lapply(.SD, \(x) sum(is.na(x)))] # werks
    ##     ## .[, imap(.SD, ~.x)] # werks
    ##     ## .[, c(imap(.SD, ~setNames(.x, paste0(names(.x), "_lul"))))] # renaming doesn't seem to work well
    ##     .[, c(imap(.SD, ~c("dd" = .x, "kk" = .x)))]
                  
    ## with melting
    dtx_wintwn <- dfx %>% adt() %>%
        .[, .(iso3c, year, year_vlu = year, tmitr = tmitr_approx_linear20step_lag0, shweal992j_p90p100_lag0)] %>%
        melt(id.vars = c("iso3c", "year")) %>%
        .[, between := mean(value), .(iso3c, variable)] %>%
        .[, within := value - between] %>%
        melt(id.vars = c("iso3c", "variable", "year"), variable.name = "form") %>%
        dcast.data.table(iso3c + year ~ variable + form)
        
    
    ## .[, year_between := mean(year), iso3c] %>%
        ## .[, year_within := year - year_between]

        ## .[, tmitr_crymean := mean(tmitr), iso3c] %>%
        ## .[, tmitr_crywin := tmitr - tmitr_crymean]

    ## single FE? compare with plm: seems pretty much the same
    r_tmitr_win <- glmmTMB(tmitr ~ year_crywin + (1 | iso3c), dtx)
    sumry(r_tmitr_win) %>% coef() %>% chuck("cond")
    
    r_tmitr_plm <- plm(tmitr ~ year, dtx, model = "within", index = "iso3c")
    sumry(r_tmitr_plm)
    
    ## what happens if I don't add random intercept?
    r_tmitr_win2 <- glmmTMB(tmitr ~ year_within, dtx)
    sumry(r_tmitr_win2) %>% coef() %>% chuck("cond")
          

    ## within-between decomposition
    r_tmitr_wintwn <- glmmTMB(tmitr ~ year_within + year_between + (1 | iso3c), dtx)
    r_tmitr_wintwn2 <- glmmTMB(tmitr ~ year_within + year_between + (1 + year_within | iso3c), dtx)
    sumry(r_tmitr_wintwn)
    sumry(r_tmitr_wintwn2)

    ## next section: pure FE: need same FE coefs
    ## 1. zero intercept 
    r_tmitr_wintwn3 <- glmmTMB(tmitr_value ~ 0 + year_vlu_within + iso3c, dtx_wintwn)
    sumry(r_tmitr_wintwn3) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl")
    
    ## 2. demeanded response 
    r_tmitr_wintwn4 <- glmmTMB(tmitr_within ~ year_vlu_within + iso3c, dtx_wintwn)
    sumry(r_tmitr_wintwn4) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl")

    ## yup indeed the same

    ## on to mixed model
    r_tmitr_mixed <- glmmTMB(tmitr_value ~ year_vlu_within + year_vlu_between + (1 | iso3c), dtx_wintwn)
    sumry(r_tmitr_mixed) %>% coef() %>% chuck("cond") %>% adt(keep.rownames = "vrbl")
    ## within effect is still the same, noice
    ## has slightly lower z tho 

    ## dtx_wintwn[, .N, year_vlu_between]
    ## compare with random effects model
    r_tmitr_random <- glmmTMB(tmitr_value ~ year_vlu_value + (1 | iso3c), dtx_wintwn)
    sumry(r_tmitr_random)
    ## huh coef is pretty much the same..
    ## hmm between coef is basically 0 so maybe not surprising

    ## what does between mean substantially anyways? 
    ## countries with higher mean year (later coverage) don't have higher/lower mean tmitrs
    ## than those with lower mean year (only early years or entier period)
    ## curious: there kinda should be, since there's a period effect?
    ## maybe it's not after controlling for within-chnage?

    model_performance(r_tmitr_mixed)
    check_model(r_tmitr_mixed)

    ## splines
    knts <- quantile(dtx_wintwn$year_vlu_value, probs = seq(0.1, 0.9, 0.4))
    knts <- quantile(dtx_wintwn$year_vlu_value, probs = seq(0.25, 0.75, 0.25))
    
    
    r_tmitr_splines <- glmmTMB(tmitr_value ~ bs(year_vlu_value, knots = knts, degree = 1) +
                                   (1 | iso3c), dtx_wintwn)

    sumry(r_tmitr_splines) %>% coef()
    
    ## random slopes LUL
    r_tmitr_splines_rs <- glmmTMB(tmitr_value ~ bs(year_vlu_value, knots = knts, degree = 1) +
                                   (year_vlu_value | iso3c), dtx_wintwn)
    
    sumry(r_tmitr_splines_rs)

    ## splines2: needs degree = 0 to get linear estimation
    r_tmitr_splines2 <- glmmTMB(tmitr_value ~ ibs(year_vlu_value, knots = knts, degree = 0) +
                                   (1 | iso3c), dtx_wintwn)
    sumry(r_tmitr_splines2) %>% coef()

    

    dt_pred <- expand.grid(year_vlu_value = min(dtx_wintwn$year_vlu_value):max(dtx_wintwn$year_vlu_value),
                           ## iso3c = c("DEU", "USA", "CHN", "overall", "CHL", "JPN", "ZAF", "SVN")) %>%
                           iso3c = c("overall", sample(unique(dtx_wintwn$iso3c), 8))) %>% 
        adt()
        
    dt_pred$pred <- predict(r_tmitr_splines, dt_pred)# , se.fit = T)
    dt_pred$pred2 <- predict(r_tmitr_splines2, dt_pred)# , se.fit = T)
    dt_pred$predrs <- predict(r_tmitr_splines_rs, dt_pred)# , se.fit = T)

    dt_pred %>% melt(id.vars = c("year_vlu_value", "iso3c")) %>% 
        ggplot(aes(x=year_vlu_value, y = value, color = iso3c)) +
        geom_line() +
        facet_grid(~variable)
    
    model_parameters(r_tmitr_splines)
    
    compare_performance(r_tmitr_splines, r_tmitr_splines2, r_tmitr_splines_rs)
    compare_models(r_tmitr_splines, r_tmitr_splines2, r_tmitr_splines_rs)

    

    ## plotting spaghetti
    ggplot() +
        ## geom_smooth() + 
        geom_line(dtx, mapping = aes(x=year, y = tmitr, group = iso3c), alpha = 0.5) +
        geom_line(dtx[, .(mean_tmitr = mean(tmitr)), year],
                  mapping = aes(x=year, y=mean_tmitr),
                  linewidth = 3)



    ## check that my demeaning approach is the same as datawizard
    dtx_dmeand <- cbind(
        adt(dfx)[, .(iso3c, year, tmitr = tmitr_approx_linear20step_lag0)],
        datawizard::demean(adt(dfx)[, .(iso3c, year, tmitr = tmitr_approx_linear20step_lag0)],
                           select = "year", group = "iso3c"))

    rbind(dtx[, .(iso3c, year, year_between, year_within, source = "manual")],
          dtx_dmeand[, .(iso3c, year, year_between, year_within, source = "wizard")]) %>%
        melt(id.vars = c("iso3c", "year", "source")) %>%
        dcast.data.table(year + iso3c + variable ~ source) %>%
        .[, diff := manual - wizard] %>%
        .[, .N, diff]
    ## looks good
}

## *** old/test plt_velp code
#' plot of "bivariate" statistics: value ~ year
        
    ## dtx_yearreg <- dtx_melt[, reg_helpr(.SD), .(variable, cbn_name)]

    ## dtx_yearreg_rs <- dtx_melt[, reg_helper_rs(.SD), .(variable, cbn_name)]

    ## 
    ## dtx_yearreg_rs95 <- dtx_melt %>% copy() %>%
    ##     .[, year := year - min(year)] %>% 
    ##     .[, reg_helper_rs(.SD), .(variable, cbn_name)]

    
    ## ## fix weird 200M cbn2 issue: can be fixed by subtracting year
    ## dtx_melt[cbn_name == "cbn_no_cult_spending" & variable == "hnwi_nbr_200M_lag0"] %>% copy() %>%
    ##     .[, year := year-min(year)] %>% 
    ##     glmmTMB(value ~ year + (year | iso3c), .) %>% coef() %>% 
    ##     chuck("cond", "iso3c") %>% adt(keep.rownames = "iso3c") %>%
    ##     ggplot(aes(x=year)) + geom_density()
        
    ## t4 = Sys.time()
    ## %>% coef() %>% chuck("cond") %>%
    ##     adt(keep.rownames = "vrbl") %>% .[, .(vrbl, coef = Estimate, se = `Std. Error`)] %>%
    ##     .[vrbl == "(Intercept)", vrbl := "cons"] %>% melt(id.vars = "vrbl") %$%
    ##     setNames(value, paste0(vrbl, "_", variable))

    ## t2-t1
    ## t3-t2
    ## t4-t3
    ## look at countries with largest slopes
    ## dt_crycoefs[variable == "hnwi_nbr_5M_lag0", head(.SD[order(-year)], 5), .(cbn_name)] #[, .N, iso3c]
    
    ## distribution of year slopes
    ## dt_crycoefs[variable == "hnwi_nbr_1M_lag0"] %>%
    ##     ggplot(aes(x=year)) + geom_histogram() + facet_grid(cbn_name ~ ., scales = "free")

    ## ## relation between slope and intercept
    ## dt_crycoefs[variable == "hnwi_nbr_1M_lag0" & cbn_name == "cbn_no_cult_spending_and_mitr"] %$%
    ##     cor(cons, year)
    ## ## perfect negative HUH?
    ## ggplot(aes(x=cons ,y=year)) + geom_point()
    
    
    ## look at muh random slopes
    ## cbn_dfs_rates_uscld$cbn_all %>%
    ##     ggplot(aes(x=year, y=hnwi_nbr_1M_lag0, group = iso3c)) + geom_point() +
    ##     geom_smooth(method = "lm", se = F, linewidth = 0.1)

## ** more outlier hunting
reg_res_objs$dt_velp_crycoefs[grepl("hnwi_nbr", vrbl)] %>%
    .[, rbind(head(.SD[order(year)],3), head(.SD[order(-year)],4)), .(cbn_name, vrbl)] %>%
    ggplot(aes(x=year, y=vrbl, label = iso3c)) + geom_point() + facet_grid(~cbn_name) + geom_text_repel() +
    geom_vline(xintercept = 0, linetype = "dashed")

## filter(cbn_dfs_rates_uscld$cbn3, iso3c %in% c("IRL", "QAT", "SAU", "DEU", "USA", "CHE", "AUS")) %>%
## filter(cbn_dfs_rates_uscld$cbn3, iso3c %in% c("NLD", "FRA", "BEL", "DEU", "CZE")) %>%
filter(cbn_dfs_rates_uscld$cbn3, iso3c %in% c("USA", "CHE", "DEU", "AUS", "CYP")) %>% copy() %>% 
    mutate(hnwi_nbr_5M_lag0 = log(hnwi_nbr_5M_lag0)) %>% 
    ## filter(iso3c == "SAU") %>% select(iso3c, year, hnwi_nbr_200M_lag0)
    viz_lines(y="hnwi_nbr_5M_lag0", duration = 1)


filter(cbn_dfs_rates_uscld$cbn3, iso3c %in% sample(unique(cbn_dfs_rates_uscld$cbn3$iso3c),5)) %>%
    viz_lines(y="hnwi_nbr_5M_lag0", duration = 1)

## check countries that differ between cbn 1 and 2
dcry_cbn12 <- setdiff(unique(cbn_dfs_rates_uscld$cbn2$iso3c), unique(cbn_dfs_rates_uscld$cbn1$iso3c))


filter(cbn_dfs_rates_uscld$cbn2, iso3c %in% dcry_cbn12) %>%
    mutate(region = countrycode(iso3c, "iso3c", "un.region.name")) %>%
    viz_lines(y="hnwi_nbr_5M_lag0", facets = "region")
## ** old code


library(patchwork)

reg_res62 <- gen_reg_res(setup_regression_folders_and_files("v62"))
reg_res63 <- gen_reg_res(setup_regression_folders_and_files("v63"))
reg_res64 <- gen_reg_res(setup_regression_folders_and_files("v64"))

reg_res63$plts$plt_best_models_condensed + reg_res64$plts$plt_best_models_condensed
reg_res63$plts$plt_reg_res_within + reg_res64$plts$plt_reg_res_within
reg_res63$plts$plt_best_models_wlag + reg_res64$plts$plt_best_models_wlag
reg_res63$plts$plt_coef_violin + reg_res64$plts$plt_coef_violin

    


## render all plots to file
## lapply(names(reg_res$plts), \(x) render_reg_res(x, fldr_info))


## ** version comparison 
## read in stuff, construct objects

reg_res_v48 <- gen_reg_res(setup_regression_folders_and_files("v48"))
reg_res_v49 <- gen_reg_res(setup_regression_folders_and_files("v49"))

setdiff(names(reg_res_v48$plt_cfgs), names(reg_res_v48$plts))

## reg_res_v48$plt_cfgs <- gen_plt_cfgs()
## reg_res_v48$plts <- gen_reg_res_plts(reg_res_v48$reg_res_objs, vvs, NBR_MDLS)
## render_reg_
render_all_reg_res_plts(reg_res_v48, "v48")
render_all_reg_res_plts(reg_res_v49, "v49")


reg_res_vsns <- list(v48 = reg_res_v48, v49=reg_res_v49)
cpr_vrsns(reg_res_vsns)



## filter(reg_res_objs$gof_df_cbn, gof_names == "log_likelihood") %>% 




## reg_anls_base$df_reg_anls_cfgs_wide$loop_nbr %>% table()


## render_reg_res(reg_res$plts$cbn_log_likelihoods, fldr_info)
## render_reg_res(reg_res$plts$best_models_condensed, fldr_info)

## ** version comparison: compare v63 and v64 to v62

## compare log-likelihood distributions between v62 (all variables with up to 5 lags), v63 (all lags = 1),
## and v64 (some variable combinations, up to 3 lags)
dt_llcpr_long <- rbind(reg_res62$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v62"),
        by = .(cbn_name, vrbl_choice)],
      reg_res63$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v63"),
        by = .(cbn_name, vrbl_choice)],
      reg_res64$reg_res_objs$gof_df_cbn %>% adt() %>%
      .[gof_names == "log_likelihood", .(max_ll = max(gof_value), version = "v64"),
        by = .(cbn_name, vrbl_choice)])

## actual comparison: first cast into wide
dt_llcpr_wide <- dt_llcpr_long %>% 
    dcast.data.table(cbn_name + vrbl_choice ~ version, value.var = "max_ll") %>%
    .[, `:=`(d63 = v62- v63,
             d64 = v62- v64)] 


dt_llcpr_long2 <- dt_llcpr_wide %>% 
    melt(id.vars = c("cbn_name", "vrbl_choice"), measure.vars = c("d63", "d64"))

## plot the differences from v63/v64 to v62
dt_llcpr_long2 %>%
    ggplot(aes(x=value, color = variable)) +
    geom_density()
    

dt_llcpr_long2[, .(mean_diff = mean(value, na.rm = T)), variable]
## d63           7.33
## d64           2.86



## ** evaluate model convergence consistency

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    group_by(regcmd, cbn_name, lag_spec) %>%
    summarize(nbr_mdls = len(mdl_id), nbr_unq_gof = n_distinct(gof_value)) %>%
    pull(nbr_unq_gof) %>% table()

## ** evaluate possible savings of better model caching 

reg_res_objs$gof_df_cbn %>% filter(gof_names == "log_likelihood") %>%
    ## mutate(vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>% 
    group_by(regcmd, cbn_name, vrbl_choice) %>%
    summarize(nbr_mdls = len(vrbl_choice), nbr_unq_mdls = n_distinct(lag_spec)) %>%
    ungroup() %>%
    summarize(sum(nbr_mdls), sum(nbr_unq_mdls))





   
## ** step-wise optimization starts here 

plot_stacker <- function(dfx, ystack, xstack, shape_clm = NULL, color_clm="lag") {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' stack coef results vertically
    #' assumes: lag (points get colored by), coef, min, max, sig, vrbl_name_unlag

    ## base_aes <- aes(y=!!sym(ystack))

    ## programmatically edit the aes: 
    ## modify color and shape based on function input
    
    point_aes <- aes(x=coef)

    if (!is.null(shape_clm)) {
        point_aes <- c(point_aes, aes(shape = !!sym(shape_clm)))
        class(point_aes) <- "uneval"
    }

    if (!is.null(color_clm)) {
        point_aes <- c(point_aes, aes(color = !!sym(color_clm)))
        class(point_aes) <- "uneval"
    }
    
    ## setdiff(unique(dfx$vrbl_name_unlag), names(vvs$vrbl_lbls))

    dfx$vrbl_name_unlag <- factor(dfx$vrbl_name_unlag, levels = names(vvs$vrbl_lbls))

    ## actual plotting 
    ggplot(dfx, aes(y=get(ystack))) + 
        geom_errorbarh(aes(xmin = min, xmax = max, height= 0.2, linetype = factor(sig), size = factor(sig)),
                       alpha = 0.8, show.legend = T)  +
        geom_point(point_aes, size = 2.5,  show.legend = T) +
        facet_grid(vrbl_name_unlag ~ get(xstack), switch = "y", 
                   labeller = labeller(vrbl_name_unlag = rev(vvs$vrbl_lbls))) +
        theme(strip.text.y.left = element_text(angle = 0),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_linetype_manual(values = c(2, 1)) + # setting errorbar linetype
        scale_size_manual(values=c(0.4, 0.7)) + ## setting errorbar size
        theme(panel.spacing.y = unit(0.1, "lines"),
              legend.position = "bottom",
              legend.justification = "left",
              panel.background = element_rect(fill = NA, color = "black")) +
        guides(shape = guide_legend(order=1, label.position = "right", direction = "vertical"),
               color = guide_colorbar(order=2),
               size = "none", ## remove the supid sig() legend
               linetype = "none") +
        labs(y=ystack)
    
}

## *** optimized functions end here 

reg_anls_base_optmz <- read_reg_res_files(fldr_info_cvrg)
reg_anls_base_optmz <- read_reg_res_files(fldr_info_optmz)

## best result per base_spec after each loop of optimization
## uglyl af -> uncomment
## filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>%
##     group_by(loop_nbr, base_lag_spec, cbn_name) %>%
##     slice_max(gof_value) %>% 
##     ggplot(aes(x=loop_nbr, y=gof_value, group = interaction(base_lag_spec, cbn_name))) +
##     geom_line() +
##     facet_wrap(~cbn_name, ncol = 1, scales = "free_y")





## comparison 
## filter(reg_anls_base$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()
## filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% pull(gof_value) %>% max()










## see if different starting coefs of same vrbl_choice lead to same results
best_mdls_optmzd <- filter(reg_anls_base_optmz$gof_df_cbn, gof_names == "log_likelihood") %>% 
    ## select(mdl_id, gof_value, base_lag_spec, loop_nbr, vrbl_optmzd, cbn_name) %>%
    select(mdl_id, gof_value, base_lag_spec, loop_nbr, vrbl_optmzd, cbn_name, technique_str, difficulty, regcmd) %>%
    mutate(step_base = 1, loop_nbr = as.numeric(loop_nbr),
           vrbl_choice = gsub("[1-5]", "0", base_lag_spec)) %>%
    ## group_by(cbn_name, vrbl_choice, base_lag_spec) %>%
    group_by(cbn_name, vrbl_choice, base_lag_spec, technique_str, difficulty, regcmd) %>%
    slice_max(gof_value, n=1) %>% 
    slice_sample(n=1)





## reg_anls_base_optmz$gof_df_cbn$base_lag_spec %>% unique()



df_anls_base_optmzd <- add_coef_sig(reg_anls_base_optmz$coef_df, reg_anls_base_optmz$df_reg_anls_cfgs_wide)
## construct_df_best_mdls(reg_anls_base_optmz, reg_anls_base_optmz$gof_df_cbn)

best_mdls_optmzd_coefs <- merge(df_anls_base_optmzd, best_mdls_optmzd) %>% atb() %>%
    mutate(min = coef - 1.96*se, max = coef + 1.96*se) %>% 
    filter(vrbl_name_unlag %!in% c("ln_s", "cons", "ln_r", "alpha", "intcpt_var", "(Intercept)"))
    

## condense the ystack to be able to save vertical space when expanding the xstack
best_mdls_optmzd_coefs <- best_mdls_optmzd_coefs %>%
    group_by(cbn_name, vrbl_name_unlag, vrbl_choice) %>%
    mutate(vrbl_choice_factor = row_number(), ## when using xstack=vrbl_choice
           just_one = factor(1), ## when using xstac=base_lag_spec
           tec_base_interact = interaction(technique_str, base_lag_spec), ## spread out the multiple versions 
           tec_diffl_interact = interaction(technique_str, difficulty))

## most straightforward way to see if different variable choices lead to different coefs/lags
## coefs/lags are pretty much the same, but handful of marginally significant coefs :/
plot_stacker(best_mdls_optmzd_coefs, ystack = "base_lag_spec", xstack = "cbn_name",
             shape_clm = "vrbl_choice", color_clm = "lag")

## want those from different combinations on top of each other: compare 
plot_stacker(best_mdls_optmzd_coefs, xstack = "vrbl_choice", ystack = "cbn_name",
             shape_clm = "cbn_name", color_clm = "lag")


## even more effective way to show that reg_specs with same variable choice converge to same results
## these 2 only make sense for having only one combination: would need more detailed ystacking
plot_stacker(best_mdls_optmzd_coefs, ystack = "vrbl_choice_factor", xstack = "vrbl_choice",
             shape_clm = "cbn_name", color_clm = "lag")


## similar to first coef visualization (one model per column)
plot_stacker(best_mdls_optmzd_coefs, ystack = "just_one", xstack = "base_lag_spec",
             shape_clm = "vrbl_choice", color_clm = "lag")


## **** convergence tests
## compare difficulty within technique strs 
plot_stacker(best_mdls_optmzd_coefs, xstack = "technique_str", ystack = "cbn_name",
             shape_clm = "cbn_name", color_clm = "lag")

## compare techniques within difficulty 
plot_stacker(best_mdls_optmzd_coefs, ystack = "technique_str", xstack = "difficulty",
             shape_clm = "technique_str", color_clm = "lag")

## compare technique within difficulty, with different runs unstacked
plot_stacker(best_mdls_optmzd_coefs, ystack = "tec_base_interact", xstack = "difficulty",
             shape_clm = "technique_str", color_clm = "lag")

## compare difficulty within tec_base_interact
plot_stacker(best_mdls_optmzd_coefs, xstack = "tec_base_interact", ystack = "cbn_name",
             shape_clm = "technique_str", color_clm = "lag")

plot_stacker(best_mdls_optmzd_coefs, xstack = "cbn_name", ystack = "regcmd",
             shape_clm = "vrbl_choice", color_clm = "lag")




## "formal" test: all gof_values of best-fitting models are the same 
best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str, regcmd) %>%
    slice_max(gof_value) %>%
    select(vrbl_name_unlag, difficulty, technique_str, gof_value) %>%
    ## pull(technique_strs) %>%
    pull(gof_value) %>%
    n_distinct()



## **** non-identical convergence



## see how coefs/lags differ between different base_lag_specs
base_lag_spec_cprn_df <- best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str) %>%
    select(vrbl_name_unlag, difficulty, technique_str, coef, gof_value, base_lag_spec, lag) %>%
    mutate(base_lag_spec_fctr = as.numeric(as.factor(base_lag_spec))) %>% 
    pivot_wider(id_cols = c(vrbl_name_unlag, difficulty, technique_str), names_from = base_lag_spec_fctr,
                values_from = c(coef, gof_value, lag)) %>% 
    mutate(diff_coef = coef_1 - coef_2,
           diff_lag = lag_1 - lag_2) 

base_lag_spec_cprn_df %>% 
    ggplot(aes(x=diff_coef)) +
    ## ggplot(aes(x=diff_lag)) +
    geom_histogram(bins = 100)

base_lag_spec_cprn_df %>%
    filter(diff_coef !=0) %>%
    ## pull(vrbl_name_unlag) %>%
    ## pull(technique_str) %>%
    pull(difficulty) %>%
    table()


## pull(diff) %>% table()
## pull(diff) %>% hist(breaks = 50)
## huh this looks like quite some different values
## well they don't look so different when you plot them, also in partly due to small differences being dwarved by the few huge coefs 

## compare base_lag_spec within tec_diffl_interact
plot_stacker(best_mdls_optmzd_coefs, xstack = "tec_diffl_interact", ystack = "base_lag_spec",
             shape_clm = "base_lag_spec", color_clm = "lag")


## should also do lag test comparison, there seems to be some difference
filter(base_lag_spec_cprn_df, diff_lag != 0) %>%
    select(vrbl_name_unlag, difficulty, technique_str, lag_1, lag_2) %>% 
    ## pull(vrbl_name_unlag) %>%
    ## pull(technique_str) %>%
    pull(difficulty) %>%
    table()


## difference between techniques: which is closest to nr

base_lag_spec_cprn_df2 <- best_mdls_optmzd_coefs %>%
    group_by(vrbl_name_unlag, difficulty, technique_str) %>%
    select(vrbl_name_unlag, difficulty, technique_str, coef, gof_value, base_lag_spec, lag)


merge(filter(base_lag_spec_cprn_df2, technique_str == "nr") %>%
      select(vrbl_name_unlag, difficulty, base_lag_spec, technique_str_nr = technique_str, coef_nr = coef),
      filter(base_lag_spec_cprn_df2, technique_str != "nr")) %>% atb() %>%
    mutate(coef_diff = abs(coef_nr - coef)) %>%
    ggplot(aes(x=coef_diff)) +
    geom_histogram() +
    facet_wrap(~technique_str)
    ## group_by(technique_str) %>% 
    ## summarize(coef_diff_mean = mean(coef_diff))

 

## **** lag test

filter(df_anls_base_optmzd, vrbl_name_unlag %in% c("ti_tmitr_interact", "tmitr_approx_linear20step")) %>%
    select(mdl_id, vrbl_name_unlag, lag, base_lag_spec) %>%
    pivot_wider(names_from = vrbl_name_unlag, values_from = lag) %>%
    mutate(lag_same = ti_tmitr_interact == tmitr_approx_linear20step) %>%
    ## head(100) %>% adf()
    pull(lag_same) %>% table()
    ## arrange(base_lag_spec) %>% 
    ## filter(!lag_same) %>% adf()


filter(df_anls_base_optmzd, vrbl_name_unlag %in% c("ti_tmitr_interact", "tmitr_approx_linear20step")) %>%
    select(mdl_id, vrbl_name_unlag, lag, base_lag_spec) %>%
    ## filter(vrbl_name_unlag == "tmitr_approx_linear20step") %>%
    ## filter(vrbl_name_unlag == "ti_tmitr_interact") %>%
    group_by(base_lag_spec) %>%
    summarize(distinct_lags = n_distinct(lag))

## squared test
## dfx <- tibble(a = rnorm(1000), b=rnorm(1000))
## t1 <- lm(a ~ b, dfx)
## t2 <- lm(a ~ b + I(b^2), dfx)
## screenreg(list(t1, t2))




## doesn't run: vrbl_varied: is not provided, means that within-changes don't make sense
## hopefully can stitch together other funcs tho 
## reg_res_objs <- proc_reg_res_objs(reg_anls_base, vvs)




## ** within base-spec changes




## LAZILY just copying 
## df_anls_within_prep2 <- df_anls_within_prep

## unique(df_anls_within$vrbl_name_unlag)

## just rbind the time-invariant values there 





## shouldn't group by base_lag_spec when selecting
## see if some aux vars can be constructed to select on 



        
## df_anls_within_ribbon

## df_anls_within %>% group_by(cbn_name, vrbl_name_unlag, lag) %>%
##     summarize(coef_mean = mean(coef), sd = sd(coef),
##               t_value_mean = mean(t_value)) %>%
##     mutate(coef_min = coef_mean - 1.96*sd, coef_max = coef_mean + 1.96*sd) %>%
##     ggplot(aes(x = lag, y=coef_mean)) +
##     geom_line(aes(color = t_value_mean)) +
##     geom_ribbon(aes(ymin = coef_min, ymax = coef_max), alpha = 0.3) + 
##     facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y") +
##     scale_color_gradient2(low = "blue", mid = "grey", high = "red") 
        


## ** coefs from all models


    ## c(ti_vars, density_vars, hnwi_vars, inc_ineq_vars, weal_ineq_vars,
    ##   cult_spending_vars, ctrl_vars_lngtd))



## ** best fitting models




## *** condensed

generate_plot_models <- function(cbn_namex) {
    #' generate texreg models of best models?

    mdl_summary <- best_mdl_coefs %>% group_by(vrbl_name_unlag) %>%
        filter(cbn_name == cbn_namex) %>% 
        summarize(coef = mean(coef), lag_mean = mean(lag), lag_sd = sd(lag), p_value = mean(pvalues),
                  t_value = mean(t_value), se = mean(se))
    ## reg_res <- lm(mpg ~ cyl + disp, mtcars)
    ## plotreg(reg_res)

    texreg_mdl <- createTexreg(coef.names = as.character(mdl_summary$vrbl_name_unlag),
                      coef = mdl_summary$coef,
                      se = mdl_summary$se,
                      pvalues = mdl_summary$p_value)

    return(texreg_mdl)
}

x <- lapply(names(cbn_dfs)[1:3], generate_plot_models)

y <- plotreg(x, type = "facet")

## *** manual plotreg





    



createTexreg(coef.names = mdl_summary$vrbl_name_unlag, coef = )


## *** LL lines

## can just merge
## maybe don't even need: can use best_mdl_coefs?
## nah doesn't have all the lag information anymore 


mdl_fit_df <- merge(df_anls_within,
                   filter(gof_df_cbn, gof_names == "log_likelihood")) %>% 
    group_by(cbn_name, vrbl_name_unlag, base_lag_spec) %>%
    mutate(gof_value = gof_value - min(gof_value)) %>%
    group_by(cbn_name, vrbl_name_unlag, lag) %>%
    summarize(gof_value = mean(gof_value), base_lag_spec = 1)


pdf(paste0(FIG_DIR, "mdl_fit_plot.pdf"), height=10, width = 8)
mdl_fit_df %>% 
    ggplot(aes(x=lag, y=gof_value, group = base_lag_spec)) +
    geom_line(show.legend = F) + 
    geom_point() + 
    facet_grid(vrbl_name_unlag ~ cbn_name, scales = "free", switch = "y", 
               labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0))
dev.off()

## *** two-axes plot

    
    



## combine data to be plotted 
two_axis_df <- rbind(
    best_mdl_coefs %>% select(cbn_name, vrbl_name_unlag, lag, vlu = coef, base_lag_spec) %>%
    mutate(source = "best_coefs"),
    mdl_fit_df %>% select(cbn_name, vrbl_name_unlag, lag, vlu = gof_value, base_lag_spec) %>%
    mutate(source = "ll_lines")) %>% atb()


## get the scales for the best_coef plots
## actually get them from within_anls plot
ll_scale <- df_anls_within %>%
    filter(cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>%
    summarize(min_vlu = min(coef), max_vlu = max(coef), range = max_vlu - min_vlu,
              source = "best_coefs") %>%
    select(cbn_name, vrbl_name_unlag, source, min_vlu, max_vlu, range)

coef_scale <- filter(two_axis_df, cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>%
    group_by(source, cbn_name, vrbl_name_unlag) %>%
    summarize(min_vlu = min(vlu), max_vlu = max(vlu), range = max_vlu - min_vlu) %>%
    filter(source == "ll_lines")


test_scale <- rbind(ll_scale, coef_scale)

## need to scale the same variables consistently -> calculate them outside of dplyr
scaler <- filter(test_scale, source == "ll_lines")$range/filter(test_scale, source == "best_coefs")$range

## now scaling best_coefs to ll_lines 
## the range of best_coefs should now be the range of ll_lines

test_scale2 <- test_scale %>%
    mutate(min_vlu = ifelse(source == "best_coefs", min_vlu * scaler, min_vlu),
           max_vlu = ifelse(source == "best_coefs", max_vlu * scaler, max_vlu))
## ranges (not the value, but the actual range between min and max values) are the same now


offset_vlu <- filter(test_scale2, source == "ll_lines")$max_vlu - filter(test_scale2, source == "best_coefs")$max_vlu 

## don't need to readjust values in test_scale, now I have scaler and offset value -> can adjust actual values
## should tho to check errors
test_scale3 <- test_scale2 %>%
    mutate(min_vlu = ifelse(source == "best_coefs", min_vlu + offset_vlu, min_vlu),
           max_vlu = ifelse(source == "best_coefs", max_vlu + offset_vlu, max_vlu))
           
## hmm seems to work, could ofc be that largest value is not the best fitting one?
## could also be due to negative numbers?
## maybe complete line is more helpful? 


filter(two_axis_df, cbn_name == "cbn_all", vrbl_name_unlag == "sptinc992j_p90p100") %>% 
    mutate(vlu = ifelse(source == "best_coefs", offset_vlu + (vlu * scaler), vlu)) %>% 
    ggplot(aes(x=lag, y=vlu, color = source)) +
    geom_point() +
    scale_y_continuous(name = "ll_lines", sec.axis = sec_axis(~ (.- offset_vlu) /scaler , name = "best_coefs"))
    
## doesn't align completely, but could be correct:
## my overall min from ll_scale is -0.525, but my lowest best_coef is only -0.38
## -0.38 * scaler + offset_vlu = 0.43, which looks correct
## -0.525 * scaler + offset_vlu = 0 -> also correct
## -> so points get scaled correctly, but second axis still wrong
## maybe the sign change?
## doesn't seem like it, but y=ax+b -> x=(y-b)/a

## can make line out of LL if i really want: just pass differently filtered data to geom_point and geom_line

gen_ll_best_coef_plot <- function(vrbl_name_unlag, cbn_name) {
    #' generate individual plot with information on fit (LL) and coef values at best fits
    #' needs as globals: df_anls_within, two_axis_df
    
    
    ll_scale <- df_anls_within %>%
        filter(cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>%
        summarize(min_vlu = min(coef), max_vlu = max(coef), range = max_vlu - min_vlu,
                  source = "best_coefs") %>%
        select(cbn_name, vrbl_name_unlag, source, min_vlu, max_vlu, range)

    ## get the scale information of the best_coefs, actually use full coefs for illustration/maybe adding coef line
    coef_scale <- filter(two_axis_df, cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>%
        group_by(source, cbn_name, vrbl_name_unlag) %>%
        summarize(min_vlu = min(vlu), max_vlu = max(vlu), range = max_vlu - min_vlu) %>%
        filter(source == "ll_lines")

    ## combine scaling information 
    scale_df1 <- rbind(ll_scale, coef_scale)

    ## generate scaler to stretch best_coef range to ll_lines range
    scaler <- filter(scale_df1, source == "ll_lines")$range/filter(scale_df1, source == "best_coefs")$range

    ## adjust ranges, needed to generate offset
    scale_df2 <- scale_df1 %>%
        mutate(min_vlu = ifelse(source == "best_coefs", min_vlu * scaler, min_vlu),
               max_vlu = ifelse(source == "best_coefs", max_vlu * scaler, max_vlu))

    ## generate offset
    offset_vlu <- filter(scale_df2, source == "ll_lines")$max_vlu - filter(scale_df2, source == "best_coefs")$max_vlu 

    filter(two_axis_df, cbn_name == !!cbn_name, vrbl_name_unlag == !!vrbl_name_unlag) %>% 
        mutate(vlu = ifelse(source == "best_coefs", offset_vlu + (vlu * scaler), vlu)) %>% 
        ggplot(aes(x=lag, y=vlu, color = source)) +
        geom_point(show.legend = F) +
        scale_y_continuous(name = "", sec.axis = sec_axis(~ (.- offset_vlu) /scaler , name = "")) +
        labs(x="", y="")

}


facets_to_plot <- df_anls_within %>% select(cbn_name, vrbl_name_unlag) %>% unique() %>% arrange(cbn_name, vrbl_name_unlag)

ll_coef_plots <- apply(facets_to_plot[1:6,], 1, \(x) gen_ll_best_coef_plot(x[["vrbl_name_unlag"]], x[["cbn_name"]]))

plot_grid(plotlist = ll_coef_plots[1:6], ncol = 1)

grid.arrange(ll_coef_plots)

grid.arrange(grobs = ll_coef_plots)



library(cowplot)
library(patchwork)

(ll_coef_plots[[1]] + ll_coef_plots[[2]]) / (ll_coef_plots[[3]] + ll_coef_plots[[4]])

wrap_plots(ll_coef_plots) +
    plot_layout(ncol = 2, widths = c(0,2))

lapply(ll_coef_plots, ggplotGrob) %>% rbind() %>% grid.draw()


gen_ll_best_coef_plot("shweal992j_p90p100", "cbn_all")

## ** between-within coef variation

get_between_within_sds <- function(vlu_vec, id_vec) {
    #' generate the between and within variation for value vector given id vector 
    
    dfx = data_frame(vlu = vlu_vec,  id = id_vec)
    xtsum_res <- xtsum(dfx, vlu, id)

    sd_within <- xtsum_res$sd[3]
    sd_between <- xtsum_res$sd[2]

    return(data_frame(sd_within = sd_within, sd_between = sd_between))
}


variation_anls_prep <- df_anls_within %>% group_by(cbn_name, vrbl_name_unlag) %>%
    do(get_between_within_sds(.$coef, .$base_lag_spec))

    
variation_anls <- variation_anls_prep %>% pivot_longer(cols = c(sd_within, sd_between)) %>%
    mutate(cbn_name = factor(cbn_name, levels = rev(names(cbn_dfs))))


pdf(paste0(FIG_DIR, "coef_variation_anls.pdf"), width = 8, height = 9)
variation_anls %>% 
    ggplot(aes(x=value, y=cbn_name , group = name, color = name)) +
    geom_path() +
    geom_point() + 
    facet_wrap(~vrbl_name_unlag, switch = "y", ncol = 1, scales = "free_y",
               labeller = labeller(vrbl_name_unlag = rel_vars)) +
    theme(strip.text.y.left = element_text(angle = 0))
dev.off()


    


## ** inequality test

vrbl_sptinc <- "sptinc992j_p90p100_lag0"
vrbl_shweal <- "shweal992j_p90p100_lag0"

cbn_dfs_counts_uscld$cbn1 %>% adt %>%
    .[, .(mean_sptinc = mean(get(vrbl_sptinc)),
          mean_shweal = mean(get(vrbl_shweal))), iso3c] %$% cor.test(mean_sptinc, mean_shweal)

    ggplot(aes(x=mean_sptinc, y=mean_shweal)) + geom_point()



library(MASS)
sigma <- rbind(c( 1.00 , 0.1, -0.2),
               c( 0.10 , 1.0,  0.8),
               c(-0.2  , 0.8,  1.00))
mu <- c(0,0,0)

dt_madeup <- mvrnorm(n=500, mu = mu, Sigma = sigma) %>% adt()

library(GGally)
ggpairs(dt_madeup)

m1 <- lm(V1 ~ V2, dt_madeup)
m2 <- lm(V1 ~ V3, dt_madeup)
m3 <- lm(V1 ~ V2 + V3, dt_madeup)

screenreg(list(m1,m2,m3))

library(texreg)

library(performance)

check_collinearity(m3) %>% plot

dt_pred <- data.table(V2 = c(2, 2, 0), V3 = c(0,2, 0))

dt_pred$V1 <- predict(m3, dt_pred)

lm(V2 ~ V3, dt_madeup) %>% summary

scale(dt_madeup) %>% eigen
svd(dt_madeup)


top_coefs2 <- reg_res_objs$top_coefs
dt_oucoefchng2 <- reg_res_objs$dt_oucoefchng

reg_res_objs$ou_anls
## get ineq coefs of OU models (don't need original coefs)
dt_coefs_ou <- reg_anls_base$ou_objs$coef_df %>% adt %>% .[grepl("ptinc992j|hweal992j", vrbl_name)] 

## get mdl_ids of OU models that vary ineq vrbls
dt_mdls_ou_ineq <- reg_anls_base$ou_objs$df_reg_anls_cfgs_wide %>% adt %>%
    .[grepl("ptinc992j|hweal992j", ou_set_title)] %>%
    .[, .(mdl_id, nonou_id, ou_set_title, cbn_name)]

dt_coefs_ou_ineq2 <- join(dt_mdls_ou_ineq, dt_coefs_ou, on = "mdl_id", how = "left")%>%
    .[, ou_set_title_unlag := gsub("_lag[0-5]", "",  ou_set_title)] %>%
    .[, vrbl_name := gsub("_lag[0-5]", "",  vrbl_name)]
    
    ## .[, .(coef = mean(coef)), .(ou_set_title_unlag, cbn_name, vrbl_name)]

## coef distribution
dt_coefs_ou_ineq2 %>%     
    ggplot(aes(x=coef, y= vrbl_name)) + 
    geom_point() +
    geom_errorbarh(mapping = aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), height = 0.5,
                   ## position = position_jitter(width = 0, height = 0.4, seed = 3)) +
                   position = position_dodge2(width = 2)) + 
    facet_grid(~cbn_name) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_label(dt_coefs_ou_ineq2[, .(mean_p = mean(pvalues)), .(cbn_name, vrbl_name)],
              mapping = aes(x=0.5, y=vrbl_name, label = round(mean_p,2)))
              

    
## average p-value

    
    
