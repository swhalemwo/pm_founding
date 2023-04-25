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


