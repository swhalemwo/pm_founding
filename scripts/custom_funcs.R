## * custom funcs

table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

'%!in%' <- function(x,y)!('%in%'(x,y))
len <- length
adf <- as.data.frame
atb <- as_tibble
adt <- as.data.table

## https://stats.stackexchange.com/questions/123366/lmer-standardized-regression-coefficients
lm.beta.lmer <- function(mod) {
    ## extract standardized effects
   b <- fixef(mod)[-1]
   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
   sd.y <- sd(getME(mod,"y"))
   b*sd.x/sd.y
}


minus <- function(a,b){return(a-b)}
plus <- function(a,b){return(a+b)}

rollmean_custom <- function(v, win_len, func, orientation = "left") {
    # computes rolling mean without removing observations by using smaller windows at the sides
    c <- 1
    ## slider needs orientation if length of sliding window is even:
    ## question is whether to orient oneself to the right of left
    len_v <- length(v)

    all_res <- c()
    
    for (i in v){
        ## select indices
        ## just assume left, win_len=2
        ## indices <- c(c, c-1)
        indices <- c(c)

        ## only add more indices if win_len > 1
        
        if (win_len>1) {
            ## initializing the operation to perform based on orientation
            if (orientation == "left"){
                op <- minus
                op_name <- "minus"
            } else {
                op <- plus
                op_name <- "plus"
            }
            for (k in seq(win_len-1)){
                ## need -1 because first value c is already in indices
                last_pos <- tail(indices,1)
                
                indices <- c(indices, op(last_pos,k))

                ## switching the operation
                if (op_name == "minus") {
                    op <- plus
                    op_name <- "plus"
                } else {
                    op <- minus
                    op_name <- "minus"
                }
            }
        }
        
        indices_sel <- indices[intersect(which(indices>=1), which(indices <= len_v))]

        ## weighted mean (normal distribution), but makes opening_count more jagged:
        ## original series is jagged, weighted means give higher impact to original series -> more jagged :(
        
        ## weights = pnorm(indices_sel, c, win_len/3)
        ## indices_sel <- sort(indices_sel)


        ## print(paste0("indices_sel: ", indices_sel))
        ## print(paste0("c: ", c))
        ## print(paste0("weights: ", weights))
        ## print("----")
        ## win_res <- weighted.mean(values, weights)        
        
        
        values <- v[c(indices_sel)]

        win_res <- mean(values)

        
        all_res <- c(all_res, win_res)
        c <- c+1
    }
    return(all_res)
}

## rollmean_custom(x, win_len = 3, orientation = "left")

## x <- seq(1,100)/10
## x1 <- sin(x)

## x2 <- sin(seq(1,100))
## ## x2 <- rnorm(100, 0,1)
## ## plot(x2)

## plot(x1+x2)

## lines(rollmean_custom(x1+x2, win_len = 30, orientation = "left"))
## lines(rollmean_custom(x1+x2, win_len = 10, orientation = "left"))
## lines(rollmean_custom(x1+x2, win_len = 15, orientation = "left"))
## lines(rollmean_custom(x1+x2, win_len = 5, orientation = "left"))
## lines(rollmean_custom(x1+x2, win_len = 2, orientation = "left"))
## lines(rollmean_custom(x1+x2, win_len = 3, orientation = "left"))





## ** add lagged values

## overly messy way of lagging variables that creates intermediary vars because mutate/lag doesn't accept variablies as input

lagger <- function(dfx, vrbls_to_lag){
    for (varx in vrbls_to_lag){
        lag_name = paste(varx, "_lag1", sep = "")
        ## eval(parse("lag_name"))
        ## df_anls$var_to_lag <- df_anls[,c(varx)]
        ## df_anls[,"var_lagged"] <- mutate(group_by(df_anls, countrycode), var_lagged = lag(var_to_lag))[,"var_lagged"]
        ## df_anls[,lag_name] <- df_anls$var_lagged
        ## df_anls <- df_anls[,-which(names(df_anls) %in% c("var_to_lag", "var_lagged"))]

        dfx[,"var_to_lag"] <- dfx[,c(varx)]
        dfx[,"var_lagged"] <- mutate(group_by(dfx, iso3c), var_lagged = lag(var_to_lag))[,"var_lagged"]
        dfx[,lag_name] <- dfx[,"var_lagged"]

        dfx <- dfx[,-which(names(dfx) %in% c("var_to_lag", "var_lagged"))]
    }
    return(dfx)
}

## vrbls_to_lag <- c("gdp_pcap", "gdp_pcapk", "gini", "nbr_opened")
## vrbls_to_lag <- c("NY.GDP.PCAP.CD", "nbr_opened")

## df_anls2 <- lagger(df_anls, vrbls_to_lag)

## filter(df_anls2[,c("iso3c", "year", "nbr_opened", "nbr_opened_lag1", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.CD_lag1")], iso3c == "USA")

cpltns_checker <- function(vx, varx) {
    #' assesses completeness of variable in terms of df_anls PM coverage 
    # there's still a bug with cry_cvrg_geq3, which can be higher than 217 sometimes 

    vx <- as_tibble(na.omit(vx[,c("iso3c", "year", varx)]))


    dfb <- df_anls[,c("iso3c", "year", "nbr_opened")]
    dfc <- as_tibble(merge(dfb, vx, by = c("year", "iso3c"), all.x = TRUE))


    cry_cvrg <- aggregate(year ~ iso3c, na.omit(dfc), length) ## how many observation periods
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$iso3c ## countries with at least 3 obs
    cry_pm_crvg_actual <- aggregate(nbr_opened ~ iso3c, na.omit(dfc), sum) ## number of PMs per country with data
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ iso3c, dfc, sum) ## number of PMs that would be covered ideally
    names(cry_pm_crvg_ideal) <- c("iso3c", "nbr_opened_ideal")

    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ratio_opngs_cvrd <- sum(cry_pm_cvrg_cprn$nbr_opened)/sum(cry_pm_cvrg_cprn$nbr_opened_ideal)
    ## percentage of PMs covered overall
    ratio_opngs_cvrd_cry_avg <- mean(cry_pm_cvrg_cprn$nbr_opened/cry_pm_cvrg_cprn$nbr_opened_ideal, na.rm = T)
    ## average country percentage of coverage of openings

    ## most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
    ##                                     function(x) (filter(cry_pm_cvrg_cprn, diff == x)$iso3c)))

    most_affected_crys <- cry_pm_cvrg_cprn$iso3c[order(cry_pm_cvrg_cprn$diff)[1:4]]
    

    PMs_covered_raw <- sum(na.omit(dfc)$nbr_opened)

    cry_cvrg_geq3 <- sum(filter(na.omit(dfc), iso3c %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- aggregate(nbr_opened ~ iso3c, dfc, sum) %>%
        filter(iso3c %in% crys_geq3 & nbr_opened >= 1) %>%
        nrow()
    ## some nicer syntax

        return(list(
        varx=varx,
        nobs=nrow(vx),
        ratio_opngs_cvrd=ratio_opngs_cvrd,
        ratio_opngs_cvrd_cry_avg=ratio_opngs_cvrd_cry_avg,
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
 
}

    
scramblematch<-function(query,target, ignore.case=T) {
    ## print(sprintf("query: %s", query))
    ## print(sprintf("target: %s", target))
    ## print("\n")
    #' returns true if query is substring of target, by default ignore case
    ## Reduce("&",lapply(strsplit(query," ")[[1]],grepl,target,fixed=TRUE))
    Reduce("&",lapply(strsplit(query," ")[[1]], grepl, target, ignore.case = ignore.case))
}



findt <- scramblematch

findt_v <- Vectorize(findt)


locate_col <- function(df, term) {
    #' find the column labeled as term (if data is not clearly structured)
    df_loc <- which(apply(df, 2, function(x) grepl(term, x)))
    if (len(df_loc) > 1) {
        stop("more than 1 colname found")
    }
    
    row_nbr <- df_loc %% nrow(df) ## modulus to find row
    col_nbr <- ceiling(df_loc/nrow(df)) ## find proper column
    

    return(list(
        col_nbr=col_nbr,
        row_nbr=row_nbr)
        )
}



xtsum <- function(data, varname, unit) {
    ## Xtsum
    varname <- enquo(varname)
    loc.unit <- enquo(unit)
    ores <- data %>% summarise(Mean=mean(!! varname, na.rm=TRUE), sd=sd(!! varname, na.rm=TRUE), min = min(!! varname, na.rm=TRUE), max=max(!! varname, na.rm=TRUE), N=sum(as.numeric((!is.na(!! varname)))))
    bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname)))) 
    bres <- bmeans %>% ungroup() %>% summarise(sd = sd(meanx, na.rm=TRUE), min = min(meanx, na.rm=TRUE), max=max(meanx, na.rm=TRUE), n=sum(as.numeric(!is.na(t.count))), `T-bar`=mean(t.count, na.rm=TRUE))
    wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, center=TRUE, scale=FALSE))
    wres <- wdat %>% ungroup() %>% summarise(sd=sd(W.x, na.rm=TRUE), min=min(W.x, na.rm=TRUE), max=max(W.x, na.rm=TRUE))
    ## Loop to adjust the scales within group outputs against the overall mean
    for(i in 2:3) {
        wres[i] <- sum(ores[1], wres[i])
    }

    ## Table Output
    Variable <- matrix(c(varname, "", ""), ncol=1)
    Comparison <- matrix(c("Overall", "Between", "Within"), ncol=1)
    Mean <- matrix(c(ores[1], "", ""), ncol=1)
    Observations <- matrix(c(paste0("N = ", ores[5]), paste0("n = ", bres[4]), paste0("T-bar = ", round(bres[5], 4))), ncol=1)
    Tab <- rbind(ores[2:4], bres[1:3], wres[1:3])
    Tab <- cbind(Tab, Observations)
    Tab <- cbind(Mean, Tab)
    Tab <- cbind(Comparison, Tab)
    Tab <- cbind(Variable, Tab)

    ## Output
    return(Tab)
}



## xtsum(df_scl, nbr_opened, iso3c)

edit_plotreg <- function(reg_plt) {

    #' set the x-axes free and add 4 columsn to  plot_reg result
    #' for some reason is able to modify the original object

    ## reg_plt_dis: reg plt disassembled
    ## plot code has to be run twice for some reason
    ## also modifies the original plot reg_plt object??
    reg_plt_dis <- ggplot_build(reg_plt)
    reg_plt_dis$layout$facet$params$free$x <- T
    reg_plt_dis$layout$facet$params$ncol <- 4
    reg_plt_dis$layout$facet$params$nrow <- NULL
    reg_plt_edt <- ggplot_gtable(reg_plt_dis)
                                        # reg_plt_dis: reg plt disassembled
    reg_plt_dis <- ggplot_build(reg_plt)
    reg_plt_dis$layout$facet$params$free$x <- T
    reg_plt_dis$layout$facet$params$ncol <- 4
    reg_plt_dis$layout$facet$params$nrow <- NULL
    reg_plt_edt <- ggplot_gtable(reg_plt_dis)
    ## plot(reg_plt_edt)

    return(reg_plt)
}


scale_wo_attr <- function(x) {
    #' scale vector without returning attributes
    scld <- scale(x)
    attributes(scld) <- NULL
    return(scld)
}


print_data_table <- function(x, ...) {
  # Adapted from data.table:::as.data.frame.data.table()
    ans <- x
    attr(ans, "row.names") <- .set_row_names(nrow(x))
    attr(ans, "class") <- c("tbl", "data.frame")
    attr(ans, "sorted") <- NULL
    attr(ans, ".internal.selfref") <- NULL
    print(ans, ...)
  invisible(x)
}



## paint::mask_print()

## print.data.table <- paint
print.data.table <- print_data_table


## have imap_dfr return data tables instead of tibbles
## imap_dfr_bu <- imap_dfr # assign original to backup (bu) function
## imap_dfr <- function(...) {adt(imap_dfr_bu(...))}
imap_dfr <- function(...) {adt(purrr::imap_dfr(...))}

## l <- list(x=list(a=1, b=2), y=list(a=3, b=4))
## imap_dfr(l, ~list(jj = .y, kk = .x$a))

## paint::unmask_print()

ctstrsplit <- function(x, ...) {
    #' custom version of tstrsplit that works with empty strings 
    res <- tstrsplit(x, ...)
    
    ## if nothing gets returned, return original string
    if (len(res) == 0) {
        res <- x
    }        
     
    return(res)
}


view_xl <- function(.data) {
    if (interactive()) {
        tmp <- tempfile(fileext = ".csv")
        fwrite(.data, tmp)
        browseURL(tmp, browser = "gnumeric")
    }
}


check_df_name_unqns <- function(l, skip_var_names) {
    #' check whether names of dataframes in list l are overlapping (check their uniqueness)
    #' skip_names: don't consider them

    

    df_names <- unlist(sapply(l, names)) %>% table() %>% adt() %>% .[order(-N)]
    if (df_names[. %!in% skip_var_names, max(N)] != 1) {
        print(df_names[. %!in% skip_var_names & N>1])
        stop("duplicate variable names in dfs")
    } else {
        print("no duplicate variable names")
    }
    return(invisible(NULL))
}

pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}


tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")


sanitize_number <- function(nbr) {
    #' custom formatting for number abbreviations because f_denom doesn't work??

    if (nbr >= 1000 & nbr < 1e6) {
        lbl <- paste0(nbr/1000, "K")
    } else if (nbr >= 1e6 & nbr < 1e+9) {
        lbl <- paste0(nbr/1e6, "M")
    } else if (nbr >= 1e9 & nbr < 1e12) {
        lbl <- paste0(nbr/1e9, "B")
    } else {
        lbl <- paste0(nbr)
    }
    return(lbl)
}



pvlt <- function(dtx, crop = T, ...) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' preview table as latex table as pdf

    
    if ("xtable" %!in% class(dtx)) {
        xtbl <- xtable(dtx)
    } else {
        xtbl <- dtx
    }


    ## ps() %>% adt() %>% .[grepl("zathura", name)]
    ## tmp <- tempfile(fileext = ".tex")
    
    tmp_dir <- "/tmp/"
    tmp_file_tex <- "pvlt_input.tex"
    
    tmp <- paste0(tmp_dir, tmp_file_tex)

    ## pvlt_list <- as.list(substitute(list(...)))
    ## pvlt_list2 <- c(pvlt_list, list(x = xtbl, file = tmp, include.rownames = F))
    ## pvlt_list2 <- list(x = xtbl, file = tmp, include.rownames = F, sanitize.text.function = identity)
    pvlt_list <- c(list(...), list(x = xtbl, file = tmp, include.rownames = F, sanitize.text.function = identity))
    
    do.call("print.xtable", pvlt_list)

    ## print.xtable(xtbl, file = tmp, include.rownames = F, ...)
        
    ## just use most barebones latex command for now
    ##  from: https://tex.stackexchange.com/questions/302788/how-to-get-the-pdf-of-a-compiled-table-only
    ## cmplcmd <- "pdflatex '\\documentclass{article}\\pagestyle{empty}\\begin{document}\\input{prvlt.tex}\\end{document}'"

    cmplcmd <- "cp /home/johannes/Dropbox/technical_stuff_general/dotfiles/texput.tex /tmp && pdflatex texput.tex"

    cmplcmd2 <- paste0("cd /tmp && ", cmplcmd)
        
    system(cmplcmd2)
    
    ## crop command pdfcrop needed to focus on table
    if (crop) {
        crop_cmd <- "cd /tmp && pdfcrop texput.pdf"
    } else {
        crop_cmd <- "cd /tmp && cp texput.pdf texput-crop.pdf"
    }
    system(crop_cmd)

    ## somewhat annoying check with lsof
    open_check_cmd <- "lsof -w /tmp/texput-crop.pdf"
    open_check_res <- system(open_check_cmd, intern = T)
    
    ## if an attribute is returned (rather than being null), file is not open
    open <- is.null(attr(open_check_res, "status"))

    if (!open) {
        open_cmd <- "zathura /tmp/texput-crop.pdf & "
        system(open_cmd)
    }
        
    invisible(NULL)

}

render_xtable <- function(xtable, filepath) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    #' renders xtable to pdf file 

    filename <- basename(filepath) %>% gsub(".pdf", "", .)

    file_dir <- dirname(filepath)

    tmp <- tempfile(tmpdir = file_dir, fileext = ".tex")

    tmp_filename <- basename(tmp)

    print(xtable, file = tmp, include.rownames = F)

    cmplcmd <- sprintf("pdflatex -jobname=%s '\\documentclass{article}\\pagestyle{empty}\\begin{document}\\input{%s}\\end{document}'",
                       filename, tmp_filename)

    full_cmd <- sprintf("cd %s && %s", file_dir, cmplcmd)
    system(full_cmd)

    ## cropping: just overwrite 
    filename_pdf <- paste0(filename, ".pdf")
    crop_cmd <- sprintf("cd %s && pdfcrop %s %s", file_dir, filename_pdf, filename_pdf)
    system(crop_cmd)

    ## remove temp files 
    cleanup_cmd <- sprintf("rm %s %s", tmp, paste0(file_dir, "/", filename, c(".log", ".aux"), collapse = " "))
    system(cleanup_cmd)

    
}

genxtbl_regtbl <- function(dt_coefs, dt_gofs, vrbl_lbls, mdl_lbls, wcptbl = F, vrbl_grps = NULL, grp_lbls = NULL,
                           wdth_grp = NULL, wdth_vrbl = NULL) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generate regression result table for tex based on xtable
    #' for now only single.row
    
    #' dt_coefs: dt of coefs: vrbl, mdl_name, coef, se, pvalue
    
    #' dt_gofs: dt with goodness-of-fit information:
    #' - mdl_name: the name (already assumes to be label for now)
    #' - gof_value: the value
    #' - decimal: number of decimal points
    
    #' wcptbl: compatibility with Microsoft word (convert to html, then import to LO)
    #' if T:
    #' - cells are wrapped in dollar signs $ $ to ensure math mode
    #' - no custom column types specifications allowed (use "l" instead)
    
    #' vrbl_lbls: labels for the variables; also provides variable order
    #' mdl_lbls:  labels for models
    #' vrbl_grps: optional: add some grouping of variables
    #' grp_lbls: labels for the groups
    
    #' wdth_vrbl, wdth_grp: width of variable/group columns (if available) in latex specification (e.g. "6cm")
    #' ignored if wcptl = T
    
    ## FIXME: add test that every vrbl and mdl has a label 


    ## if (!is.null(substitute(group))) {
    ##     group_vrbl <- as.character(substitute(group)) # get grouping variable 
    ##     v_agg <- c("vrbl", "mdl_name", group_vrbl) # aggregation vector 

    ## } else {
    ##     v_agg <- c("vrbl", "mdl_name")
    ## }

    ## set up vrbl/mdl to name dts
    dt_vrbl_lbls <- data.table(vrbl = factor(names(vrbl_lbls)), lbl = latexTranslate(unname(vrbl_lbls)))
    dt_mdl_lbls <- data.table(mdl = names(mdl_lbls), lbl = latexTranslate(unname(mdl_lbls)))
    dt_vrbl_grps <- data.table(vrbl = names(vrbl_grps), grp = unname(vrbl_grps))
    dt_grp_lbls <- data.table(grp = names(grp_lbls), lbl = unname(grp_lbls))

    ## format the cells 
    dt_coefs_fmtd <- dt_coefs %>% copy() %>%
        .[, .(cell_fmtd = fmt_cell(coef, se, pvalue, wcptbl)), by = .(vrbl, mdl_name)] %>%
        dcast.data.table(vrbl ~ mdl_name, value.var = "cell_fmtd") %>%
        dt_vrbl_lbls[., on = "vrbl"] %>%
        .[, vrbl := factor(vrbl, levels = names(vrbl_lbls))] %>% # reordering variables necessaryh somehow
        .[order(vrbl)]
    ## %>% .[, vrbl := NULL] ## yeet variable column 
    ## print(names(dt_coefs_fmtd))

    dt_gofs_fmtd <- dt_gofs %>% copy() %>%
        .[, .(gof_vlu_fmt = format(round(gof_value, decimal), nsmall = decimal)), by = .(mdl_name, gof_name)] %>%
        dcast.data.table((lbl = gof_name) ~ mdl_name, value.var = "gof_vlu_fmt") %>%
        cbind(vrbl = "")
    
    dt_coefs_fmtd <- rbind(dt_coefs_fmtd, dt_gofs_fmtd)
    

    ## add the group column if required
    if (!is.null(vrbl_grps)) dt_coefs_fmtd <- cbind(grp = "", dt_coefs_fmtd)

    ## generate the variable add.to.row components
    ## generate the vector of model names; yeet vrbl clmn "temporarily" to simulate table after yeeted for good
    chr_mdl_names <- map_chr(names(copy(dt_coefs_fmtd)[, vrbl := NULL]),
                  ~if(.x %in% names(mdl_lbls)) {
                       sprintf("\\multicolumn{1}{c}{%s}", chuck(mdl_lbls, .x))
                   } else { " "}) %>% paste0(collapse = " & ") %>% # collapse column names to string
        paste0("\\hline \n", . ," \\\\ \n") # add hline/linebreak at start, linebreak at end

    ## generate sign note
    sig_note_vlus <- paste0("standard errors in parantheses.", 
                            "\\textsuperscript{***}p \\textless 0.001;",
                            "\\textsuperscript{**}p \\textless 0.01;",
                            "\\textsuperscript{*}p \\textless 0.05.")
    
    sig_note <- sprintf("\\hline \n \\multicolumn{4}{l}{\\footnotesize{%s}}\n", sig_note_vlus)

    l_add_to_row <- list()
    l_add_to_row$pos <- list(-1, nrow(dt_coefs_fmtd))
    l_add_to_row$command <- c(chr_mdl_names, sig_note)
    
    
    if (!is.null(vrbl_grps)) {
        
        dt_grp_order <- dt_coefs_fmtd %>% copy() %>% .[, .(vrbl)] %>%
            dt_vrbl_grps[., on = "vrbl"] %>%
            .[, nbr := 1:.N] %>%
            .[, .(pos = min(nbr)-1), grp] %>% # gets added at end of line -> add -1
            dt_grp_lbls[., on = "grp"] %>%
            .[!is.na(lbl)] %>% # yeet group labels that have no labels (Intercept)
            .[, lbl2 := sprintf("\\multicolumn{%s}{l}{\\textbf{%s}} \\\\ \n", ncol(dt_coefs_fmtd)-1, lbl)]
        ## -1 because vrbl column still active here

        ## if groups are used, add them to l_add_to_row
        l_add_to_row$pos <- c(l_add_to_row$pos, dt_grp_order$pos)
        l_add_to_row$command <- c(l_add_to_row$command, dt_grp_order$lbl2)
        
    }

    dt_coefs_fmtd[, vrbl := NULL]

    ## generate align cfg
    if (wcptbl) {
        align_cfg <- rep("l", ncol(dt_coefs_fmtd) + 1) # just use l (anything else gets ignored)
    } else {
                
        if (!is.null(vrbl_grps)) {
            align_cfg <- c("l", sprintf("p{%s}", wdth_grp), sprintf("p{%s}", wdth_vrbl),
                           # hacky way to get column counts: count occurences of multicolumn
                           rep("D{)}{)}{8)3} ", str_count(chr_mdl_names, "multicolumn"))) 
        }
        else {
            align_cfg <- c("l", sprintf("p{%s}", wdth_vrbl),
                           rep("D{)}{)}{8)3} ", str_count(chr_mdl_names, "multicolumn")))
        }
    }
    

    
    list(dt_fmtd = dt_coefs_fmtd,
         align_cfg = align_cfg,
         add_to_row = l_add_to_row,
         hline_after = c(0, nrow(dt_coefs_fmtd)-nrow(dt_gofs_fmtd)))

}






render_xtbl <- function(dt_fmtd, align_cfg, add_to_row, hline_after, caption, label, file) {
    #' generic xtable rendering command

    xtable(dt_fmtd, caption = caption, label = label, align = align_cfg) %>% 
        print.xtable(include.rownames = F,
                     include.colnames = F,
                     file = file,
                     sanitize.text.function = identity,
                     add.to.row = add_to_row,
                     hline.after = hline_after)

}


pvxtbl <- function(xtbl_rslt, crop = T) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' generic xtable preview command

    xtable(xtbl_rslt$dt_fmtd, align = xtbl_rslt$align_cfg) %>%
        pvlt(add.to.row = xtbl_rslt$add_to_row,
             include.colnames = F,
             hline.after = xtbl_rslt$hline_after,
             crop = crop)
    
}



## render_xtable(reprt_tbls$tbl_age_sumry, paste0(REP_TABLE_DIR, "tbl_age_sumry.pdf"))
## render_xtable(reprt_tbls$tbl_close_after_death, paste0(REP_TABLE_DIR, "tbl_close_after_death.pdf"))

## head(mtcars) %>% prvlt()
## data.table(brand = rownames(mtcars), mpg = mtcars$mpg, cyl = mtcars$cyl) %>%
    ## xtable(caption="cars") %>% 
    ## prvlt()


plt_inspector <- function(plt_list) {

    

    for (i in seq_along(plt_list)) {
        print(names(plt_list)[i])
        plot(plt_list[[i]])
        readline("press F to continue")
    
    }
    invisible(NULL)
   
}

prep_sqlitedb <- function(dbx, dfx, table_title, constraints, insert_data = F) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    
    #' create schema for dfx (in terms of dbx)
    #' add any additional constraints (primary key(s), foreign keys)
    #' insert data if insert_data==T
    
    schema <- dbDataType(dbx, dfx)

    if (table_title %in% dbListTables(dbx)) {
        dbRemoveTable(dbx, table_title)
    }

    ## try to modify primary keys: modify schema
        
    init_part <- sprintf("CREATE TABLE %s (", table_title)

    
    column_info <- paste0(imap_chr(schema, ~sprintf("%s %s", .y, .x)))
    ## constraints <- "PRIMARY KEY (mdl_id, gof_names)"
        
    setup_cmd <- paste0(c(init_part,
                          paste(c(column_info, constraints), collapse = ",\n"), ")"), collapse = "\n")
    cat(setup_cmd)
    dbSendQuery(dbx, setup_cmd)

    if (insert_data) {
        dbAppendTable(dbx, table_title, dfx)
    }
    return(invisible(T))
}
