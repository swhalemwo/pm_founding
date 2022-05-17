## * custom funcs

table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

'%!in%' <- function(x,y)!('%in%'(x,y))
len <- length
adf <- as.data.frame
atb <- as_tibble

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

rollmean_custom <- function(v, win_len, func, orientation = "left"){
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
    #' returns true if query is substring of target, by default ignore case
    ## Reduce("&",lapply(strsplit(query," ")[[1]],grepl,target,fixed=TRUE))
    Reduce("&",lapply(strsplit(query," ")[[1]], grepl, target, ignore.case = ignore.case))
}


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
# Xtsum
varname <- enquo(varname)
loc.unit <- enquo(unit)
ores <- data %>% summarise(Mean=mean(!! varname, na.rm=TRUE), sd=sd(!! varname, na.rm=TRUE), min = min(!! varname, na.rm=TRUE), max=max(!! varname, na.rm=TRUE), N=sum(as.numeric((!is.na(!! varname)))))
bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname)))) 
bres <- bmeans %>% ungroup() %>% summarise(sd = sd(meanx, na.rm=TRUE), min = min(meanx, na.rm=TRUE), max=max(meanx, na.rm=TRUE), n=sum(as.numeric(!is.na(t.count))), `T-bar`=mean(t.count, na.rm=TRUE))
wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, center=TRUE, scale=FALSE))
wres <- wdat %>% ungroup() %>% summarise(sd=sd(W.x, na.rm=TRUE), min=min(W.x, na.rm=TRUE), max=max(W.x, na.rm=TRUE))
# Loop to adjust the scales within group outputs against the overall mean
for(i in 2:3) {
wres[i] <- sum(ores[1], wres[i])
}

# Table Output
Variable <- matrix(c(varname, "", ""), ncol=1)
Comparison <- matrix(c("Overall", "Between", "Within"), ncol=1)
Mean <- matrix(c(ores[1], "", ""), ncol=1)
Observations <- matrix(c(paste0("N = ", ores[5]), paste0("n = ", bres[4]), paste0("T-bar = ", round(bres[5], 4))), ncol=1)
Tab <- rbind(ores[2:4], bres[1:3], wres[1:3])
Tab <- cbind(Tab, Observations)
Tab <- cbind(Mean, Tab)
Tab <- cbind(Comparison, Tab)
Tab <- cbind(Variable, Tab)

# Output
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
