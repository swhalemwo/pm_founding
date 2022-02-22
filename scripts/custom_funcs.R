
'%!in%' <- function(x,y)!('%in%'(x,y))
len <- length


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

    dfb <- df_anls[,c("countrycode", "year", "nbr_opened")]
    dfc <- as_tibble(merge(dfb, vx, by = c("year", "countrycode"), all.x = TRUE))

    cry_cvrg <- aggregate(year ~ countrycode, na.omit(dfc), length)
    crys_geq3 <- cry_cvrg[which(cry_cvrg$year >= 3),]$countrycode
    cry_pm_crvg_actual <- aggregate(nbr_opened ~ countrycode, na.omit(dfc), sum)
    cry_pm_crvg_ideal <- aggregate(nbr_opened ~ countrycode, dfc, sum)
    names(cry_pm_crvg_ideal) <- c("countrycode", "nbr_opened_ideal")

    cry_pm_cvrg_cprn <- as_tibble(merge(cry_pm_crvg_ideal, cry_pm_crvg_actual, all.x = TRUE))
    cry_pm_cvrg_cprn$nbr_opened[which(is.na(cry_pm_cvrg_cprn$nbr_opened))] <- 0
    cry_pm_cvrg_cprn$diff <- cry_pm_cvrg_cprn$nbr_opened - cry_pm_cvrg_cprn$nbr_opened_ideal

    ## most_affected_crys <- unlist(lapply(sort(cry_pm_cvrg_cprn$diff)[1:4],
    ##                                     function(x) (filter(cry_pm_cvrg_cprn, diff == x)$countrycode)))

    most_affected_crys <- cry_pm_cvrg_cprn$countrycode[order(cry_pm_cvrg_cprn$diff)[1:4]]
    

    PMs_covered_raw <- sum(na.omit(dfc[which(dfc$countrycode %in% crys_geq3),])$nbr_opened)

    cry_cvrg_geq3 <- sum(filter(dfc, countrycode %in% crys_geq3)$nbr_opened)

    nbr_of_crys_geq3 <- len(crys_geq3)

    ## how many of crys_geq3 that have at least one PM founded, maybe relevant for comparative purposes 
    nbr_of_crys_geq1pm <- filter(aggregate(nbr_opened ~ countrycode, dfc, sum), countrycode %in% crys_geq3) %>%
        filter(nbr_opened >= 1) %>%
        nrow()

    return(list(
        varx=varx,
        nobs=nrow(vx),
        PMs_covered_raw=PMs_covered_raw,
        cry_cvrg_geq3=cry_cvrg_geq3,
        most_affected_crys = paste(most_affected_crys, collapse = "--"),
        nbr_of_crys_geq3=nbr_of_crys_geq3,
        nbr_of_crys_geq1pm=nbr_of_crys_geq1pm))
 
}

