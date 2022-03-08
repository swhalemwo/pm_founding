## * reading in

tax_vars_all <- c("NPO.tax.exemption", "Individual.Tax.Incentives", "Corporate.Tax.Incentives", "Estate.Tax.Reducable.by.donation", "money_score", "time_score", "Q1","Q2","Q3","Q4","Q5")

tax_vars_caf <- c("NPO.tax.exemption", "Individual.Tax.Incentives", "Corporate.Tax.Incentives", "Estate.Tax.Reducable.by.donation", "money_score", "time_score")



read_in_tax_incentives <- function() {
    #' read in the tax data sources: CAF rules to give by/world giving  index, Hudson

    ## ** CAF rules to give by
    df_rules <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "rules_to_give_by.csv")))
    df_rules$iso3c <- countrycode(df_rules$country, "country.name", "iso3c")
    ## huh that went well 

    df_rules$Estate.Tax.Reducable.by.donation <- 0
    df_rules[which(df_rules$Estate.tax == 1 & df_rules$Donations.after.death.exempt ==1),"Estate.Tax.Reducable.by.donation"] <- 1

    ## ** CAF world giving index
    df_give <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "caf_world_giving_index.csv")))

    df_give$iso3c <- countrycode(df_give$Country, "country.name", "wb")


    ## ** Hudson
    df_hudson <- as_tibble(read.csv(paste0(TAX_INCENTIVES_DIR, "hudson.csv")))
    df_hudson$iso3c <- countrycode(df_hudson$Country, "country.name", "iso3c")


    ## ** merging, check that names are unique
    ## c(names(df_rules),names(df_give),names(df_hudson))
    ## unique(c(names(df_rules),names(df_give),names(df_hudson)))


    df_taxinc <- as_tibble(Reduce(function(x,y,...) merge(x,y, by=c("iso3c"), all = TRUE),
                                  list(df_rules, df_give, df_hudson)))

    return(df_taxinc)
}




## * anls 

## ** compare PCAs separately next to each other



viz_pca_arrows <- function(pca.res, title) {
    #' do these nice arrow plots of the first two pca factors
    p <- fviz_pca_var(pca.res,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE,     # Avoid text overlapping
                      title = title
                      )
    return(p)
}
## p.arrows.all <- viz_pca_arrows(res.pca.all, title = "All")


viz_loadings <- function(res.pca, nbr_factors=3, title = NA) {
    #' visualize pca loadings
    res.pca.df <- as.data.frame(res.pca$rotation[,1:nbr_factors])
    res.pca.df$item <- rownames(res.pca.df)
    rownames(res.pca.df) <- NULL
    pca.melt <- reshape2::melt(res.pca.df, id="item")
    ggplot(pca.melt, aes(x=item, y=abs(value), fill=value)) +
        facet_wrap(~ variable, nrow=1) +
        geom_bar(stat="identity") +
        coord_flip () +
        scale_fill_gradient2(name = "Loading", 
                             high = "green", mid = "white", low = "red", 
                             midpoint=0, guide=F) +
        ylab("Loading Strength") + #improve y-axis label
        theme_bw(base_size=10) +
        labs(title = title)
}
## p.loads.all <- viz_loadings(res.pca.all, title = "ALL") 


create_pca_df <- function(df, pca.res, id.col, nbr.factors=3, rename_cols = NA) {
    #' bind the pca scores to an identifier 
    #' id.col can be single string or vector of columns


    ## use the pca.res rownames to not have to pass vars as separate variable

    pca_df <- na.omit(df[,c(rownames(pca.res$rotation), id.col)])[,id.col]
    pca_scores <- pca.res$x[,1:nbr.factors]
    pca_df <- as_tibble(cbind(pca_df, pca_scores))


    ## rename cols if requested
    if (!is.na(rename_cols)) {
        rnm_col_ids <- (length(id.col)+1):ncol(pca_df)
        ## print(rename_col_ids)
        names(pca_df)[rnm_col_ids] <- paste0(names(pca_df)[rnm_col_ids], "_", rename_cols)
    }
        
    return(pca_df)
}


## create_pca_df(df=df_taxinc, pca.res = res.pca.all, id.col = "country")
## create_pca_df(df=df_taxinc, pca.res = res.pca.caf, id.col = "country")

## create_pca_df(df=df_taxinc, pca.res = res.pca.all, id.col = c("country", "subregion"))
## create_pca_df(df=df_taxinc, pca.res = res.pca.caf, id.col = c("country", "subregion"))

     
create_ind_plot <- function(df, pca.res, label_col, color_col, vars, title) {
    #' create the individual plot
    #' df: the original plot
    #' label_col: string of variable that holds labels
    #' color_col: string of var to color by 
    
    opt_vars <- c(label_col, color_col)
    opt_vars <- opt_vars[which(!is.na(opt_vars))]

    ## create plotting df with optional vars
    ## pca_df <- na.omit(df[,c(opt_vars, vars)])[,c(opt_vars)]
    ## pca_scores <- pca.res$x[,1:2]
    ## pca_df <- as_tibble(cbind(pca_df, pca_scores))

    pca_df <- create_pca_df(df, pca.res, id.col = opt_vars, nbr.factors = 2)
    
    
    ind.plt <- ggplot(pca_df, aes(x=PC1, y=PC2)) +
        geom_hline(yintercept=0, linetype="dashed") +
        geom_vline (xintercept=0, linetype="dashed")  +
        scale_fill_manual(values = colors_manual_light) +
        labs(title = title)

    ## different combinations of colors and labels
    ## it is inefficient to specify combinations separately:
    ## gonna be not possible if I add more parameters like size
    
    ## both label and color 
    if (!is.na(label_col) & !is.na(color_col)) {
        
        ind.plt <- ind.plt +
            geom_point(size=3) + 
            geom_label_repel(aes(label=substr(get(label_col), 1, 50),
                                 fill=get(color_col)), max.overlaps = 100)

        ## only label 
    } else if (!is.na(label_col)) {
        
        ind.plt <- ind.plt +
            geom_point(size=3) + 
            geom_label_repel(aes(label=substr(get(label_col), 1, 50)), max.overlaps = 100)

    } else if (!is.na(color_col)) {
        ## only color 

        ind.plt <- ind.plt +
            geom_point(aes(color=get(color_col)), size=3) +
            scale_color_manual(values = colors_manual_light)
        
    } else {
        
        ## neither color nor label
        ind.plt <- ind.plt +
            geom_point(size=3)
    }
    return(ind.plt)
}
        




pca_viz <- function(pca.res=NA, df=NA, vars=NA, title, label_col=NA, color_col=NA, return="plots") {
    #' general visualization things for a pca res
    #' either pass pca.res or df with vars
    #' if df with vars is passed, perform the PCA here 

    ind_plot <- FALSE

    if (is.na(pca.res)) {
        pca.res <- prcomp(na.omit(df[,vars]),scale = T)
        ind_plot <- TRUE
    }
    
    p.scree <- fviz_eig(pca.res, title = title)
    p.arrows <- viz_pca_arrows(pca.res, title = title)

    p.loads <- viz_loadings(pca.res, title = title)
    plot_list <- list(p.scree, p.arrows, p.loads)
    
    if (ind_plot){
        p.ind <- create_ind_plot(df=df, pca.res = pca.res, vars = vars, label_col = label_col,
                                 color_col = color_col, title = title)
        plot_list[[4]] <- p.ind
    }
    return(plot_list)
    ## grid.arrange(grobs = plot_list, nrow=2,ncol=2)
    
}



get_taxinc_dfs <- function() {
    #' overall function to create the tax incentive PCA scores
    #' atm var specifications hardcoded here, not sure if they should be moved outside later 
    #' atm also each list specified explicitly, maybe should be generalized into list of configs 


    df_taxinc <- read_in_tax_incentives()
    
    ## constructing the 
    res.pca.all <- prcomp(na.omit(df_taxinc[,tax_vars_all]),scale = T)
    res.pca.caf <- prcomp(na.omit(df_taxinc[,tax_vars_caf]),scale = T, retx = T)

    ## create_pca_df(df_taxinc, res.pca.all, id.col = "iso3c", nbr.factors = 2, rename_cols = "all")

    res.pca <- list(list(pca = res.pca.all, lbl = "all"),
                    list(pca = res.pca.caf, lbl = "caf"))

    res.pca.dfs <- lapply(res.pca, function(x) create_pca_df(df_taxinc, pca.res = x$pca, id.col = "iso3c", nbr.factors = 2, rename_cols = x$lbl))

    res.pca.cbn <- as_tibble(Reduce(function(x,y,...) merge(x,y,by="iso3c", all=TRUE), res.pca.dfs))
    return(res.pca.cbn)
}




