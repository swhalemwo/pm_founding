colors_manual <- c("#c78ab5","#74b648","#6f4aca","#c7994f","#cb4fc2","#516833","#cd4872","#5cb099","#d25133","#6289c0","#814135","#613a76")

colors_manual <- c("#01867b","#e2183d","#44ddb8","#891ba6","#93d93d","#ff3c8a","#006e00","#d2b1ff","#ff9e30","#009cf4","#ff6830","#86375a","#ae8400","#967649")

colors_manual_light <- c("#a4e3a5","#f197c1","#98fff4","#f29a83","#2bcef0","#ffefa5","#75bfff","#c2bc71","#d6d3ff","#52bcae","#ffc9c2","#85b4b6","#f3ffd5","#b0ad84")



## * reading in 
TAX_INCENTIVES_DIR <- paste0(PROJECT_DIR, "data/tax_incentives/")

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


## ** merging
## names are unique
c(names(df_rules),names(df_give),names(df_hudson))
unique(c(names(df_rules),names(df_give),names(df_hudson)))


df_taxinc <- as_tibble(Reduce(function(x,y,...) merge(x,y, by=c("iso3c"), all = TRUE), list(df_rules, df_give, df_hudson)))


## * anls 
tax_vars_all <- c("NPO.tax.exemption", "Individual.Tax.Incentives", "Corporate.Tax.Incentives", "Estate.Tax.Reducable.by.donation", "money_score", "time_score", "Q1","Q2","Q3","Q4","Q5")

tax_vars_caf <- c("NPO.tax.exemption", "Individual.Tax.Incentives", "Corporate.Tax.Incentives", "Estate.Tax.Reducable.by.donation", "money_score", "time_score")

library("PerformanceAnalytics")
library(factoextra)

pdf(paste0(FIG_DIR, "tax_incentive_cors.pdf"), width = 21, height = 12)
chart.Correlation(df_taxinc[,tax_vars_all], pch=21)
dev.off()

## ** some PCA

res.pca.all <- prcomp(na.omit(df_taxinc[,tax_vars_all]),scale = T, retx = T)
res.pca.caf <- prcomp(na.omit(df_taxinc[,tax_vars_caf]),scale = T, retx = T)



options(max.overlaps = 100)
options("max.overlaps" = 100)
options(ggrepel.max.overlaps = 100)
options("ggrepel.max.overlaps" = 100)


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





p1 <- viz_pca(res.pca.all, title = "ALL")
p2 <- viz_pca(res.pca.caf, title = "CAF")



## tax_inc1$PC3 <- res.pca$x[,3]
## tax_inc1$PC3[order(tax_inc1$PC3)]

## c(tax_inc1$country[order(tax_inc1$PC3)])

## compare how factors scores correlate depending on whether Hudson variables are included
## factor 2 no change really (r=0.97), but factor 1 "only" has 0.69 (nice) correlation
## think that's good enough tho to justify using CAF scores generally, if necessary

pca_all_df <- na.omit(df_taxinc[,c("country", tax_vars_all)])[,c("country")]
pca_all_df <- as_tibble(cbind(pca_all_df, res.pca.all$x[,1:3]))
names(pca_all_df)[2:4] <- paste0(names(pca_all_df)[2:4], "_all")

pca_caf_df <- na.omit(df_taxinc[,c("country", tax_vars_caf)])[,c("country")]
pca_caf_df <- as_tibble(cbind(pca_caf_df, res.pca.caf$x[,1:3]))
names(pca_caf_df)[2:4] <- paste0(names(pca_caf_df)[2:4], "_caf")

pca_cpr_df <- as_tibble(merge(pca_all_df, pca_caf_df))

chart.Correlation(pca_cpr_df[,2:7])

pdf(paste0(FIG_DIR, "pca_anls.pdf"), height=5, width=10)
p1
p2
chart.Correlation(pca_cpr_df[,2:7])
dev.off()
