

read_reg_res <- function(idx) {
    ##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@11@"]]));##:ess-bp-end:##
    


    reg_res <- readRDS(paste0(REG_RES_DIR, idx))



}


df_reg_anls <- atb(read.csv(paste0(REG_RES_FILE), sep = " ", header = F))
names(df_reg_anls) <- c("vrbl", "lag_vlu", "model_id", "converged")

table(df_reg_anls$V4)


