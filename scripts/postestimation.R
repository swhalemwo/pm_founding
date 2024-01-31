## * funcs 


if (identical(args, character(0))) {
    stop("functions are done")
}

if (is.null(args[[1]])) {
    stop("functions are DONE")
}


## * main

PROJECT_DIR <- "/home/johannes/Dropbox/phd/papers/org_pop/"
SCRIPT_DIR <- paste0(PROJECT_DIR, "scripts/")

source(paste0(SCRIPT_DIR, "startup_postestimation.R"))

print("run postestimation")

## run the one-out analysis


postestimation(fldr_info_optmz)
