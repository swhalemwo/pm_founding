
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

rollmean_custom <- function(v, win_len, func, orientation){
    # computes rolling mean without removing observations by using smaller windows at the sides
    c <- 1
    ## slider needs orientation if length of sliding window is even:
    ## question is whether to orient oneself to the right of left
    len_v <- length(v)

    orientation <- "left"

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
        ## print(paste0("indices_sel: ", indices_sel))
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




