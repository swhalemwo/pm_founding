## * basic test of testing 



## lol <- 10

## expect_true((lul <- 10) > 0)

## test_fun <- function() {
test_that("basic tests", {
    ## lol <- 7
    expect_true((lul <- 10) > 0)
    ## print(lol)
    expect_equal(test_me(3,4), lul)
    expect_equal(test_me(5,5), lul)
    ## expect_equal(test_me(3,2), 7)
    ## expect_equal(purrr::quietly(warning("jj")), 10)
    ## expect_equal(warning("jj"), warning("jj"))
    expect_warning(warning("jj"))
    ## print("jj")
})

test_that("more tests", {
    expect_equal(test_me(3,4), 8)
    expect_equal(test_me(3,4), 7)
    expect_equal(test_me(2,3), 5)
})
    
## expect_equal(lul, 10)


## }

## test_fun()
