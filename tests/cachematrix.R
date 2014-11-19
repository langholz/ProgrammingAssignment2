test_that("makeCacheMatrix with no arguments constructs a valid object", {
    m <- makeCacheMatrix()
    expect_is(m, "list")
    expect_named(m, c("set", "get", "setinverse", "getinverse"))
    expect_is(m$get(), "matrix")
    expect_equivalent(dim(m$get()), c(1, 1))
    expect_equivalent(m$get()[1,1], NA)
    expect_equivalent(m$getinverse(), NULL)
})

test_that("makeCacheMatrix with a matrix as an arguments constructs a valid object", {
    expectedMatrix <- matrix(1:4, 2, 2)
    m <- makeCacheMatrix(expectedMatrix)
    expect_is(m, "list")
    expect_named(m, c("set", "get", "setinverse", "getinverse"))
    expect_is(m$get(), "matrix")
    expect_equivalent(dim(m$get()), c(2, 2))
    expect_equivalent(m$get(), expectedMatrix)
    expect_equivalent(m$getinverse(), NULL)
})

## The evaluate_promise from the testthat library has a bug uncovered while
## writing these units tests: https://github.com/hadley/testthat/issues/197.
## This is a temporary workaround I divised to use until the suggested fix is pulled.
evaluate_promise_v2 <- function (code, ...) {
    value <- NULL
    r <- evaluate_promise({ value <- code }, ...)
    r$result <- value
    r
}

test_that("cacheSolve calculates the square matrix inverse", {
    expectedMatrix <- matrix(1:4, 2, 2)
    expectedInverse <- matrix(c(-2, 1, 1.5, -0.5), 2, 2)
    m <- makeCacheMatrix(expectedMatrix)
    r <- evaluate_promise_v2(cacheSolve(m))
    expect_equivalent(r$result, expectedInverse)
    expect_equivalent(r$messages, character(0))
})

test_that("cacheSolve returnes the cached square matrix inverse", {
    expectedMatrix <- matrix(1:4, 2, 2)
    expectedInverse <- matrix(c(-2, 1, 1.5, -0.5), 2, 2)
    m <- makeCacheMatrix(expectedMatrix)
    r <- evaluate_promise_v2(cacheSolve(m))
    expect_equivalent(r$result, expectedInverse)
    expect_equivalent(r$messages, character(0))
    r <- evaluate_promise_v2(cacheSolve(m))
    expect_equivalent(r$result, expectedInverse)
    expect_equivalent(r$messages, "using cached inverse\n")
})
