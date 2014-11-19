## Helper function which allow caching the Inverse of a Matrix which is a
## time-consuming computation if recalculated at runtime multiple times

#' Creates a special "matrix" object that can cache its inverse.
#' 
#' Creates a list containing functions to:
#' 1. \code{set(y)}: Assign the value of the matrix.
#' 2. \code{get}: Retrieve the value of the matrix.
#' 3. \code{setinverse(inverse)}: Assign the inverse value of the matrix. 
#' 4. \code{getinverse}: Retrieve the inverse value of the matrix.
#' 
#' @param x The square matrix.
#' @return The list that wraps the matrix and provides functions to set/get the
#'   value as well as set/get the inverse.
#' @seealso \code{\link{cacheSolve}} which is used in conjunction to return the
#'   inverse of the matrix.
#' @export
#' @examples
#' makeCacheMatrix()
#' m <- makeCacheMatrix(matrix(1:4, 2, 2))
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#' Computes the inverse of the wrapper "matrix" constructed by
#' \code{makeCacheMatrix}. If the inverse has already been calculated (and the
#' matrix has not changed), then \code{cachesolve} retrieves the inverse from
#' the cache.
#' 
#' @param x The special "matrix" cache wrapper constructed by \code{makeCacheMatrix}.
#' @param ... Further arguments passed to or from other methods to the \code{solve} function.
#' @return The inverse of the provided matrix.
#' @seealso \code{\link{makeCacheMatrix}} which is used in conjunction to return the
#'   inverse of the matrix.
#' @export
#' @examples
#' cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2)))
#' 
#' m <- makeCacheMatrix(matrix(1:4, 2, 2))
#' cacheSolve(m)
#' cacheSolve(m)
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (is.null(inv)) {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    } else {
        message("using cached inverse")
    }
    inv
}
