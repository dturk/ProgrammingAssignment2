## An overall description of the code in this file:
# This file contains a pair of functions that compute and cache the inverse of 
# an invertible matrix to help reduce excess costly computation that may arrise 
# from the repeate use of the same calculaton.

## A short comment describing the following function:
# The "makeCacheMatrix" function creates a list containing functions to
#  > set the value of the matrix
#  > get the value of the matrix
#  > set the value of the inverse of the matrix
#  > get the value of the inverse of the matrix
#
# Arguments: makeCacheMatrix takes a single argument - an invertible matrix

makeCacheMatrix <- function(x = solve()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## A short comment describing the following function:
# The "cacheSolve" function returns the inverse of matrix 'x' from 
# a special version created with the "makeCacheMatrix" function if 'x' 
# has not changed since the last cacheSolve request. If 'x' has changed
# then cacheSolve recalculates the inverse and updates the cache accordingly
#
# Arguments: cacheSolve takes a signle argument - the output of makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
