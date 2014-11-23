## Coursera R Programming Assignment 2

## Function that cache matrix and its inverse
## set(x)          : cache  original matrix
## get()           : get cache of original matrix
## setsolve(solve) : cache inverted matrix 
## getsolve()      : get cache of inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Function that calculate, cache (if necessary) and return the inverse of the matrix
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) { ## If the inverse of the matrix exists in cache, return it
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
