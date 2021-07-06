## Functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve #invert our matrix using the solve function 
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve gives the inverse of the special "matrix" returned by makeCacheMatrix
## When the inverse has already been calculated and Matrix did not changed, 
## cachesolve retrieves the inverse from the cache

cacheSolve<- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix.data <- x$get()
        s <- solve(matrix.data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
