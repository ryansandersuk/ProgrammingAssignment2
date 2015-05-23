## This file contains the following functions:
##   makeCacheMatrix
##   cacheSolve

## Creates a list that holds a matrix and its inverse.
## The list holds functions for:
##   getting and setting the matrix,
##   getting and setting the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        # now that we have a new matrix, need to clear the cached inverse
        inverseMatrix <<- NULL
    }
    get <- function() { x }
    setInverse <- function(inverse) { inverseMatrix <<- inverse }
    getInverse <- function() { inverseMatrix }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Take a list created through makeCacheMatrix and:
##   returns the cached inverse matrix if it exists, or,
##   creates the inverse, caches it and returns the inverse, otherwise.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    # For this assignment, assume that the matrix supplied is always invertible
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
