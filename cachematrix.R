# Course 2
# Week 3 - Programming Assignment

# Matrix inversion is a costly computation and there may be some benfit
# to caching the inverse of a matrix rather than computing it repeatedly.

# The function 'makeCacheMatrix' creates a special "matrix" object that 
# can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


# The function 'cacheSolve' computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then cacheSolve retrieves
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)){
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
