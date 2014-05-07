## Matrix inversion is usually a costly computation. 
## The R script tries to cache the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set  <- function(y){
        x <<- y
        matrixInv <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) matrixInv  <<- inverse
    getinverse  <- function() matrixInv
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    inv  <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data  <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}
