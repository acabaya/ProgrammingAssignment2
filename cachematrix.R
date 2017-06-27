## ACAbaya
## June 27, 2017

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 
## Here is pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    # 1. set the value of the matrix
    # 2. get the value of the matrix
    # 3. set the value of the inverse
    # 4. get the value of the inverse
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by 
    ## makeCacheMatrix above. If the inverse has already been calculated (and the 
    ## matrix has not changed), then cacheSolve should retrieve the inverse from the 
    ## cache.
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } else {
        message("set the data to cache")
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
