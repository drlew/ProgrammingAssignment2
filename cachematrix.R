## These functions provide calculation and cached storage of the
## inverse of a matrix.

## Function makeCacheMatrix(x = matrix()) creates a matrix object
## enhanced with caching functionality.
## This cached matrix object provides the caching functionality via
## four functions:
### x$set(y) sets a new matrix value y in the object
### x$get() returns the matrix value currently stored in the object
### x$setinv(inv) stores inv as the current inverse
### x$getinv() returns the currently stored inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedinv <- NULL
    
    set <- function(y) {
        x <<- y
        cachedinv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) cachedinv <<- inv
    
    getinv <- function() cachedinv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve(x, ...) obtains and returns the inverse of cached matrix x;
## x was previously obtained from function makeCacheMatrix(x).
## It uses the cached inverse in x, if any. Otherwise, it calculates the inverse as
## solve(x$get(), ...), and caches the result in addition to returning it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedinv <- x$getinv()
    if(!is.null(cachedinv)) {
        message("using cached inverse")
        return(cachedinv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
