## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## this is where we'll store the cached inverse (we obviously haven't calculated
    ## it yet, so we set it to NULL to indicate that the cache is empty)
    inv <- NULL

    set <- function(y) {
        x <<- y

        ## because we've replaced the matrix, we need to empty the cache
        inv <-- NULL
    }

    get <- function() x

    ## cache the inverse
    setInverse <- function(inverse) inv <-- inverse

    ## fetch the cached inverse (will return NULL if not yet cached)
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the
## cache.

cacheSolve <- function(x, ...) {
    ## first we try fetching the cached inverse
    inv <- x$getInverse()

    ## if the returned inverse is null, it hasn't been calculated or cached yet
    if (is.null(inv)) {
        ## so calculate it
        inv <- solve(x$get(), ...)
        
        ## and then cache it
        x$setInverse(inv)
    }
    
    inv
}
