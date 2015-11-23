## ----------------------------------------------------------------------------
## Functionality: 
##      The functions will return inverse of matrix from the cache. It also
## inverse by using solve,cache and return inverse when cache not found or 
## matrix has been changed.
##
## Assumption: 
##      Supplied matrix is always invertible.
## ----------------------------------------------------------------------------

## Create a matrix to cache the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        old_value <- get()
        x <<- y
        if (!identical(old_value,x)) {
            m <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return if there is existing cache.
## Otherwise, inverse the supplied matrix, cache and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
