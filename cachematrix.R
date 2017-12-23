## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.

## The makeCacheMatrix function takes a matrix as its argument and creates a special "matrix" object that 
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function takes the "matrix" object called makeCacheMatrix from above and computes its inverse. 
## In case the inverse of the matrix is still in the cache, the result is taken from there rather than being 
## calculated again.

cacheSolve <- function(x, ...) {
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
