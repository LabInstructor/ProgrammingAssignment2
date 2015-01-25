## These functions calculate the inverse of a matrix and store the inverse in cache.
## If the same matrix is evaluated again, these fucntions return the inverse already stored
## in the cache. If the matrix has changed, the new inverse is calculated and returned.

## makeCacheMatrix creates a special matrix object that caches its inverse. The inverse is 
## the inverse is calculated by cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The cacheSolve function takes a matrix as input, checks to see if the inverse is already
## in cache via getinverse. If the inverse is already in the cache it returns it. 
## If the inverse is not in cache it will calculate the inverse and store it in the cache via
## the setinverse function. 

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
