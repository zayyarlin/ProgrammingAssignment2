## Wraps the matrix object with a list containing
## it's inverse value
makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(im) invertedMatrix <<- im
    getinverse <- function() invertedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Look up the availability of cached inverse value,
## if not found, compute inverse using 'solve' 
## and then set the result to cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
