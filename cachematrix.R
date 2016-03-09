## makeCacheMatrix caches the inverse of a matrix as inverse calculation can take a huge chunk of CPU time.
## set sets the value of the matrix
## get returns the matrix
## set_inverse sets the calculated inverse
## get_inverse returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve returns the inverse of a matrix, first it checks whether the matrix has been solved
## if it is solved then the value is taken from the cache else,
## new inverse in calculated and it is set to cache.
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
