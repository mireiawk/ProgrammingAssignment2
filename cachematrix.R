## These functions allow to cache an inverse of 
## a matrix to reduce time of calculations for the
## same matrix.

## This function takes a matrix as input and make 
## an object to be able to caching input.
## inv variable is used to store caching input so
## is reset whenever the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function return the inverse of a matrix
## but always try to get a cached version if exists.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
