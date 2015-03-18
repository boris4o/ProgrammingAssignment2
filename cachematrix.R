#This function creates a special "matrix" object which caches its inverse.

makeCacheMatrix <- function(matr = matrix()) {
    inverse <- NULL
    set <- function(x) {
        matr <<- x;
        inverse <<- NULL;
    }
    get <- function() return(matr);
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function() return(inverse);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(matr, ...) {
    inverse <- matr$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- matr$get()
    invserse <- solve(data, ...)
    matr$setinverse(inverse)
    return(inverse)
}