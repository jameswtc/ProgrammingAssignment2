## These pair of functions compute and cache the inverse of a matrix 
## in the memory. If the inverse of the matrix has already been computed, 
## the cacheSolve returns the memory version of the inverse matrix. 
## Otherwise, the cacheSolve computes the inverse matrix, stores the result 
## in the memory cache, and then returns the inverse matrix.

## This function creates a special "matrix" object that can cache its inverse

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
         setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
