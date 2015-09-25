## This R file contains 2 functions to store in cache, manage and
## and invert a matrix.


## Create a function makeCacheMatrix which contains 4 functions 
## to manage a matrix passed through parameter.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Create a function cacheSolve that invert a matrix,
## if the inverted matrix is already in calculated and 
## stored in cache, matrix won't be recalculated and a
## message will be return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
