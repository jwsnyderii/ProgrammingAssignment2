## The functions in this file are used to create and manipulate a
## matrix object.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.  It returns a list of setter and getter
## functions (or methods) for setting and getting matrix object's data or 
## inverse data.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function takes as an argument a matrix object created
## by the makeCacheMatrix function. The function returns the inverse matrix
## if it has already been cached in the object.  Otherwise it calculates
## the inverse using the 'solve' function, and returns and stores the inverse
## in the matrix object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
