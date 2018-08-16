## When used together, the makeCacheMatrix and cacheSolve functions allow for
## the inverse of a matrix to be cached for fast recall later, reducing the
## costly computation time needed to compute the inverse from scratch each
## time.

## The makeCacheMatrix function creates a special list object containing four
## functions:  set and get, for assigning and retrieving a square, invertible
## data matrix; and setinv and getinv, for assigning and retrieving the inverse
## of this data matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The cacheSolve function calls upon the list object produced by the
## makeCacheMatrix function in order to do one of two things.  If the inverse
## of the data matrix stored in the object has NOT been calculated yet,
## cacheSolve will compute the inverse using R's solve function, store it, and
## display it.  If the inverse of the data matrix HAS been stored in the
## object, cacheSolve will retrieve the stored inverse and print it without
## having to perform a new calculation.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}