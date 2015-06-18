## Cache the computed inverse of a square matrix to save resources.
## Two primary functions are defined:
## 1) makeCacheMatrix - which creates a special "matrix", which is really
##                      a list containing a series of sub-functions to:
##        a) set the value of the matrix
##        b) get the value of the matrix
##        c) set the value of the inverted matrix
##        d) get the value of the inverted matrix
## 2) cacheSolve - computes the inverted matrix of the special "matrix" created
##                 with the makeCacheMatrix function. Checks first to see if 
##                 the inverted matrix has already been computed. If so it gets
##                 the cached matrix and skips recomputation.  Otherwise, it 
##                 computes the inverted matrix via the solve function and sets
##                 the inverted matrix in the cache via the setsolve function.

## makeCacheMatrix - create special "matrix" which is a list of several functions.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) im <<- solve
    getsolve <- function() im
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## cacheSolve - check if inverted matrix is cached, if not compute inverted
##              matrix and store in cache via setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getsolve()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setsolve(im)
    im
}
