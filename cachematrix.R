## A set of two functions which work together to cache and return the inverse of a matrix

## Creates a special "matrix" which is list of functions that caches the value of the inverse 
##of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){
                X <<- y
                mat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mat <<- inv
        getinv <- function() mat
        list(set = set, get = get, setinv = setinv, getinv = setinv)
}


## Checks to see if inversve has already been calculated, and is in cache. If so, returns cached 
## inverse. if not, calculates and returns inverse.

cacheSolve <- function(x, ...) {
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinv(mat)
        mat
        ## Return a matrix that is the inverse of 'x'
}
