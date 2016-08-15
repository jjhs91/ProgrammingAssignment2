## Calculate the inverse of a matrix and cache it, when you run cacheSolve
## looks if the inverse haye been calculated, if that is true just get it from the
## cache

## create a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## gets the inverse of the matrix from cache or calculate ir if the inverse
## has not been calculated yet

cacheSolve <- function(x, ...){
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}