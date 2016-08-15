## Creates a Matrix in the global environment and caches it for fast retreival.

## Makes a matrix and its functions accessible globally

makeCacheMatrix <- function(x = matrix()) {
inversed <- NULL
    # Make a Matrix "x" accessible globally
    set <- function (y) {
        x <<- y
        inversed <<- NULL
    }
    # Get the contents of x
    get <-function() x
    # Caches the inversed matrix
    setinverse <- function(solve) inversed <<- solve
    # Gets the cached inversed matrix
    getinverse <- function() inversed
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Applies Inverse on a matrix if it is not already Cached

cacheSolve <- function(x, ...) {
inversed <- x$getinverse()
    if(!is.null(inversed)){
        message("getting cached data")
        return(inversed)
    }
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinverse(inversed)
    inversed
        ## Return a matrix that is the inverse of 'x'
}
