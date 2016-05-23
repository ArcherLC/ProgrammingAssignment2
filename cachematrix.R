## Caching functions to compute the inverse of a square matrix

## Function returns vector that gets and sets values for matrix.  Also,
## it sets the environment for the matrix inverse computation.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of a square matrix, or retrieves the inverse of the matrix
## from cache if previously calculated, give the list of arguments from function makeCacheMatrix().

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinverse(m)
    m
}
