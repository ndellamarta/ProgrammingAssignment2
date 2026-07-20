## The following pair of functions cache the inverse of a matrix:

## 1. "makeCacheMatrix" creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y) {
                x <<- y
                j <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) j <<- inverse
        getinv <- function() j
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## "cacheSolve" computes the inverse of the special "matrix" returned by 
## "makeCacheMatrix" above. If the inverse has already been calculated 
## (and the matrix has not changed), then "cacheSolve" should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        j <- x$getinv()
        if(!is.null(j)) {
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat, ...)
        x$setinv(j)
        j
}
