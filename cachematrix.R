## This is the second programming assignment for R Programming class.
## In this assignment, we will write a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function create a "special matrix"
## with members of the given matrix "x" and its inverse "iv"
## it has four functions/methods to set and get the given matrix or 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) iv <<- inverse
    getInverse <- function() iv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function takes the "special matrix" defined by makeCacheMatrix
## function, and trys to retrieve the cached inverse for performance
## reasons. If not cached yet, it will solve for the inverse and save it 
## to cache.

cacheSolve <- function(x, ...) {
    iv <- x$getInverse()
    if(!is.null(iv)) {
        message("getting cached inverse matrix")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setInverse(iv)
    iv
}
