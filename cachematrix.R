## Caching the Inverse of a Matrix:
## Below are several functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a "matrix" that can cache its inverse.
makeCacheMatrix <- function(x = matrix())  {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the "matrix" created by function
## above. If the inverse has already been calculated and the matrix has not 
## changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
