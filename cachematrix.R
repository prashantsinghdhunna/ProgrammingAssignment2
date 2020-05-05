##Matrix inversion is normally an exorbitant calculation and there might be a few
##benefit to caching the inverse of a matrix instead of register it over and again.
##The following are a couple of functions that are utilized to make a special object that 
##stores a matrix and caches its inverse.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(s) {
                x <<- s
                invrs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##This function processes the inverse of the special "matrix" made by
##makeCacheMatrix above. If the inverse has already been determined (and the 
##matrix has not changed), at that point it ought to recover the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if (!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        matrx <- x$get()
        invrs <- solve(matrx, ...)
        x$setInverse(invrs)
        invrs
}
