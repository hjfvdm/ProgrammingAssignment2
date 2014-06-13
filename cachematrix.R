## Methods for creating a cachable matrix where the inverse is cached and retrieved
## The cached inverse is returned if no changes have been made to the matrix.
##
## Example usage:
##
## invertibleMatrix = rbind(c(1, -1/4), c(-1/4, 1))  
## cachedMatrix <- makeCacheMatrix(invertibleMatrix)
## solvedMatrix <- cacheSolve(cachedMatrix)
## running the solveMatrix again will retrieve the cached result
## You will see this message: 'getting cached data'

## Creates a matrix with getters and setters for the inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
}


## Method will return the cached version of the matrix if it exists,
## otherwise it will be computed and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
