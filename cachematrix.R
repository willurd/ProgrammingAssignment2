## This function creates a special kind of matrix that is able to cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## The variable that stores the inverse cache.
        cachedInverse <- NULL

        ## Sets a new matrix and invalidates the cache.
        set <- function(newMatrix) {
                x <<- newMatrix
                cachedInverse <<- NULL
        }

        ## Returns the matrix.
        get <- function() {
                x
        }

        ## Sets the cached inverse.
        setinverse <- function(inverse) {
                cachedInverse <<- inverse
        }

        ## Returns the cached inverse.
        getinverse <- function() {
                cachedInverse
        }

        ## Return the "special" matrix.
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes a special matrix (created with makeCacheMatrix),
## solves for its inverse if it has not already been cached, then returns
## its inverse.
cacheSolve <- function(x, ...) {
        ## Check if we have cached data.
        inverse <- x$getinverse()

        if (!is.null(inverse)) {
                ## We already have cached data; return it.
                return(inverse)
        }

        ## We have no cached data. Solve for the inverse and cache it.
        m <- x$get()
        inverse <- solve(m)
        x$setinverse(inverse)

        ## Return the inverse.
        inverse
}
