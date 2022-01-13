## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Create list of functions to set matrix, get matrix
        # set inverse, and get inverse
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        ## If inverse has already been calculated, then return cached value.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # Get the matrix
        data <- x$get()
        # Solve the inverse of the matrix
        i <- solve(data, ...)
        # Cache the calculated inverse into the matrix object
        x$setinv(i)
        i
}
