# Defines a matrix and allows for the setting and return thereof, and of the
# matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # Sets m value to NULL.
    m <- NULL
    
    # Defines set function in which x is defined; m is set to NULL.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # "get" returns the created matrix.
    get <- function() x
    
    # "setInverse" sets m to the (passed-in) inverse of a matrix and returns it.
    setInverse <- function(matrixinv) m <<- matrixinv
    
    # "getInverse" returns the inverted matrix.
    getInverse <- function() m
    
    # Returns a list of the functions.
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}

# Determines whether matrix inversion has already been taken place,
# returning the already-calculated inverted matrix if so. Otherwise
# calculates and returns the inverted matrix. 

cacheSolve <- function(x, ...) {

    # Sets m to the matrix-inverse of x. 
    m <- x$getInverse()
    
    # If m isn't null, it has been calculated already and cached,
    # and is therefore returned instead of being recalculated.
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    
    # If m is null, then it hasn't been calculated yet and is
    # therefore calculated now.
    data <- x$get()
    m <- solve(data, ...)
    
    # This section sets x's inverse matrix to the recently-
    # calculated m, and returns m.
    x$setInverse(m)
    m
    
}
