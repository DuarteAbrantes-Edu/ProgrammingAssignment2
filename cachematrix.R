## Functions that compute an inverse matrix of a given input invertible matrix  
## If the inverse matrix has already been computed at an earlier time
## the functions allows for returning the cached result thus preventing
## recalculation

## Function that creates an object that can set and get a cached matrix

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


## Function that retrieves the inverse of a matrix
## if the inverse was already computed for the input 
## matrix it returns the cached calculation
## othewise the function will perform the computation
## of the inverse matrix

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