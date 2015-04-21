# makeCacheMatrix: function to create a special "matrix" object
#                  that can cache its inverse.
# cacheSolve: function to compute the inverse of the special "matrix" 
#             returned by makeCacheMatrix above. 

# The makeCacheMatrix function takes an invertible matrix as input.
# It returns a list containing functions to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse of the matrix
# - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


# The cacheSolve function takes as input 
# - the list returned from the makeCacheMatrix function
# - any additional parameters to be passed to the solve function.
# It returns the inverse of the matrix that was input to makeCacheMatrix. 
#
# It first checks to see if the matrix inverse has already been calculated.
# If so, it gets the matrix inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets 
# the value of the matrix inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
