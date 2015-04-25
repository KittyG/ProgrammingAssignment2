##
## makeCacheMatrix: function to create a special "matrix" object
##                  that can cache its inverse.
## cacheSolve: function to compute the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. 
## 
## The makeCacheMatrix function takes an invertible matrix as input.
## It returns a list containing functions to
##   - set the value of the matrix (in the calling environment)
##   - get the value of the matrix
##   - set the value of the inverse of the matrix 
##         (in the calling environment)
##   - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        solvedMatrix <- NULL
        
        set <- function(newMatrixValue) {
                x <<- newMatrixValue
                solvedMatrix <<- NULL
        }
        get <- function() x
        setSolved <- function(solved) solvedMatrix <<- solved
        getSolved <- function() solvedMatrix
        
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)
}


## The cacheSolve function takes as input 
##   - the list returned from the makeCacheMatrix function
##   - any additional parameters to be passed to the solve function.
## It returns 
##   - the inverse of the matrix that was input to makeCacheMatrix. 
##
## If the matrix inverse has already been calculated, it gets the 
## matrix inverse from the cache and skips the computation. 
## Otherwise, it 
##   - calculates the inverse of the matrix
##   - sets the value of the matrix inverse in the cache 
##     via the setSolved function.

cacheSolve <- function(x, ...) {
        # if inverse already calculated, return cached inverse
        solvedMatrix <- x$getSolved()
        
        if(!is.null(solvedMatrix)) {
                message("getting cached data")
                return(solvedMatrix)
        }
        
        # otherwise, retrieve matrix from cache, calculate inverse, 
        # store inverse in cache
        data <- x$get()
        solvedMatrix <- solve(data, ...)
        x$setSolved(solvedMatrix)
        
        solvedMatrix
}


## following test functions based on tests posted by CTA Karl Schultz
## https://class.coursera.org/rprog-013/forum/thread?thread_id=127#post-661

## testCacheSolve function 
## - takes as input a matrix dimension (matrixDim)
## - creates a matrix of size matrixDim * matrixDim using a random normal
##   distribution
## - runs makeCacheMatrix on the created matrix
## - runs cacheSolve twice and compares the two results
## - outputs
##   - input matrix, cached matrix, and matrix inverse
##   - a warning message if the calculated and cached inverses are not 
##     identical
##
testCacheSolve <- function(matrixDim) {
        n <- matrixDim
        mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
        print("input matrix")
        print(mat)
        matCached <- makeCacheMatrix(mat)
        print("cached matrix")
        print( matCached$get())
        matSolved1 <- cacheSolve(matCached)
        matSolved2 <- cacheSolve(matCached)
        if (!identical(matSolved1, matSolved2))
                message("Cached version does not match solved version")
        else {
                print("matrix inverse")
                print(matSolved2)
        }
}

## timeCacheSolve function 
## - takes as input a matrix dimension (matrixDim)
## - creates a matrix of size matrixDim * matrixDim using a random normal
##   distribution
## - runs makeCacheMatrix on the created matrix
## - runs cacheSolve twice, storing system.time() data for each execution
## - outputs
##   - system.time() data ("user") for each execution
##   - a warning message if the time to calculate the inverse is less than
##     the time to retrieve the inverse from the cache
##
timeCacheSolve <- function(matrixDim) {
        n <- matrixDim
        mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
        matCached <- makeCacheMatrix(mat)
        time1 <- system.time(matSolved1 <- cacheSolve(matCached))
        time2 <- system.time(matSolved2 <- cacheSolve(matCached))
        print(paste("Solve time = ", time1["user.self"]))
        print(paste("Cache time = ", time2["user.self"]))
        if (time1["user.self"] < time2["user.self"])
                message("Solve time is less than cache time")        
}

##  sample tests and output

## using randomly generated 3 x 3 matrix

# > testCacheSolve(3)
# [1] "input matrix"
# [,1]       [,2]       [,3]
# [1,] -0.06607251  0.6071788  0.5198630
# [2,]  1.97171234  0.9679140  0.2585506
# [3,] -0.20751228 -0.4497720 -0.9870281
# [1] "cached matrix"
# [,1]       [,2]       [,3]
# [1,] -0.06607251  0.6071788  0.5198630
# [2,]  1.97171234  0.9679140  0.2585506
# [3,] -0.20751228 -0.4497720 -0.9870281
# getting cached data
# [1] "matrix inverse"
# [,1]       [,2]       [,3]
# [1,] -0.9895782  0.4310415 -0.4082954
# [2,]  2.2319485  0.2041421  1.2290313
# [3,] -0.8090127 -0.1836460 -1.4873513

## timed test with 512 x 512 matrix

# > timeCacheSolve(512)
# getting cached data
# [1] "Solve time =  0.4"
# [1] "Cache time =  0"







