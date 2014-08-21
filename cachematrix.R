## Author: Harsha
## Date: 08/21/2014
## Last Modified: 08/21/2014
## This R program contains two main functions
## Based on these two functions, this programe returns the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize a variable (that is to be the inverse of matrix) to NULL
    m <- NULL
    # Below function is used to reset/store the input matrix when the matrix has changed & reset inverse to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Stores the input matrix
    get <- function() x
    # Used to set the inverse of matrix 
    setinv <- function(inv) m <<- inv
    # Used to retrieve the inverse of matrix
    getinv <- function() m
    # A list containing the above 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Retrieve the inverse of matrix
    m <- x$getinv()
    # Check if a "null" is returned based on the above operation. If not, get the cached inverse matrix.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If a "null" is returned based on above "if" condition, store the input matrix.
    data <- x$get()
    # Calculate the inverse of matrix & store it
    m <- solve(data, ...)
    # Set the inverse of matrix using a function
    x$setinv(m)
    # Return the inverse of matrix
    m
}
