## This are the functions for the Programming Assignment 2
## R programming - Coursera
## The first function makeCacheMatrix creates a matrix object. This object has
## two attributes, its own value and the value of its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL          # First, assign a null value to the inverse matrix. 
        set <- function(y) { 
                x <<- y       # The set function is defined in order to store 
                inv_mat <<- NULL  # the values for both the matrix and its inverse.
        }
        get <- function() x # The get function retrieves the value of the matrix.
        setinv <- function(inverse) inv_mat <<- inverse
        getinv <- function() inv_mat   # and similarly for the inverse matrix.
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

## The cacheSolve function checks whether the inverse is already calculated and
## stored and if that is the case, it prompts the cached value. If not, it
## computes the inverse of the given matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getinv() # First, check if matrix x has alreadt a stored inverse
    if(!is.null(inv_mat)) {
        message("Getting cached inverse matrix")  
        return(inv_mat)  # If that is the case, return the cached value with the 
                         # corresponding message
    }
    data_matrix <- x$get() # If not, obtain the value of the matrix using get
    inv_mat <- solve(data_matrix, ...) # calculate the inverse
    x$setinv(inv_mat) # storing the inverse matrix using setinv method or function
    inv_mat # return the inverse of the given matrix
}