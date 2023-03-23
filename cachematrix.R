# R programming - Week 3 (Denise S. Almeida Ferreira)
# Assignment: Caching the Inverse of a Matrix

# Write the following functions:

# 1. makeCacheMatrix: This function creates a special “matrix” object that can
# cache its inverse.

# ANSWER:
# A pair of functions that cache the inverse of a matrix.
# This function creates a special "matrix" object that can cache its inverse.

install.packages("MASS")
library(MASS) # This package is used to calculate inverse for non squared as
              # well as squared matrices

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   # Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x            # Function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() {
    inver <- ginv(x)
    inver%*%x                   # Function to obtain inverse of the matrix
  }                   
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# 2. cacheSolve: This function computes the inverse of the special “matrix” 
# returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#ANSWER:

cacheSolve <- function(x, ...)  # Gets cache data.
{
  inv <- x$getinverse()
  if(!is.null(inv)){            # Cheking whether inverse is NULL
    message("Getting cached data")
    return(inv)                 # Returns inverse value
  }
  data <- x$get()
  inv <- solve(data,...)        # Calculates inverse value
  x$setinverse(inv)
  inv                           # Return a matrix that is the inverse of "x".  
  }

# Example:

M <- makeCacheMatrix(matrix(1:8,2,4))

M$get()

M$getinverse()

cacheSolve(M)
