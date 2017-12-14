######### Peer graded assignment week 3 ####################
# Programming assignment
# Lexical scoping

######LIBRARIES###########################################
library(dplyr)                                           #
##########################################################
# The following function makeCacheMatrix  creates a special matrix object, that can cache the inverse of the matrix, 
# if the inverse was calculated by the seond function cacheSolve.
# The function Cachesolve will calculate the inverse of the matrix, when the function is called for the first time, 
# otherwise the function will return the inverse of the matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
# The function Cachesolve will calculate the inverse of the matrix, when the function is called for the first time, 
# otherwise the function will return the inverse of the matrix from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Create a matrix 
y = matrix(rnorm(n=100, mean=10, sd=2),
           nrow=10, ncol=10, byrow=T)
y1 = makeCacheMatrix(y)

# Test cachSolve with the created matrix
cacheSolve(y1)

