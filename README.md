# Rprogramming-week3
#My own solution to the assignment from week 3 of the R programming 
#course on Coursera by Johns Hopkins University
#########Peer graded assignment week 3 ####################
#Programming assignment
#Lexical scoping

######LIBRARIES###########################################
library(dplyr)                                           #
##########################################################

#creating the cache matrix
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
       getinverse= getinverse)
}
  
#function to search for cache data or compute the inverse of matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#create a matrix
y = matrix(rnorm(n=100, mean=10, sd=2),
           nrow=10, ncol=10, byrow=T)
y1 = makeCacheMatrix(y)

#test the inverse function
cacheSolve(y1)

