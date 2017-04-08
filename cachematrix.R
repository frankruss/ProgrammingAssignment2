
#---------------------------------------------------------------------------
# Week 3 Programming Assignment #2 
# Lexical scoping and caching instead of recalculating
#
# Using the functions:
#
#   Generate a 3 x 3 matrix
#   > genmatrix <- matrix(sample(1:9), nrow=3, ncol=3)
#
#   Cache that matrix
#   > cachemat <- makeCacheMatrix(genmatrix)  
#
#   Demonstrate retrieval of matrix
#   > cachemat$get()   
#
#   Calculate the inverse
#   > cacheSolve(cachemat)
#
#   Demonstrate retrieval of previous matrix
#   > cacheSolve(cachemat)
#
# FrankRuss
#---------------------------------------------------------------------------


# makeCacheMatrix
# Establishes functions for get, set, inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  cachematrix <- NULL
  
  set <- function(y) {
    
    # <<- allows it to be found in parent environments
    x <<- y
    cachematrix <<- NULL
  }
  
  # Simply returns x
  get <- function() x
  
  # Calculates inverse, stores
  setinverse <- function(inverse) cachematrix <<- inverse
  
  # simply returns matrix
  getinverse <- function() cachematrix
  
  # Sets up functions in list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#---------------------------------------------------------------------------
# Uses solve() for inverse of matrix 
# Or retrieves previous calculation when same request is made
#---------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  # Get the inverse of the matrix
  cachematrix <- x$getinverse()
  
  # If we already calculated this
  if (!is.null(cachematrix)) {
    
    # Just retrieve previous
    
    message("Retrieving previously calculated cached data")
    return (cachematrix)
  }
  
  # Otherwise do new calculation
  newmatrix <- x$get()
  cachematrix <- solve(newmatrix)
  x$setinverse(cachematrix)
  cachematrix
}
