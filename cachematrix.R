## The following functions setup a cached matrix and return the inverse
## of a matrix. If the inverse is already calculated it returns the cached value.

## Assumptions : The matrix supplied is always invertible.

## Function 'makeCacheMatrix' creates a matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  setMatrix <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  
  getMatrix <- function() x
  
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## Function 'cacheSolve' returns the inverse of a matrix and if the inverse is already calculated
## the function gets the value without computing.
## If not, the function continues to compute the inverse.

cacheSolve <- function(x, ...) {
       
  inverse <- x$getInverse()
  
  if (!is.null(inverse)){
    message("Getting cached Inverse Matrix")
    return(inverse)
  }
  
  matrix <- x$getMatrix()
  
  inverse <- solve(matrix,...)  ## Solve() to return the inverse of the matrix
  
  x$setInverse(inverse)
  
  inverse
  
}
