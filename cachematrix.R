## R Programming - Assigment 2
##
## Adalberto Cubillo
##
## These functions will make a custom cached matrix component 
## and will calculate the first time (if the matrix doesn't change) the inverse of the matrix.
## or return the cached one if re-executed with the same data. 

## Makes a custom matrix capable of caching the inverse of it. 
makeCacheMatrix <- function(matrix = matrix()) {
  cachedInverseMatrix <- NULL
  
  # Set the custom matrix data. 
  set <- function(newMatrix) {
    matrix <<- newMatrix
    cachedInverseMatrix <<- NULL
  }
  
  # Get the custom matrix data. 
  get <- function() {
    matrix
  }
  
  # Set the inverse matrix. 
  setInverse <- function(inverseMatrix) {
    cachedInverseMatrix <<- inverseMatrix
  }
  
  # Return the cached inverse matrix or NULL. 
  getInverse <- function() {
    cachedInverseMatrix
  }
  
  # List the special methods for caching the inverse matrix. 
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Will return the cached matrix 
##or calculate the inverse the first time (if the matrix doesn't change).
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # Return the cached inverse matrix. 
  if (!is.null(inverse)) {
    message("Getting cached inverse matrix.")
    return(inverse)
  }
  
  # Calculate and store the inverse matrix.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  # Return the inverse matrix.
  inverse
}
