## cachematrix.R
## This file contains utility methods for working with matrix inverses.
## makeCacheMatrix is used for storing a matrix and it's inverse.
## cacheSolve will calculate the inverse for the given matrix if necessary, 
## and return the inverse 


## makeCacheMatrix stores a matrix x and defines getters and setters 
## for the matrix value and the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(i) {
    inv <<- i
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = set, get = get)
}

## cacheSolve accepts a cacheMatrix x and returns the inverse.
## If the inverse has not been calculated, it will be calculated and stored.
## if it has already been calculated, the already-calculated value will be returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  
  inv
}
