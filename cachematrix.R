## File contains 2 functions makeCacheMatrix and cacheSolve. makeCacheMatrix returns a list of functions to
## get and set the cached value of the matrix and it's inverse. cacheSolve calculates the inverse of the matrix
## and returns it if it has not been set or returns the cached value of inverse matrix

## Function returns a list of 4 functions to set and get the value of matrix and the value of the inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initializes the variable to be use for the inverse matrix to null
  m <- NULL
  
  ## function for setting the matrix and the cached varialbe
  set <- function(y) {
    ## sets the value of the cached matrix
    x <<- y
    ##reinitializes the cached value inverse matrix to null
    m <<- NULL
  }
  
  ## function returns the value of the matrix 
  get <- function() x
  
  ## function sets the cached variable with inversed value of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  ## function gets the value of the cached inversed matrix
  getInverse <- function() m
  
  ## returns a list with all the functions listed above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function returns the value of the inverse matrix by using the functions in the list 
## provided as a parameter

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## initializes m with the cached value of the inversed matrix
  m <- x$getInverse()
  
  ## verify that the value of the inversed matrix is not empty and returns a message and the value if
  ## value has been set and ends the function call(exits).
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## gets the value of the matrix
  data <- x$get()
  
  ## calculates the inverse of the matrix assigns to local variable m
  m <- solve(data, ...)
  ## sets and caches the value of the inverse matrix
  x$setInverse(m)
  ## returns the inverse matrix
  m
}
