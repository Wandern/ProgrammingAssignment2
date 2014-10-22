# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it repeatedly. 
# The below functions allow us to cache the inverse of a matrix and recall this inverse 
# rather than having to recalculate it each time.
  
# This function creates a special "matrix" object (list of functions)
# that can cache x's inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize the cached inverse variable
  i <- NULL
  
  #Function to set the matrix x (Input parameter of the makeCacheMatrix function)
  set <- function(y) {
    #Set x (Input parameter of the makeCacheMatrix function) to the value y
    x <<- y
    #Ensure the cached inverse variable is Initialized.
    i <<- NULL
  }
  
  #A function to acquire the matrix x.
  get <- function() x
  
  #The function that caches our inverse results to the cached variable i.
  setInverse <- function(inverse) i <<- inverse
  
  #A function to acquire the cached inverse. 
  getInverse <- function() i
  
  #Return the list of objects needed to solve the cache(find the inverse of matrix x)
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


# The function cacheSolve returns the inverse of the matrix created with
# the makeCacheMatrix function.
# If the cached inverse is available, cacheSolve retrieves it, and if
# not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ##Assign our cached variable i to inverse.
  inverse <- x$getInverse()
  
  ##If the cached inverse is still NULL
  if (is.null(inverse)) {
    ##Assign the cached matrix x to a new variable, data.
    data <- x$get()
    ##Find the inverse of matrix data and assign it to inverse, our current non cached inverse variable.
    inverse <- solve(data, ...)
    ##Assign the local inverse variable to the cached variable i.
    x$setInverse(inverse)
  }
  ##Return the inverse.
  inverse
}