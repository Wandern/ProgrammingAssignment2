## The following functions create and populate "cache matrix" objects
## that can significantly improve performance of operations that require

## makeCacheMatrix creates a special "matrix" object that can cache
## the inverse of a matrix alongside the original. The returned value
## is actually a list with functions to access the original matrix and
## its cached inverse.
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
  
  # A function to acquire the matrix x.
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


## cacheSolve returns the inverse of a matrix, utilizing the list created by makeCacheMatrix. 
##If the inverse has not been calculated previously it will be and the result stored 
##for future requests. If it has been calculated previously then the cached
## value will be returned.
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