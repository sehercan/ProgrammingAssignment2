# Following function creates a special matrix
# It returns the list of functions for the following actions
# get -> to get the value of a matrix
# set -> to set a value to matrix
# setinverse -> to set the value of inverse matrix
# getinverse -> to get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #initial value for the inverse matrix
  inv <- NULL
  
  #set the value of matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #get the value of matrix
  get <- function() x
  
  #get the value of inverse matrix
  getinverse <- function() inv
  
  #set the value of inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #return the list of functions
  list(set =set, get = get, getinverse = getinverse, setinverse = setinverse)
  
}

# Following function returns the inverse of a matrix from cache
# if cache doesn't exist, it creates the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check whether the cache of inverse exists.
  # if yes, return inverse from cache 
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # else, get the matrix to calculate the inverse
  my_data <- x$get()
  
  # calculate the inverse using solve function
  inv <- solve(my_data, ...)
  
  # cache the inverse of the matrix
  x$setinverse(inv)
  
  # return the inverse
  inv
  
}
