## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates an object for storing a matrix and it's inverse in cache.
## It provides the methods for setting and getting the matrix and its inverse for 
## retrieval by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve is a function that manages the interactions with the makeCaccheMatrix
## objects, including creating a new inverse if one isn't available in cache. I 
## added a check to see if the matrix is 1) a square matrix and 2) invertible by checking for a determnant of
## for rows = cols and a determinant = 0. If the matrix is not square or invertible an 
## appropriate message is returned.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## this if/else clause checks to see if matrix is invertible, rows = cols
  ## determinant != 0, and returns an appropriate error message.
  
  if (dim(x$get())[1] != dim(x$get())[2]){
    message("ERROR - it must be a square matrix to be invertible!")
    message("This is a ", dim(x$get())[1], " by ", dim(x$get())[2], " matrix.")
    message(" ")
    return(x$get())}
  else if(det(x$get()) == 0){
    message("This matrix is not invertible,")
    message("The determinant = ", det(x$get()))
    message(" ")
    return(x$get())
  }
  
  ## to get the inverse, check to see if it is already in cache, if NULL
  ## get data and create a new one.
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    message("The inverse is:")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  message("The original matrix is: ")
  message(x$get())
  message(" ")
  message("The inverse is:")
  inverse
}
