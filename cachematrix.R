## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## crteates a wrapper for the matrix with the capability to cache the inverse  
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the inverse to null
  i <- NULL
  
  ## setter for matrix
  set <- function(y) {
    x <<- y
    ## if the value of x changes, reset the value of i
    i <<- NULL
  }
  
  ## getter for matrix
  get <- function() x
  
  ## saves the inverse of the matrix in the parent environment
  setinverse <- function(inv) i <<- inv
  
  ## retrieves the value of inverse of the matrix from the cached parent environment
  getinverse <- function() i
  
  ## return the list that encapsulated all
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse from cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


