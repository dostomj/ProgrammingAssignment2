## Put comments here that give an overall description of what your
## functions do

## crteates a wrapper for the matrix with the capability to cache the inverse  
## returns the special matrix
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


## cacheSolve computes the inverse of the matrix and saves the value in the cache.
## first argument x is a special matrix created using makeCacheMatrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## see if the inverse is cached      
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting inverse from cached data")
    return(i)
  }
  
  ## compute the inverse as the inverse is not already in cache
  ## extract the matrix from the special matrix
  matrix <- x$get()
  
  ## compute the inverse
  i <- solve(matrix, ...)
  
  ## cache the inverse for the matrix
  x$setinverse(i)
  
  ## return the inverse of the matrix
  i
}


