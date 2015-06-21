## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is the class that exposes 
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
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

