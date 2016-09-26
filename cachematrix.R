## This script contains functions to save and get a matrix and its inverse matrix
## bulding a structured list with a list of fuctions that can be used to get/set matrix and its inversion
## matrix into other high enviroment to get the best performance in this operation.

## This function allows save and get your matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(minv) im <<- minv
  
  getsolve <- function() im
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## Get the inverse matrix calculated form the orignal matrix witch was set 
## with "set" fuction into makeCacheMatrix function

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m) 
    
  }
  
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}