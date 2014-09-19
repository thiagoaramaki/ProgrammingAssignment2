## The makeCacheMatrix is a function that converts a simple R matrix into one
## that could have its inverse stored

## Transform a simple matrix into a special matrix which inverted matrix can be 
## efficiently calculated

makeCacheMatrix <- function(x = matrix()) {
  #if m does not exist, create it
  if (!exists("m"))  m <<- NULL
  #m <- NULL
  
  #set a value for the matrix
  set <- function(y) {#
    x <<- y
    if (!exists("m", 1)) m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  # set the inverted matrix
  setInversa <- function(inversa) m <<- inversa
  
  #get the inverted matrix
  getInversa <- function() m
  
  #return the list of functions
  list( set = set, get = get, setInversa = setInversa, getInversa = getInversa)
  
}


## This function return the inverted matrix of the argument which is supposed
## to be an invertible one. If the inverted matrix already exists then no 
## calculation is done and the stored matrix is returned.

cacheMatrix <- function(x, ...) {
  # get the invertible matrix if it exists
  m <- x$getInversa()
  if(!is.null(m)) {
    if (checkInversa(x$get(),x$getInversa())==TRUE)
    {
      message("getting cached data")
      return(m)
    }
    #message("getting cached data")
    #return(m)
  } 
  
  # if the invertible matrix does not exist in cache...
  data <- x$get()
  # invert the matrix
  m <- solve(data, ...)
  #set new inverted matrix
  x$setInversa(m)
  #return ma
  m
}

checkInversa <- function(x=matrix, y=matrix) {
  mul <- x%*%y
  i <- diag(nrow(x))
  if (identical(mul,i)==TRUE) {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}