## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list containing functions that:
## set the contents of the matrix
## get the contents of the matrix
## set the values of the inverse of the matrix
## get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- matrix()
  set <- function(y) {
    x <<- y
    inv_m <<- matrix()
  }
  get <- function() x
  setinv <- function(z) inv_m <<- z
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## The cacheSolve function checks if the inverse of a matrix has already
## been computed.  If so, it returns that cached inverted matrix.  If not,
## it does the inversion and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!all(is.na(inv_m))) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinv(inv_m)
  inv_m
}
