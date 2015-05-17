## Put comments here that give an overall description of what your functions do

## Create an inversed matrix calling for another in same environment
# then starts putting back the values to the new inversed matrix

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x) {
    m <<- x;
    inverse <<- NULL;
  }
  
  get <- function() return(m);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, 
              setinv = setinv, 
              getinv = getinv))
}

## Computes whatever function is called based on the inversed matrix

cacheSolve <- function(m, ...) {
  inverse <- m$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- m$get()
  invserse <- solve(data, ...)
  m$setinv(inverse)
  return(inverse)
}
