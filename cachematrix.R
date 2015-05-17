## Put comments here that give an overall description of what your functions do

## Create an inversed matrix calling for another in same environment
# then starts putting back the values to the new inversed matrix

makeCacheMatrix <- function(m = matrix()) { # calls function to m to set as matrix
  inverse <- NULL
  set <- function(x) {
    m <<- x;
    inverse <<- NULL;
  }
  
  # sets 4 parameters: 
  # 1 - sets the matrix (from function above)
  # 2 - gets the matrix
  # 3 - sets the inversed matrix
  # 4 - gets the inversed matrix
  
  get <- function() return(m);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);

  return(list(set = set, 
              get = get, 
              setinv = setinv, 
              getinv = getinv))
}

## Computes whatever function is called based on the inversed matrix
# from the output of the makeCacheMatrix

cacheSolve <- function(m, ...) {
  inverse <- m$getinv()
  if(!is.null(inverse)) {             # if the inverse is aleady calculated
    message("Getting cached data...")
    return(inverse)                   # get it from the cache and skips the computation. 
  }
  data <- m$get()                     # otherwise, calculates the inverse
  invserse <- solve(data, ...)
  m$setinv(inverse)                   # sets the value of the inverse in the cache via setinv
  return(inverse)
}
