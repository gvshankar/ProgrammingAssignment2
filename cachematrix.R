## Name:  cachematrix.R
## Purpose: This R function inverses a invertible matrix (if not done already).
##          It first searches the "cache" to see if a similar inverse has been
##          calculated.  If found, just fetches from cache and returns it.
#
## Function: makeCacheMatrix.  This has 4 functions to set, get the matrix,
##           and set, get the inverted matrix.
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinvm <- function(solve) m <<- solve
  getinvm <- function() m
  
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## Function: cacheSolve

cacheSolve <- function(x, ...) {
  
  m <- x$getinvm()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
    message("m is null")
  }

  data <- x$get()

  m <- solve(data, ...)

  x$setinvm(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

