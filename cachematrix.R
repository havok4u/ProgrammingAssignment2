## Put comments here that give an overall description of what your
## functions do

## This function creates an object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function caches an inverse if the inverse is not cached or the matrix changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Returning from cache")
    return(m)
  } else {
    data <-x$get()
    m <- solve(data)
    x$setinverse(m)
    return(m)
  }
}
