## Put comments here that give an overall description of what your
## functions do

## Extra checking: is the matrix square
is.Square <- function(x) {
  d <- dim(x)
  if (d[1] == d[2]){
    return(1)
  } else {
    return(0)
  }
}

## Extra checking: is the matrix solveable
is.Solvable <- function(x) {
  if(det(x) == 0) {
    return(0)
  } else {
    return(1)
  }
}

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
    if(!is.Square(data)) {
      message("Matrix is not a square")
      return(invisible(NULL))
    }
    if (!is.Solvable(data)) {
      message("Matrix not solvable")
      return(invisible(NULL))
    }
    m <- solve(data)
    x$setinverse(m)
    return(m)
  }
}


# Testing
n <-makeCacheMatrix()
# Test case 1 is it a squre
m <-matrix(c(1,2,3,4,5,6),2)
n$set(m)
result <- cacheSolve(n)
if (is.null(result)) {
  print("Pass")
} else {
  print("Fail")
}
# Test case 2 is it solvable
m <-matrix(c(5,6,7,8,9,10,11,12,13),3)
n$set(m)
result <- cacheSolve(n)
if (is.null(result)) {
  print("Pass")
} else {
  print("Fail")
}
# Test case 3 write to cache
m <-matrix(c(1,2,3,4),2)
n$set(m)
result <- cacheSolve(n)
if (is.matrix(result)) {
  print("Pass")
} else {
  print("Fail")
}
# Test case 4 is it cached
cacheSolve(n)
