## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## There may be some benefot to caching the inverse instead of repeat computations
## Below are functions that are used to create an object
## These will store a matrix and it's inverse

## This function will create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x<<-y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will compute thte inverse of the special matrix above
## If it's already been calcuated, then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
      message("retrieving cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
