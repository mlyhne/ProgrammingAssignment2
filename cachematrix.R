## creates a wrapper to matrix to cache the inverse matrix 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(y) i <<- y
  getinverse <- function() i
  list(set = set, 
       get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## This function calculates the inverse of a makeCacheMatrix,
## get cached result, if result not calculated, calculate and store in cache.
## return result.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## if i is NULL, get the matrix, solve the matrix and store the result in cache.
  if (is.null(i)){
    i <- solve(x$get(), ...)
    x$setinverse(i)
  }
  ## Return a matrix that is the inverse of 'x'
  i
}