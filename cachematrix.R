## makeCacheMatrix() 
##    Creates a wrapper object to matrix,
##    contains the matrix and the cashed calculation af the inverse matrix.
## cacheSolve()
##    Returns the inverse matrix from a makeCacheMatrix object.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  ## matrix is stored in x
  ## declare i to store the inverse matrix.
  i <- NULL
  
  ## set the matrix stored in x function.
  set <- function(y){
    x <<- y
    ## when x is changed, the cached inverse matrix must be flushed.
    i <<- NULL
  }
  
  ## get the matrix stored in x.
  get <- function() x
  
  ## set the inverse matrix stored in i
  setinverse <- function(y) i <<- y
  
  ## get the inverse matrix stored in i
  getinverse <- function() i
  
  ## returns a list contining the functions in makeCacheMatrix,
  ## so we can call them from outside the functions enviroment.
  list(set = set, 
       get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## This function calculates the inverse of a makeCacheMatrix,
## get cached result, if result not calculated, calculate and store in cache.
## return result.

cacheSolve <- function(x = makeCacheMatrix(), ...) {
  i <- x$getinverse()
  ## if i is NULL, get the matrix, solve the matrix and store the result in cache.
  if (is.null(i)){
    i <- solve(x$get(), ...)
    x$setinverse(i)
  }
  ## Return a matrix that is the inverse of 'x'
  i
}