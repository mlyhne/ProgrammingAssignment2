## makeCacheMatrix 
##    This function creates a special "matrix" object that can cache its inverse.
##    use cacheSolve to calculate/return the inverse matrix.
## cacheSolve
##    This function checks if the inverse matix is cached in a "makeCacheMatrix" object.
##    If the inverse matrix is not cache, calculate it and cache it.
##    Retuns the inverse matrix of "x"
##

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  ## matrix is stored in "x"
  ## declare "i" to store the inverse matrix.
  i <- NULL
  
  ## set the matrix stored in "x" function.
  set <- function(y){
    x <<- y
    ## when "x" is changed, the cached inverse matrix "i" must be flushed.
    i <<- NULL
  }
  
  ## get the matrix stored in "x".
  get <- function() x
  
  ## set the inverse matrix stored in "i".
  setinverse <- function(y) i <<- y
  
  ## get the inverse matrix stored in "i".
  getinverse <- function() i
  
  ## returns a list contining the functions declared in "makeCacheMatrix",
  ## so we can call them from outside the functions enviroment.
  list(set = set, 
       get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## cacheSolve returns the inverse of a "makeCacheMatrix" object.
cacheSolve <- function(x = makeCacheMatrix(), ...) {
  
  i <- x$getinverse()
  ## if "i" is NULL, get the matrix, solve the matrix and store the result in cache.
  if (is.null(i)){
    i <- solve(x$get(), ...)
    x$setinverse(i)
  }
  ## Return a matrix that is the inverse of 'x'
  i
}