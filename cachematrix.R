## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#JQ: This function creates a "cacheable" matrix that can store its inverse.
#JQ: Provides the properties:
#JQ:    - set(matrix). Sets the matrix to working with.
#JQ:    - ("cacheable matrix") get. Returns the original matrix, previously setted with the "set" method.
#JQ:    - setinverse (inverse). Sets the inverse for the matrix.
#JQ:    - ("inverse of the matrix") getinverse. Returns the inverse of the matrix, previously stored with setinverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
#JQ: Solves the matrix "x", previously created with the function "makeCacheMatrix" specified in parameters and stores its inverse. Returns the inverse of the matrix.
#JQ: The first time its called will calculate the inverse of the matrix, subsequent calls for this matrix
#JQ: will return the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
