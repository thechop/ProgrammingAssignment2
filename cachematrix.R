## The makeCacheMatrix function creates a special matrix that contains original matrix
## and the result of some operation on that matrix. I've made it generic so that result can
## be anything. For the purpose of this assessment the result will be the inverse of the
## original matrix.
## The function cacheSolve return the inverse of a special matrix constructed with a call to
## makeCacheMatrix. function cacheSolve returns the cached inverse matrix result it is exist or,
## if it doesn't exist, computes the inverse and stores the result in the cache before returning it.

## Makes a special matrix that can store a cached result of an operation on the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  cim <- NULL # The cached inverse matrix

  setMatrix <- function(m) {
    cim <<- NULL
    x <<- m
  }

  getMatrix <- function() x
    
  # Function to set the inverse matrix  
  setResult = function(im) {
    cim <<- im
  }
  getResult <- function() cim
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setResult = setResult, getResult = getResult)
}


## Takes a matrix constructed from makeCacheMatrix and returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check the cache first
  im <- x$getResult()
  if(!is.null(im)) {
    message("Retreiving from cache")
    return(im)  # Return the cached result (which is the inverse matrix)
  }
  # Else, matrix inverse hasn't been computed.
  # Compute and store in the cache
  m <- x$getMatrix()
  im <- solve(m, ...)
  x$setResult(im)
  im
}
