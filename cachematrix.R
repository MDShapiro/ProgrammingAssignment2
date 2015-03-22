## makeCacheMatrix creates a list of functions for setting and obtaining
## matrix values from memory.
## cacheSolve computes the inverse of the matrix created by makeCacheMatrix

## makeCacheMatrix stores computed matrix values in memory for later retrieval

makeCacheMatrix <- function(x = matrix(),...) {
  m <- NULL
  setMatrix <- function(y) {
    message("setting cache")
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() {
    #message("getting cached data from mCM")
    x
  }
  setInvMatrix <- function(matrix) {
    message("setting inverse of matrix")
    m <<- matrix
  }
  getInvMatrix <- function() {
    message("getting inverse from cache")
    m
  }
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve computes the inverse of matrix 'x' and stores the resulting matrix in the
## makeCacheMatrix object

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()

  if(!is.null(m)) {
    mti <<- m
    im <- solve(m)
    x$setInvMatrix(im)
    return(im)
  }
  else {
   x$getInvMatrix()    
  }
}
