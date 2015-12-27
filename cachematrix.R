## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(matrixObj = matrix()) {
  invMatrixObj <- NULL
  set <- function(y) {
    matrixObj <<- y
    invMatrixObj <<- NULL
  }
  get <- function() matrixObj
  setinverse<- function(inverse) invMatrixObj <<-inverse
  getinverse <- function() invMatrixObj
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(matrixObj, ...) {
  invMatrixObj <- matrixObj$getinverse()
  if (!is.null(invMatrixObj)) {
    message("getting cached data")
    return(invMatrixObj)
  } else {
    invMatrixObj <- solve(matrixObj$get())
    matrixObj$setinverse(invMatrixObj)
    return(invMatrixObj)
  }
}
