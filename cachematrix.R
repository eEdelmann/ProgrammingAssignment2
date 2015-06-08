# makeCacheMatrix
# This inputs work both with vectors and square matrices

makeCacheMatrix <- function(x = numeric(), z = 2) {
  cacheInv <<- NULL
  # First caches the input
  setMat <- function(y) {
    cacheInv <<- NULL
    cacheMat <<- y
  }
  setMat(x)
  # Creates and caches square matrix when the input is a vector
  getMat <- function(a) {
  # This input z allows the matrix to be always square
    Matriz <- matrix(a, z, z)
    # set matrix in cache
    cacheMat <<- Matriz
  }
  # Getting and setting the inverse matrix
  b <- getMat(x)
  getInv <- solve(b)
  setInv <- function() {
    cacheInv <<- getInv
  }
  setInv()
  message("You just cached a matrix!")
  getInv  
}


# cacheSolve

InvCacheMat <- function(x, ...) {
  # Calling cached matrix
  if(identical(x, cacheMat)==TRUE) {
    message("We're giving you your cached matrix!")
    return(cacheInv)
  } else {
    # Solving uncached matrices
    getInv <- solve(x)
    message("We're solving your uncached matrix!")
    getInv
  }
}


# Example matrix - jus to make testing easier.

i <- matrix(30:33, 2, 2)
