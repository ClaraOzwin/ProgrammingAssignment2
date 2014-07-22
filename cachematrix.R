## A pair of functions that cache the inverse of a matrix. Since matrix inversion
## can be a costly computation there can be benefit to caching the inverse rather than computing it repeatedly.

## makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setmatrix <- function(inv) mInv <<- inv
  getmatrix <- function() mInv
  list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve computes the inverse of the cached "matrix" returned by makeCacheMatrix.
## Retrieves the inverse from the cache if the inverse has already been calculated (and the matrix has not changed),

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getmatrix()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setmatrix(mInv)
  mInv
}

cachematrix.R (END) 