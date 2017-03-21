## Functions to cache the inverse of a matrix

## Creates an object-like structure (a list of functions) capable of
## getting and setting and internal matrix and a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(sol) s <<- sol
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function takes a "CacheMatrix" (an object returned by makeCacheMatrix)
## and returns the inverse of its underlying matrix. If the inverse wasn't yet cached,
## the function calculates it and stores to cache.
## ... arguments are passed on to Matrix::solve

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
