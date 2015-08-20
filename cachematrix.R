## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setcache <- function(c) cache <<- c
  getcache <- function() cache
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getcache()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setcache(cache)
  cache
}





