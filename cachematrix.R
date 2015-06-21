## makeCacheMatrix creates functions that allow caching of the matrix and its inverse
## cacheSolve obtains the inverse of a matrix stored in the list returned by makeCacheMatrix, either
##  via calculation or by obtaining the cached result

## Usage:
##> n <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##> k <- makeCacheMatrix()
##> ans <- cacheSolve(k)
##> ans
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## makeCacheMatrix: create a list of functions that will get/set a matrix 
## and its inverse in a separate environment, effectively caching the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: determine the inverse of a matrix created via makeCacheMatrix
## If the inverse has not already been calculated, then calculate it and cache the result
## Otherwise, obtain the result from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}