## Matrix inverse computation is a costly operation, these functions/objects will
## allow us to compute matrix inverse as well as store them in cache, so that 
## the next time we need a matrix inverse we can simply get it from cache
## instead of recomputation
##
## Example Test case:
## a <- makeCacheMatrix(new matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3))
## cacheSolve(a)
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## cacheSolve(a)
## getting cached data
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##

## Would take as input an invertible matrix and return
## a complex object which contains functions for storing
## its value and inverse 

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Would take as input a makeCacheMatrix object
## Will compute the matrix inverse
## if it is not there already in cache of passed object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
