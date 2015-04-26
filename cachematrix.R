## The following code is intended to cache the inverse of a matrix to save time
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), c(2,2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1

## This will create a matrix, which is a list containing
## a function to
##   - set value of the matrix
##   - get value of the matrix
##   - set value of the inverse matrix
##   - get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This will calculate inverse of matrix created with above
## function, while reusing cached result if available

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
