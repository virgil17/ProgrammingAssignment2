## Submission by Gerald Yong Khin Chuen
##
## The makeCacheMatrix creates a special object that contains the matrix and its inverse.
## cacheSolve is then used to determine the inverse of the matrix; the first time the
## inverse is calculated, it is stored in the cache. Subsequently when cacheSolve is 
## called, the cached result will be obtained.
##
## Here is a sample way to test these functions:
##
## > x <- makeCacheMatrix(matrix(c(1:8,10),3,3))
## > x$get()
##     [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6   10
## > cacheSolve(x)
##            [,1]       [,2] [,3]
## [1,] -0.6666667 -0.6666667    1
## [2,] -1.3333333  3.6666667   -2
## [3,]  1.0000000 -2.0000000    1
## > cacheSolve(x)
## [Cached inverse matrix]
##            [,1]       [,2] [,3]
## [1,] -0.6666667 -0.6666667    1
## [2,] -1.3333333  3.6666667   -2
## [3,]  1.0000000 -2.0000000    1
## > 
##
## Thanks for reviewing my work!

## This function creates a class that contains the matrix and its inverse,
## and the accessor methods to set both values.
makeCacheMatrix <- function(x = matrix()) {
  ## Stores the cache of the inverse matrix.
  matinv <- NULL

  ## Method to set the matrix to a new value and clear the cache.
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }

  ## Method to get the matrix.
  get <- function() x 

  ## Method to set the cache. Note, this does not calculate
  ## the inverse but just sets the cache to the provided value.
  setmatinv <- function(inv) matinv <<- inv

  ## Method to get the cached value.
  getmatinv <- function() matinv

  ## To return a list of the functions if the object is called directly.
  list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)
}


## This function determines the inverse of the matrix created with makeCacheMatrix.
## If the inverse has already been calculated before, the cached result will be returned.
cacheSolve <- function(x, ...) {
  ## Get the contents of the cache.
  matinv <- x$getmatinv()

  ## If the cache is not null, then just return it and job done.
  if(!is.null(matinv)) {
    message("[cached inverse matrix]")
    return(matinv)
  }

  ## Otherwise, proceed to calculate the inverse.
  data <- x$get()
  matinv <- solve(data, ...)

  ## Update the cache.
  x$setmatinv(matinv)

  ## Return the result.
  matinv
}
