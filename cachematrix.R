## R Program Assignment 2
## This source is inclused two funtions
##
## makeCacheMatrix (Matrix )
## cacheSolve (output from makeCacheMatrix)
## to make the Inverse of a Matrix


## makeCacheMatrix (matrix)
## example: mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
## a<- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  y <- NULL
  setmatrix <- function(y) {
    x <<- y        ## cache for matrix
    m <<- NULL     ## set value to m   
  }
  getmatrix <- function() x
  setsolve <- function(solve) 
  m <<- solve
  getsolve <- function() m

  ## making list for cache
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setsolve  = setsolve ,
       getsolve = getsolve)
}


## cacheSolve (cached data)
## example: cacheSolve(mat2)
##

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$getmatrix()
  x$setmatrix(data)
  ## computing for solve (matrix inverse)
  
  m <- solve(data, ...)
  x$setsolve(m)
  m   ##return m
}

