## Write a short comment describing this function

# A pair of functions that cache the inverse of a matrix.
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## This function computes the inverse of matrix returned created
## by the function makeCacheMatrix.

CacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## This is an example matrix using the CacheSolve function on a defined matrix.
> pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
> pmatrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> pmatrix$getInverse()
NULL
> CacheSolve(pmatrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> CacheSolve(pmatrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5