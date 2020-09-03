## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The functions "makeCacheMatrix" and "cacheSolve" 
## cache the inverse of a matrix
## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the value of matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##get the value of matrix
  get <- function(){x}
  ## set the inverse
  setInverse <- function(inverse) {inv <<- inverse}
  ## get the inverse
  getInverse <- function() {inv} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' and assign it to inv
  inv <- x$getInverse()
  ## Check if inverse is already cached or not
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  mat <- x$get()
  ##Compute inverse of matrix
  inv <- solve(mat, ...)
  ##Set the inverse
  x$setInverse(inv)
  ##Return inverse
  inv
}
