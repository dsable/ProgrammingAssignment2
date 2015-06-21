## Coursera - Data Science - R Programming
## Programming Assignment 2 "Caching the Inverse of a Matrix"
## by Alexey Sobolev

## makeCacheMatrix() 
## 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL

## set() reads in the matrix, clears inverseMatrix for new data
  set <- function (y) {
    x <<- y
    inverseMatrix <<- NULL
  }

## get() returns the matrix
  get <- function() x

## setInverse() saves the passed inverse matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse

## getInverse() returns inverse matrix
  getInverse <- function() inverseMatrix

list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve()
## 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix(). It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Get the cached inverse matrix
  inverseMatrix <- x$getInverse()

  ## If the inverse has already been calculated  
  if(!is.null(inverseMatrix)) { 
    ## then return inverse matrix from the cache
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  ## Otherwise calculate and return inverse matrix
  inverseMatrix <- solve(x$get())
  inverseMatrix
}
