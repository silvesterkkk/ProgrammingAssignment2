## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## A pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {   # define the argument with default mode of "matrix"
  inv <- NULL                                 # define inv as NULL; will hold value of matrix inverse
  set <- function(y) {                         # define the set function to assign new
    x <<- y                                   # value of matrix in parent environment
    inv <<- NULL                              # if there is a new matrix, reset inv to NULL
  }
  get <- function() x                         # define the get fucntion, returns the matrix argument
  setInverse <- function(inverse) inv <<- inverse   # define value of inv in parent environment
  getInverse <- function() inv                      # gets the value of inv where called
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv      
}


## TESTING

