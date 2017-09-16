## Programming Assignment 2: Lexical Scoping - R Programming 
## Assignment: Caching the Inverse of a Matrixless 
## Data Science Specialization Track
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## The purpose of the assignment is to write a pair of functions
## that cache the inverse of a matrix.

## For this purpose, the following functions were created:
## 1. makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this project, we assume that the matrix supplied is always invertible.


## The first function, makeCacheMatrix creates a special matrix, 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  
  inverse1 <- NULL
  set <- function(y) {
    ## <<- operator which can be used to assign a value to an object in an environment 
    ## that is different from the current environment
    x <<- y
    inverse1 <<- NULL
  }
  get <- function() x
  setinverse <- function(INV) inverse1 <<- INV
  getinverse <- function() inverse1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {

  inverse1 <- x$getinverse()
  if(!is.null(inverse1)) {
    message("getting cached matrix ")
    return(inverse1)
  }
  data <- x$get()
  inverse1 <- solve(data, ...)
  x$setinverse(inverse1)
  inverse1
}
