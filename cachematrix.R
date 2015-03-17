## The program has two functions 1. makeCacheMatrix and 2. cacheSolve
## 1. makeCacheMatrix accepts a Square matrix that is nonsingular and returns a list
## 2. cacheSolve accepts the result of makeCacheMatrix and returns the inverse of matrix from the cache (if available)

## makeCacheMatrix receives a Nonsingular square matrix and returns a list containing
## 1. get and set functions for the matrix (getmatrix and setmatrix)
## 2. get and set functions for the inverse of the matrix (getinverse and setinverse)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = setmatrix, get = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve receives the result of makeCacheMatrix above and 
## returns the inverse of the matrix accepted by makeCacheMatrix.
## The inverse is calculated in this function if the inverse is not already calculated in makeCacheMatrix
## The inverse is obtained from the cache if the inverse is already calculted in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) ## Inverse of matrix available in the cache
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse ## Inverse of matrix calculted here        
}
