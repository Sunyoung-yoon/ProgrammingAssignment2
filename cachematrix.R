## Put comments here that give an overall description of what your
## write 2 type of function named "makeCacheMatrix" and "cacheSolve"

## functions do
## Inverse of matrix data

## Write a short comment describing this function
## makeCachematrix : make special matrix data

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inverse<<-inverse
  getinverse <- function() inverse
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
## cacheSolve : inverse the input data

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting Cached Result")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

test_matrix <- matrix(rnorm(16),4,4)
test_matrix1 <- makeCacheMatrix(test_matrix)
cacheSolve(test_matrix1)
