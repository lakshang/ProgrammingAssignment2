## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##following function stores a matrix and returns its inverse. makeCacheMatrix creates a special matrix which contains a list of functions such as set value to the matrix, get the value of the matrix, set value of the inverse matrix, and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <-function()x
  stinverse <- function(inverse) i <<- inverse
  gtinverse <- function()i
  list(set = set,
       get = get,
       stinverse = stinverse,
       gtinverse = gtinverse)
}


## Write a short comment describing this function
## below function computes the inverse of the special matrix returned from the above function makeCacheMatrix. In case the inverse is already calculated(where the matrix is not changed), then the below function retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$gtinverse()
  if(!is.null(i)){
    message("get cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$stinverse(i)
}


## testing the above function
C = matrix(c(1,2,3,4),2,2)
print(C)

makeC = makeCacheMatrix(C)
print(cacheSolve(makeC))
