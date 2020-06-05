## Put comments here that give an overall description of what your
## functions do

## The following function consists of a list that assigns the vector to get and set the value of the vector and the mean value consecutively.

makeCacheMatrix <- function(x = matrix()) { 
   inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks with the abovementioned criteria. But for not getting the mean it will give a message.

cacheSolve <- function(x, ...) {
     cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
