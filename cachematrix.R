## These two functions collectively constitute a way of inverting a matrix and saving the inverse in cache. This to reduce the potential effort necessary to invert it in the future

## This function returns a list of functions to set and get a matrix as well as the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function (inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}



## This file returns the inverse of a matrix. If a cached version exists, it will return it. If not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
