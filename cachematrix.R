## These functions help create a special matrix whose inverse can be cached after computed once

## This function create a special matrix whose inverse can be cached later

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(val) inverse <<- val
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks if the inverse has been computed, the cached inverse will be returned
## if not, this function will compute the inverse and cache it so that it can be retrieved in the following calls without computing the inverse again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
