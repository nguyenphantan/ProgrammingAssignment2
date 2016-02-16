## These functions help create a special matrix whose inverse can be cached after computed once

## This function create a special matrix whose inverse can be cached later
## Input: a regular matrix (x)
## Output: the CachableMatrix (wrapper for matrix x)
makeCacheMatrix <- function(x = matrix()) {

  ## inverse matrix is initialized as NULL
  inverse <- NULL

  ## set the raw matrix of this CachableMatrix to a new value
  set <- function (y) {
    ## set the matrix
    x <<- y
    ## reset the cached inverse matrix to NULL
    inverse <<- NULL
  }

  ## get the raw matrix
  get <- function() x

  ## set the inverse matrix so that it can be retrieved later with out calculated again
  setInverse <- function(val) inverse <<- val

  ## get the current value of the inverse matrix
  getInverse <- function() inverse

  ## export a list of member functions to get or set the member variables of this CachableMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks if the inverse has been computed, the cached inverse will be returned
## if not, this function will compute the inverse and cache it so that it can be retrieved in the following calls without computing the inverse again
## Returns the inverse matrix. If the inverse matrix is retrieved from cache, an addition message will be printed out telling "getting cached data"

cacheSolve <- function(x, ...) {
  ## Try to get inverse matrix from cache
  inverse <- x$getInverse()
  
  ## check if the inverse matrix has been calculated before and cached
  if (!is.null(inverse)) {
    ## inverse matrix has been calculated before
    message("getting cached data")
    return (inverse)
  }
  
  ## inverse matrix has NOT been calculated before

  ## get matrix and calculate the inverse matrix
  data <- x$get()
  inverse <- solve(data)

  ## save the calculated inverse matrix to cache so that it can be retrieved later
  x$setInverse(inverse)

  ## return the inverse matrix
  inverse
}
