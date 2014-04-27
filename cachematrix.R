## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here). 
## Those functions consist in a pair of functions that cache the inverse of a matrix.
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
##                     cache its inverse.
##
## 2. cacheSolve:      This function computes the inverse of the special "matrix"
##                     returned by makeCacheMatrix above. If the inverse has already 
##                     been calculated (and the matrix has not changed), then the 
##                     cachesolve should retrieve the inverse from the cache.

## Function : makeCacheMatrix <- function(x = matrix())
## This function allows to create an object that takes
## the input matrix 'x' and encapsulates
## - the input matrix 'x' as the current matrix
## - the cached inverse of the matrix 'x'
## - a list of accessors and modifiers methods
##  - get, set(x) : get/set the current matrix
##  - getInverse, setInverse(x) : get/set the inverse
##    of the current matrix
##
## Contract : 
## - the input matrix is supposed to be invertible
## - the cached inverse is either NULL or equals the current matrix
makeCacheMatrix <- function(x = matrix()) {
      ## The cached variable that contains the inverse of the current matrix 'x'
      inverted_matrix <- NULL
      
      ## Set modifier : set the current matrix 'x' with the matrix 'y' 
      ## and reset the current inverse to NULL
      set <- function(y) {
          x <<- y
          inverted_matrix <<- NULL
      }
      
      ## Get accessor : allow to get the current matrix
      get <- function() x
      
      ## Set the inverse of the current matrix and assign its value
      ## in a different environment so it emulates a type of caching
      setInverse <- function(inverse) inverted_matrix <<- inverse
      
      ## Get the inverse of the current matrix
      getInverse <- function() inverted_matrix
      
      ## List of functions interface
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Function : cacheSolve <- function(x, ...)
## This function allows to compute the inverse of
## the current matrix encapsulated in the input
## makeCacheMatrix object 'x'
## It returns the inverse matrix of the encapsulated
## matrix in 'x'
##
## The function returns the inverse attribute of the 'x'
## object if not NULL otherwise it computes the inverse,
## caches it via the setInverse method of 'x' and returns it
## Contract : 
## - the input object 'x' encapsulates a matrix 
##   that is supposed to be invertible
cacheSolve <- function(x, ...) {
  
      ## Return a matrix that is the inverse of the matrix 'x'
      inverse <- x$getInverse()
      
      ## Return the cached inverse if not NULL
      ## and exit the function
      if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
      }
  
      ## Otherwise we compute the inverse
      ### Get the current matrix
      data <- x$get()
      
      ### Compute the inverse
      inverse <- solve(data)
      
      ### Set the inverse
      x$setInverse(inverse)
      
      ### Return the computed inverse
      inverse
}
