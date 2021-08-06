## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache the matrix and its inverse.
## It consists of four functions, get and set matrix, and get and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  matrix_set <- function(y){
    x<<-y
    inverse <<- NULL
  }
  matrix_get <- function() x
  inverse_set <- function(inv) inverse <<- inv
  inverse_get <- function() inverse
  list(matrix_set=matrix_set,matrix_get=matrix_get,inverse_set=inverse_set,inverse_get=inverse_get)
}


## This function caches (if not already) and returns the inverse of a matrix.
## It first checks if the matrix inverse is chashed. If not, it calculates the inverse and caches it.
## and in either situation, it returns the inverse as an output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$inverse_get()
  if(!is.null(inv)){
    return(inv)
    message("getting cached data")
  }
  matrix <- x$matrix_get()
  inv <- solve(matrix)
  x$inverse_set(inv)
  inv
}
