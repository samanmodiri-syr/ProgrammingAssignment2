## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
