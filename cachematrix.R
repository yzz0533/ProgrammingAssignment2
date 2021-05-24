## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  n <- NULL
  set_matrix <- function(y) {
        x <<- y
        n <<- NULL}


get_matrix <- function() x
  set_matrix_inv <- function(inv) n <<- inv
  get_matrix_inv <- function() n
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_matrix_inv = set_matrix_inv,
       get_matrix_inv = get_matrix_inv)}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 n <- x$get_matrix_inv()
  if (!is.null(n)) {
    message("taking data from Cache")
    return(n)}
 data <- x$get_matrix()
  n <- solve(data, ...)
  x$set_matrix_inv(n)
  n}


