## Pair of functions to take a matrix and store it and its inverse
## in the parent/calling environment. If the inverse has already 
## been calculated, it will be pulled from cache for efficiency.

## Example syntax:  t <- makeCacheMatrix(m)
##                  cacheSolve(t)

## Returns a list of four functions in the parent/calling scope to
## set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_m <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_m <- function() x
  set_inv_m <- function(solve) inv <<- solve
  get_inv_m <- function() inv
  list(set_m = set_m, get_m = get_m,
       set_inv_m = set_inv_m,
       get_inv_m = get_inv_m)
}

## Using the matrix and functions defined in makeCacheMatrix, get the
## matrix inverse from cache if it exists, if not, calculate and set

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$get_inv_m()
  if(!is.null(invm)) {
    message("pulling from cache")
    return(invm)
  } 
  data <- x$get_m()
  invm <- solve(data, ...)
  x$set_inv_m(invm)
  invm
}