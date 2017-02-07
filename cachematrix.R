## These two functions define a matrix which also contains a cache for its inverse
## and a inverse-calculating function that can read the inverse from that cache
## if present rather than wastefully recomputing it.

## Creates an environment containing a matrix x, an empty container m, and a list
## of commands to set and retrieve those variables.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function takes the 'x-environment' created by function makeCacheMatrix as
## an argument. It first fills a local i with the value of i stored in the loaded
## environment. If this already contains a value, it reports so and returns the
## stored value. If not it loads the x which stores the matrix to be inverted,
## inverts it, places the inverted matrix inside the x-environment, and returns it
## as the result of the function. As the i is now inside the x-environment it can
## be recalled by future calls of cacheSolve(x).

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
