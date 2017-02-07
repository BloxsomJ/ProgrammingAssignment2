## These two functions define a matrix which also contains a cache for its inverse
## and a inverse-calculating function that can read the inverse from that cache
## if present rather than wastefully recomputing it.

## Creates an environment containing a matrix x, an empty container m, and a list
## of commands to set and retrieve those variables.

makeCacheMatrix <- function(mat = matrix()) {
  set_matrix <- function(y = matrix()) {
    mat <<- y #place y in the value of x outside the subfunction
    inverse <<- NULL #reset the cache when x changed
  }
  set_matrix(mat) #initialises the cache via the subfunction
  get_matrix <- function() mat #returns x stored in the function environment
  set_cache <- function(inv) inverse <<- inv #write to cache
  get_cache <- function() inverse #read from cache
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_cache = set_cache,
       get_cache = get_cache) #names the functions to enable calling with $
}

## This function takes the 'x-environment' created by function makeCacheMatrix as
## an argument. It first checks to see if the cache is empty. If so, it performs
## the inversion and returns the result. If the cache is occupied, it returns the
## contents of the cache.

cacheSolve <- function(mat) {
  if(is.null(mat$get_cache())) { #if cache is empty, determine and return inverse
    inverse <- solve(mat$get_matrix()) #retrieve and invert the matrix
    mat$set_cache(inverse) #write into cache
    return(inverse) #return inverse
  }#otherwise, return cached value
  message("reading from cache")
  return(mat$get_cache()) #return value from cache
}
