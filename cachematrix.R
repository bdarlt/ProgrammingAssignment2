# create a list to fake a matrix class.
# get/set functions access the matrix
# setInverse/getInverse access the inverse
# square is a boolean value TRUE if the number
# of rows is iqual to the number of columns
### is that a bad OO tecnique? Yes. But this is a Q&D
### class. This whole file should really be in its own namespace.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  size<-dim(x)
  # they say I don't have to handle non square matrix
  # but I don't care. Handled!
  square <- size[1]==size[2]
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       square = square)
}

# solve the inverse of matrix in cacheMatrix object
cacheSolve <- function(x, ...) {
  # we want some primitive check to see if we have an object that
  # we should try to work with
  # I know this isn't perfect, better than nothing (I hope)
  if (!identical(names(x), c("set","get", "setInverse", "getInverse", "square") )) {
    stop("cacheSolve: not passed cacheMatrix object")
  }
  
  if (!x$square){
    stop("cacheSolve: Only square matrixes work with solve()")
  }
  # retrieve the inverse from the cacheMatrix object
  # the matrix inverse is primed to NULL
  # We don't calculate the inverse until expressly asked
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    # they didn't say we couldn't call message
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get(), ...)
  
  x$setInverse(inverse)
  inverse
}

# I hate making test data, so this does it for me
# A matrix return. row, col, max int value
createRandomMatrix <- function(nrow=10,ncol=10, maxVal=15) {
  matrix(sample.int(maxVal, size = nrow*ncol, replace = TRUE), nrow = nrow, ncol = ncol)
}
