## Function makeCacheMatrix makes a "special" matrix with the ability to store its inverse. You can access the cached inverse by using $getInverse() on your "special" matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Using function cacheSolve checks if the inverse is already calculated and stored, otherwise it calculates the inverse (after checking if it is possible). If the inverse is already cached - it does not calculate it again - it returns it.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # in the above "if" we check if the matrix can be invertible - if it is square 
  # matrix of equal dimension greater than 1 (first condition),
  # or if it is just a single number (condition after ||)
  if ((class(data)=="matrix" && dim(data)[1]==dim(data)[2]) || length(data)==1){
    inverse <- solve(data)
  }else{ 
    message("Cannot compute inverse of matrix - invalid dimensions")}
  x$setInverse(inverse)
  inverse
}
