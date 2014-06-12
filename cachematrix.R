## Identically to example of caching the mean of a vector
## these functions do constuct matrix wrapper which allows 
## to get cached version of a inverse matrix

## Makes a matrix wrapper for incapsulating its operations
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns either cached value of inversed matrix or evaluates it 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}