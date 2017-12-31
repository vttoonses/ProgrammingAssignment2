
## Provide a cache'ing and inverse solving functions for matrices.

## Stores a matrix and its inverse (calculated externally).

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(newVal) {
    x <<- newVal
    i <<- NULL
  }

  get <- function() x

  setInverse <- function(inv) i <<- inv

  getInverse <- function() i

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns either the cached inverse value of the given matrix or solves
## and stores the matrix inverse and returns that value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()

  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

