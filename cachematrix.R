## Calculating the inverse of a matrix. Once the inverse is calculated it is cached 
## in order to avoid a recalculation of this inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  setinverse <- function(inverse) m <<- inverse
  get <- function() x
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache (provided the matrix is invertible).

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (dim(data)[1] != dim(data)[2]) {
    stop("The matrix is not square!")
  } else if (det(data) == 0) {
    stop("The matrix is not invertible!")
  } else {
    m <- solve(data, ...)
    x$setinverse(m)
  }
  m
}
