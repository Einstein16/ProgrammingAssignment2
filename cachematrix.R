## This function creates a special matrix object that can cache its inverse.

# Defines the function to set the value of the matrix. It also clears the old
#inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m))
    message("getting cached data")
  return(m)
}
data <- x$get()
m <- solve(data) %*% data
x$setinverse(m)
m
}

cacheSolve <- function(x) {
  m <- x$getInverse() # This calls the cached value for the inverse
  if(!is.null(m)) { # If the cache was not empty, it will just return it
    message("getting cached data")
    return(m)
  }
  # If the cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()  # Get value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}
