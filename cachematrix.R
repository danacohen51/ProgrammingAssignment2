#The following functions cache in the inverse of a matrix.

#makeCacheMatrix is a function that creates a special matrix that is really a list containing a function to:
#set the value of the matrix,
#get the value of the matrix,
#set the value of the inverse,
#and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) x_inv <<- solve
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve is a function that calculates the inverse of the special matrix created with the function above.
#cacheSolve first checks to see if the inverse has already been calculated.
#If it has been calculted, cacheSolve gets the inverse from the cache.
#If the inverse has not already been calculated, cacheSolve sets the inverse of the matrix
#via the setinverse function.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
}