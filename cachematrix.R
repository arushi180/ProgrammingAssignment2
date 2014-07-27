## makeCacheMatrix creates a special 'matrix' containing a function to:
## get the value of matrix, set the value of matrix, set the value of the inverse and 
## get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix calculated by makeCacheMatrix.
## It checks to see if the inverse has been calaculated before, and if it has, then it
##gets the value from cache, otherwise it calculates the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
