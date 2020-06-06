## This function takes an object as a matrix or numeric class and 
## save in the global enviorement its inverse form 
 
## This function take the input metione above and create a 
## list with the function for set the matrix, and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function take the list of the set-get function for the matrix
## and return the inverse value of our matrix

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
