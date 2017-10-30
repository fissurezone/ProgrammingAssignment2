# Matrix Inverse Memoisation
#
# Use a cached inverse of a matrix to avoid 
# recalculation of inverse matrix at every 
# invocation of solve(<matrix>)

# Use wrapper functions around matrix instance 
# to store it along with its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# Cache inverse from first invocation and use it 
# for subsequent invocations
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
