## Set the input x as a matrix
## Then, set the solved value, inverse, as null
## x and inv stored in enclosing environment. of the set, get ,setinv and getinv functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function(){inv}
    list(set=set, get=get, setinv = setinv, getinv=getinv)
}
## calculates the inverse of the matrix.
## prior to this, it checks if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}


