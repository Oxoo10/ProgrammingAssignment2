makeCacheMatrix <- function (x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvmat <- function (solve) i <<- solve
  getinvmat <- function() m
  list (set = set, get = get,
        setinvmat = setinvmat,
        getinvmat = getinvmat)
}

cacheSolve <- function(x, ...) {
  i <- x$getinvmat()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvmat(i)
  i
}

