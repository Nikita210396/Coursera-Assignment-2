makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
  x <<- y
  a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

cacheSolve <- function(x, ...) {

  j <- x$getInverse()
  if(!is.null(a)){
  message("getting cached data")
  return(a)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(a)
  j
}