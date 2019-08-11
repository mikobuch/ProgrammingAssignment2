## These functions store matrix and calculate its inverse matrix

## makeCacheMatrix creates an object that is able to store matrix and its inverse. 

makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL
  set <- function(mxy) {
    if(!identical(mxy,mx))
    {
      mx <<- mxy
      inv <<- NULL
    }
  }
  get <- function() mx
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates invetrse matrix . If it is already calculated, it reads it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating...")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
