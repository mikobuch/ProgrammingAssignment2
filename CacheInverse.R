makeCacheMatrix <- function(mx) {
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
cacheSolve <- function(x, ...) {
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
m<-matrix(c(8,13,5,8),2,2)
m1<-matrix(c(8,13,6,7,8,13,8,7,6),3,3)
cm <- makeCacheMatrix(m)
print(cm$get())
print(cacheSolve(cm))
print(cm$get())
print(cacheSolve(cm))

cm <- makeCacheMatrix(m1)
print(cm$get())
print(cacheSolve(cm))
print(cm$get())
print(cacheSolve(cm))

cm$set(m)

print(cm$get())
print(cacheSolve(cm))
print(cm$get())
print(cacheSolve(cm))
