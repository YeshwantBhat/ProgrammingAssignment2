## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
        x <<- y
        im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(length(x) == 0 || length(x$get()) == 0) {
          message("Either empty list or empty matrix")
          r<-NA
          return(r)
  }
  im<-x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <-x$get()
  im<-solve(data,...)
  x$setinverse(im)
  im
}
