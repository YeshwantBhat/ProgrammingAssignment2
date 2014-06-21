## makeCacheMatrix function caches the inverse of a given matrix.
## cacheSolve function computes the inverse of a given matrix.

## makeCacheMatrix accepts a matrix. Returns a list of functions.
## Through the list the function can be used to set, get the matrix.
## It can also be used to get, set the inverse of a given matrix.
## The inverse of the matrix is cached and returned if the matrix has not changed.

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


## cacheSolve accepts a list of functions. Returns the inverse of a matrix.
## It first gets the cached inverse matrix if it exists.If no cached inverse matrix exists
## then it computes the inverse of the matrix and returns the same.
## The function assumes the matrix to be square and invertible.

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
