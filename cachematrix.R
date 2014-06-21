## makeCacheMatrix function caches the inverse of a given matrix.
## cacheSolve function computes the inverse of a given matrix.

## makeCacheMatrix accepts a matrix. Returns a list of functions.
## Through the list the function can be used to set, get the matrix.
## It can also be used to get, set the inverse of a given matrix.
## The inverse of the matrix is cached and returned if the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL                                  #The inverse matrix is set to NULL.
      set <- function(y) {
        x <<- y                                   #Matrix is assigned to x.
        im <<- NULL                               #Inverse of the matrix is set to NULL.
  }
  get <- function() x                             #Returns the matrix.
  setinverse <- function(inverse) im <<- inverse  #Caches the inverse of the matrix.
  getinverse <- function() im                     #Returns the cached inverse of the matrix.
  list(set = set, get = get,                      #The list is set to the various functions and returned.
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve accepts a list of functions. Returns the inverse of a matrix.
## It first gets the cached inverse matrix if it exists.If no cached inverse matrix exists
## then it computes the inverse of the matrix and returns the same.
## The function assumes the matrix to be square and invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(length(x) == 0 || length(x$get()) == 0) {   #Returns the null object for emply list or empty matrix.
          message("Either empty list or empty matrix")
          r<-NULL
          return(r)
  }
  im<-x$getinverse()                                   #Gets cached inverse of the matrix.
  if(!is.null(im)) {                                   #Return the cached inverse matrix if available.
    message("getting cached data")
    return(im)
  }
  data <-x$get()                                       #Gets the matrix
  im<-solve(data,...)                                  #Computes the inverse of the matrix.
  x$setinverse(im)                                     #Caches the inverse of the matrix
  im                                                   #Returns the inverse of the matrix.
}
