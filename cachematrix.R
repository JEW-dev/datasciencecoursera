## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {   ##set the value of the function to hold the inverse
    x <<- y
    inver <<- NULL
  }
  get <- function() x    ##this function is used to extract the inverse
  
  setinverse <- function(inverse) inver <<- inverse ## set the value of the inverse
  getinverse <- function() inver   ## get the value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## determines if the inverse is present then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- inverse(data, ...)
  x$setinver(inver)
  inver
  
  
  
}
