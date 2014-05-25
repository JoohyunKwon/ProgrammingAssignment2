## Following is overall explanation about two functions
## functions create the unique matrix for getting inverse matrix and store the inverse matrix.
## The main purpose of these functions are saving time-consumed computation.
## When we are getting inverse matrix of a large matrix (100000 x 100000), it may require time for your computer.
## Once you get the inverse of "x", you are gonna get the inverse easier than before!

## This function creats a special "matrix" object that can cache its inverse.
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse matrix (setInverse)
## get the value of the inverse matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) i <<- solve(x)
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## If the inverse of the matrix is stored, cacheSolve function immediately return the value with message
## If not, the function calculate the inverse of x and sets the value of the inverse matrix in the cache via setInverse function.

cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
  
      ## Return a matrix that is the inverse of 'x'
}
