## These two functions together calculate and display the inverse of a matrix
## The first function, makeCacheMatrix, creates a matrix object that can cache
## the inverse of matrix
## The second function first checks to see if the inverse of the matrix has been
## calculated, if it has then it displays the inverse from the cache otherwise it
## computes the inverse of the matrix built by the first function using the 
## 'solve' function
## Written By: Mark Howard
## Date: 19 Aug 2015

## This function creates the matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function returns the inverse of a matrix. If the matrix has already been
## calculated then it is retrieved from cache and displayed, saving re-computation
## time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
