## The 2 functions create and then compute the inverse of a special 'matrix'.

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## i is my variable to contain the inverse of the matrix
  i <- NULL
  
  ## Create a function to set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Create a function to return the value of the matrix
  get <- function() x
  
  ## Create a function to set the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Create a function to return the inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Compute the inverse of the special 'matrix' returned by makeCacheMatrix, 
## retrieving the inverse from the cache if it has already been calculated and
## the matrix has not changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve inverse
  i <- x$getinverse()
  
  ## Return the inverse if it exists
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix and determine its inverse
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i

}
