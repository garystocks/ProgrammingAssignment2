## The 2 functions create and then compute the inverse of a special 'matrix'.

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## i is my variable to contain the inverse of the matrix
  i <- NULL
  
    ## Create a variable to set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Create a variable to get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Compute the inverse of the special 'matrix' returned by makeCacheMatrix, 
## retrieving the inverse from the cache if it has already been calculated and
## the matrix has not changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve inverse
  m <- x$getinverse()
  
  ## Return the inverse if it exists !! CHECK IF MATRIX HASN'T CHANGED
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix and determine its inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
