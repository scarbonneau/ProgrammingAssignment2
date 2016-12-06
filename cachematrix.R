## As explained in the assignment
## Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than computing 
# it repeatedly. 

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  IX <- NULL
  set <- function(y) 
  {x <<- y
  IX <<- NULL}
  get <- function() x
  setinverse <- function(inverse) IX <<- inverse
  getinverse <- function() IX
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  IX <- x$getinverse()
  if(!is.null(IX)) {
    message("getting cached matrix")
    return(IX)
  }
  message("creating cached matrix")
  data <- x$get()
  IX <- solve(data, ...)
  x$setinverse(IX)
  IX
}
