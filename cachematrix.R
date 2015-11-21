## The two functions below take a (square and invertible) matrix and see if 
## its inverse have already been calculated; if so, they return the cached 
## inverse; otherwise, they calculate the inverse of the given matrix and 
## cache the result.

# The first function, makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix, 
# 2. get the value of the matrix, 
# 3. set the value of inverse of the matrix,  and
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (   setmatrix = setmatrix
         , getmatrix = getmatrix
         , setinverse = setinverse
         , getinverse = getinverse)
}

# The following function calculates inverse of the matrix created with the 
# above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse of the matrix from the cache and
# skips the computation. Otherwise, it calculates the inverse of the matrix 
# and sets the value of the inverse of the matrix in the cache via the 
# setinversefunction.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}