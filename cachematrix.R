## These functions will calculate the inverse of a square, invertible matrix, using solve.
## Once the inverse has been computed, it will be cached.
## If the inverse is to be re-computed with the same data, the cached value of the inverse is returned.
## This will save time as the matrix inverse need not be recalculated.

## makeCacheMatrix creates a list to do the following:
## 1. Set the the square invertible matrix
## 2. Get the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set x to be th square invertible matrix input
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the contents of the matrix
  get <- function() x
  
  ## Set the inverse of the matrix using solve
  setinv <- function(solve) inv <<- solve
  
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will compute the inverse of x. 
## If inverse is already computed for this matrix, return cached inverse

cacheSolve <- function(x, ...) {
  
  ## Check for cached data
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
