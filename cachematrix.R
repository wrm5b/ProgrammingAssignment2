## These functions cache the inverse of a  matrix.

## makeCacheMatrix creates a special "matrix," really a list
## containing a function that can cache its inverse by:
## 1. setting the value of the matrix,
## 2. getting the value of the matrix,
## 3. setting the value of the inverse, and
## 4. getting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve returns the inverse of the matrix. First, it checks
## if the inverse has already been computed. If so, it gets the
## inverse from the cache and skips the computation. If not, it
## computes the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse fuction.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
