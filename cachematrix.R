## makeCacheMatrix(x)
## Purpose: a constructor of "cached" matrices
## Parameters: x is an optional matrix definition
## Output: "cached" matrix
## Usage example:
## 1) m <- makeCacheMatrix() to instantiate an empty matrix 
## or 
## 2) m <- makeCacheMatrix(matrix(...))) to instatiate with a pre-defined matrix
## 
## For either the 1) or 2) case you can always (re-)define the cached matrix by using the "set" method:
## m$set(matrix(...))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x, ...)
## Purpose: a solver of "cached" matrices. The first time its called it solves the inverse of the matrix, in subsequent calls a cached result is returned.
## Parameters:  x is a "cached" matrix and "..." are additional arguments that can be passed to the "solve" method.
## Output: inverse of a matrix
## Usage example:
## result <- cacheSolve(m) where m is "cached" matrix instatiated with the method "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
