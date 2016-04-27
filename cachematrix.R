## Author: Lee Hanxue
## Email: leehanxue@gmail.com
## These 2 functions will solve (inverse) a Matrix value, 
## and store the matrix in the environment. Every time the 
## cacheSolve() function is called, it will use the 
## cached value if exist. 

## makeCacheMatrix() returns a list of functions that 
## can be applied to the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve() matrix will first look for an 
## existing inversed matrix, and return it if exist. 
## If no cached matrix exist, it will call solve() 
## to inverse the matrix, and store the result by 
## calling setInverse()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached inversed matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
