## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    if(nrow(y) == ncol(y) && det(y) != 0){
      x <<- y
      inverse <<- NULL
    }
    else 
      message("the input matrix is not invertible")
  }
  
  get <- function() x
  setInverse <- function() inverse <<- solve(x)
  getInverse <- function() inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## the function calculates the inverse of the matrix returned by above function
## if the inverse has been computerized, it retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if( !is.null(inv) ){.
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m,...)
  x$setInverse(inv)
  inv
}

