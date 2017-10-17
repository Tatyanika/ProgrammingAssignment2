## Functions computes and caches matrix invention
## 

## makeCacheMatrix creates a special vector, which is really a list containing a function to
## set and get matrix, set and get invention for this matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  setmatr <- function(mat) {
    x <<- mat
    i <<- NULL
  }
  
  getmatr <- function () x
  setinv <- function (inv) i <<- inv
  getinv <- function() i
  list(setmatr = setmatr, getmatr = getmatr,
       setinv = setinv,
       getinv = getinv)
  
}


## cachSolve calculates invention for matrix, or takes it from cache, if it's been calculated

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatr()
  i <- solve(data)
  x$setinv(i)
  i
}
