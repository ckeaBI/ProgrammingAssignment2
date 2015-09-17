## These functions are to fulfull the requirements of Programming in R
## Assignment 2. 

## Create a "matrix", That contains a function to 
##   - set value of matrix
##   - get value of matrix

  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
    )
  
}


## Calculate the inverse of matrix created in makeCacheMatrix
## function, the cached result is used if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("returning cached")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

