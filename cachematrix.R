## The functions in this code can cache the inverse of a matrix without any need to compute the inverse if it's already computed
## This type of functions help while dealing with large sizes of data

## This Function returns a special type of matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Setting the inverse to NULL
  m <- NULL
  
  ## Setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Getting the matrix
  get <- function() x
  
  ## Setting the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## Getting the inverse of the matrix
  getinverse <- function() m
  
  ## Returning the final list
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This Function retrieves the inverse from the cache if it has been already calculated and the matrix has not changed
## Else computes the inverse using solve

cacheSolve <- function(x, ...) {
  
  ## Inverse of Matrix x 
  m <- x$getinverse()
  
  ## Returning the inverse if it's already set
  if (!is.null(m)) {
    message("data is cached already")
    return(m)
  }
  
  ## Getting the mtrix
  data <- x$get()
  
  ## Inverse using matrix multiplication
  m <- solve(data, ...)
  
  ## Setting the inverse
  x$setinverse(m)
  
  ## Returning  matrix
  m
        ## Return a matrix that is the inverse of 'x'
}
