## makeCacheMatrix() creates a special "matrix" object that can
## cache its inverse and cacheSolve() computes and returns the inverse 
## of the special "matrix" created by makeCacheMatrix()

## makeCacheMatrix() function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  inv <- NULL
  
  ## set(matrix) Function to "set" the matrix and inverse
  set <- function (matrix) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## get(matrix) Function to "get" the matrix 
  get <- function() {
    ## Return the "set" matrix
    m
  }

  ## setInverse() Function to "set" the inverse of the matrix
  setInverse <- function (inverse) {
    inv <<- inverse
  }
  
  ## getInverse() Function to "get" the inverse of the matrix
  getInverse <- function () {
    ## Return the Inverse matrix
    inv
  }
  
  ## Return the list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() function computes and returns the inverse of the
## special "matrix" returned by makeCacheMatrix() function.
## If the matrix is unchanged cachesolve returns the inverse of
## the special "matrix" from cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if already "set"
  if(!is.null(m)){
    print("Fetching Cached Data")
    return(m)
  }
  
  ## "get" the matrix from the object set
  dat <- x$get()
  
  ## Calculate the Inverse of the matrix
  m <- solve(dat) %*% dat
  
  ## Set the inverse of the matrix
  x$setInverse(m)
  
  ## Return the matrix
  m
}
