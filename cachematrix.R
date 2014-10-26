## The first function looks to see if there already is a matrix and/or inverse already
## If not it returns NULL (for the get() and getinverse() subfunctions)
## The set() and setinverse() subfuctions are used to store a new matrix or its inverse
## The second function acutally calculates the inverse function. 

## This function stores the matrix and its inverse in memory.
## Using the <<- operator, the function sets the value of the matrix and its inverse in the parent environment
## When a new matrix (or inverse) is set using the set() (or setinverse) sub function, m is set to NULL
## this ensures that a new inverse matrix is first calculated using the cacheSolve function


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function finds the inverse of a given matrix, x.
## First the function checks to see if there already is a cached matrix m.
## It uses the solve() function to find the inverse.
## The product of a matrix and its inverse should give back the the identity matrix
## The diag() function gives out the Identity matrix.

cacheSolve <- function(x, ...) {
  m = x$getinverse
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m <- solve(data, diag(nrow(amatrix$get())), ...)
  x$setinverse(m)
  m
}
