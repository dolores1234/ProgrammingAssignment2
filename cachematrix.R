## Accepts square matrix and determines if the inverse already exists.
## If it does, returns the inverse matrix; if it does not, calculates the inverse matrix and returns it

## makeCacheMatrix: returns a list of functions to: 
## 1.  Set the value of the matrix
## 2.  Get the value of the matrix
## 3.  Set the value of the inverse
## 4.  Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # m will store the cached inverse matrix
  m <- NULL
  
  # setter for the matrix
  set <- function(y) {
    x <<- y   
    m <<- NULL
  }
  
  # Getter for the matrix
  get <- function( ) x
  
  # Setter for the inverse
  setinverse <- function(solve) m <<- solve
  # Getter for the invers
  getinverse <- function( )m
  
  # Return the matrix with newly defined functions 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: Computes the inverse of the matrix. If the inverse is already
## calculated, it returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # calculates the inverse
  matrix <- x$get()
  m <- solve(matrix)
  # caches the inverse
  x$setinverse(m)
  # returns the inverse
  m
}
