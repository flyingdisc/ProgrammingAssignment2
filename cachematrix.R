## Lexical scoping, to save a cached inverse-matrix, 
##   to save computational time when an time-consuming inverse-matrix 
##   is already calculated previously, 
## instead of re-calculation the same operation. 

## makeCacheMatrix, is function to create a list to store functions, 
## 4 functions defined are, 
## - setMatrix, to set the matrix vector, 
## - getMatrix, to retrieve the matrix vector, 
## - setInverseMatrix, to set the cached inverse-matrix, 
## - getInverseMatrix, to retrieve the previously cached inverse-matrix, 

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(invmat) inv_matrix <<- invmat
  getInverseMatrix <- function() inv_matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve, is function to calculate the inverse-matrix, 
## firstly, it checks whether there exist a cached inverse-matrix, 
##    if so, it doesn't do calculation, instead it retrieve the cached value. 
## if the cached inverse-matrix doesn't exist yet, it calculates the value, 
##    then storing the value to the cache for future uses. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv_matrix <- x$getInverseMatrix() 
  if(!is.null(inv_matrix)) 
  {
    message("getting cached data") 
    return(inv_matrix) 
  }
  data<-x$getMatrix() 
  inv_matrix<-solve(data,...) 
  x$setInverseMatrix(inv_matrix) 
  
  inv_matrix 
}
