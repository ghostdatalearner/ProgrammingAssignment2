## Coursera R programming Assignment 2
## 
## This file contains the code that implements the two functions
## required for programming assignment 2
##
## Usage example:
##
## > A <- matrix(c(1,2,3,4),nrow=2,ncol=2)
## > resA<-makeCacheMatrix(A)
## > cacheSolve(resA)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(resA)
## Getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > A%*%cacheSolve(resA)
## Getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 
##
## May 2014
## Author: ghostdatalearner
## https://github.com/ghostdatalearner/ProgrammingAssignment2/cachematrix.R


## makeCacheMatrix creates a list containing four functions to 
## [[1]] Create the matrix object
## [[2]] Get the matrix object
## [[3]] Solve the inverse matrix
## [[4]] Get the value of the inverse matrix
##
## Input parameter: x, numeric matrix, mandatory
##
## Error checking: The function aborts if the input matrix x is not 
## square or if it is singular, as under those conditions is not 
## invertible, by definition
## Despite the problem description lets the programmer assume that the
## input matrix is invertible, this is an easy check that improves
## the user experience.

makeCacheMatrix <- function(x = matrix()) {
  # Check that the matrix is square and determinant nonzero, otherwise is not invertible
  if (nrow(x)!=ncol(x))
  {
    errmsg <- "Error: Matrix is not square and so not invertible"
    print(errmsg)
    return(NULL)
  }
  else if (is.infinite(determinant(x)[[1]][1]))
  {
    errmsg <- "Error: Matrix is singular (|x| is zero) and so not invertible"
    print(errmsg)
    return(NULL)
  }
  # After checking that the input matrix is invertible we set the 
  # inversematrix variable to null
  inversematrix <- NULL
  # Setting initial values
  setmatrix <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  getmatrix <- function() x
  setinversematrix <- function(solve) inversematrix <<- solve
  getinversematrix <- function() inversematrix
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

## caheSolve takes the four element list that returns makeCacheMatrix. If the inverse
## has not been computed it performs the inversion algorithm using the solve() function.
## If the inversion was computed previously and cached in inversematrix it returns that
## value.
##
## Input parameter: x, 4 elements vector, mandatory
##
## Error checking: The input parameter must not be null. It may be so if you try
## to use the result of an aborted makeCacheMatrix call.

cacheSolve <- function(x, ...) {
  ## Check that input parameter at least is not NULL
  if (length(x)==0 )
  {
    errmsg <- "Error: input parameter x must be a list of dimension 4"
    print(errmsg)
    return(NULL)
  }
  # We get the inversematrix value cached in the x object. If the function has
  # not be called previously it will be null.
  inversematrix <- x$getinversematrix()
  # If the value is set, the function returns it and ends
  if(!is.null(inversematrix)) {
    message("Getting cached data")
    return(inversematrix)
  }
  # Else it is the first invocation of the function. Inverse value must be computed
  data <- x$getmatrix()
  inversematrix <- solve(data, ...)
  # Here is where the inversematrix value is set for the first time
  x$setinversematrix(inversematrix)
  inversematrix
}
