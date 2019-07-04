## Put comments here that give an overall description of what your
## functions do
#
## This function make checks of the passed values
## if no values were passed then a new square matrix is created with random values
## at the end makeCacheMatrix function calls cacheSolve to show the inverse of matrix 'x'
#
makeCacheMatrix <- function(x = matrix()) { 
  if  (is.null(dim(x))) { 
    return(message(paste(as.character(x) , " <- It is not a Matrix")))}
  if ((is.null(x)) || (missing(x))){
    random <- sample(1:100,1)
    random <- round(sqrt(random))
    x <- matrix(sample(1:1000,random^2),random,random)
  } 
  cacheSolve(x)
}
#
## Write a short comment describing this function
#
## This function verify if "x" is a matrix and return the inverse of "x" matrix
## 
cacheSolve <- function(x = matrix()) {
  if((!missing(x)) && is.matrix(x) && (ncol(x) == nrow(x)) && (det(x) != 0))  {
    xi <- solve(x)
    xi
  } 
  else {
    message("matrix is invalid for the inverse operation")
  }
}


#### Validation
####  makeCacheMatrix(matrix(c(10,13),2,2))
####  makeCacheMatrix(matrix(c(12,1,4,9),2,2))
####  makeCacheMatrix(matrix(c(12,1,479),2,2))
####  makeCacheMatrix()