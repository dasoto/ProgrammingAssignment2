## The followings functions represent the Assignment 2 of R Programming Course

## The first function called makeCacheMatrix will create a super Matrix that contain
## a list of functions to operate a given matrix.
## The list of functions are:
## 1. set() To set the matrix
## 2. get() To get the matrix
## 3. getInverse() To obtain the inverse of the matrix
## 4. setInverse() To set the inverse of the matrix
## 
## A simple example about how to create the SuperMatrix is:
## > myMatrix <- matrix(c(1,2,3,4), nrow=2,ncol=2)
## > mySuperMatrix <- makeCacheMatrix(myMatrix)


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve is a function that evaluate the inverse of the SuperMatrix
## In case the inverse has been already calculated, it use and return the cached Inverse
## If the inverse has not been assigned, the the inverse is calculated and assigned to 
## the given SuperMatrix.
##
## A basic example of use is:
##
## > cacheSolve(mySuperMatrix)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First we check if the inverse has been already calculated
  inv <- x$getInverse()  
  if(!is.null(inv)) {
    ## If the inverse exist it show a meesage and return the previously calculated inverse
    message("getting cached data")
    return(inv)
  }
  ## If the current inverse doesn't exist (null) the get the matrix and calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # and we assign the inverse to the SuperMatrix for future use.
  x$setInverse(inv)
  inv
}
