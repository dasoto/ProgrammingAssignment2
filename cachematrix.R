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
## To check the Super Matrix is OK:
## > mySuperMatrix$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4


makeCacheMatrix <- function(x = matrix()) {
  ## When the SuperMatrix is created, we assign the value of his inverse to Null
  inv<-NULL
  
  ## Function 1.- to assign a new matrix to the SuperMatrix. Inverse is set to null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Function 2.- to get the matrix
  get <- function() x
  
  ## Function 3.- to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Function 4.- to get the inverse of the matrix
  getInverse <- function() inv
  
  ## Now we use the functional programming propierties that allow to create list of functions
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
