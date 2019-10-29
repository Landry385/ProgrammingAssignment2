## THIS TWO FUNCTIONS (makeCacheMatrix & cacheSolve) create a special objet that 
## stores a matrix and caches its inverse


## This function "makeCacheMatrix" creates a special "matrix" in which, we will:

# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL             ## give a initial value to the inverse of the matrix 
  set <- function(y) {        ## set the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x         ## get the matrix
  
  Squarmat <- x                     ## set the square matrix "Squarmat"
  Inversemat <- solve(Squarmat)     ## get the inverse of this matrix
  
  setinverse <- function(Inversemat) inverse <<- Inversemat ## set the inverse of the matrix
  getinverse <- function() inverse                          ## get the inverse of the matrix
  
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)   

}


## This function 'cacheSolve' computes the inverse of the matrix created with
## the above function (makeCacheMatrix). First, this function checks to see if 
## the inverse of the matrix has already been calculated. If that's the case,
## the function 'cacheSolve' gets the inverse from the cache and skip the computation.
## If it's not the case, so this function computes the inverse of 'data' and sets
## the inverse calculated in the cache by using the setinverse() function. finally,
## 'cacheSolve' returns the inverse of the matrix 'x'.


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()           ## This function checks if the inverse has already calculated
  if(! is.null(inverse)) {            ## If so, it retrieves the inverse calculated
          message("getting cached data")  
          return(inverse)
  }
  
  data <- x$get()           ## If not, the function calculates the inverse
  squardata <- data
  inverse <- solve(squardata)
  x$setinverse(inverse)
  inverse                  ## Return a matrix that is the inverse of 'x'
}


## TEST OF makeCacheMatrix and cacheSolve

# Matrix order 2

makeCacheMatrix(x = matrix(1:4,2,2))
cacheSolve(makeCacheMatrix(x = matrix(1:4,2,2)))

makeCacheMatrix(x= matrix(c(2,4,3,5),2,2))
cacheSolve(makeCacheMatrix(x= matrix(c(2,4,3,5),2,2)))

makeCacheMatrix(x= matrix(c(2,-3,-4,1),2,2))
cacheSolve(makeCacheMatrix(x= matrix(c(2,-3,-4,1),2,2)))


# Matrix order 3

makeCacheMatrix(x = matrix(c(2,5,4,0,8,7,-1,1,3),3,3))
cacheSolve(makeCacheMatrix(x = matrix(c(2,5,4,0,8,7,-1,1,3),3,3)))

makeCacheMatrix(x = matrix(c(-1,1,-2,2,2,8,5,3,10),3,3))
cacheSolve(makeCacheMatrix(x = matrix(c(-1,1,-2,2,2,8,5,3,10),3,3)))

makeCacheMatrix(x = matrix(c(3,2,1,-2,-4,8,4,5,2),3,3))
cacheSolve(makeCacheMatrix(x = matrix(c(3,2,1,-2,-4,8,4,5,2),3,3)))





