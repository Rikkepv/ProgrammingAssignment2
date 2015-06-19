# Below are two functions which calculates and caches the inverse of a matrix respectively
# An example of how it works is provided below:
# > matrix <- makeCacheMatrix(matrix(c(0.5, 0, 0, 0.5), nrow=2, ncol=2))
# > cacheSolve(matrix)
# [,1] [,2]
# [1,]  2  0
# [2,]  0  2

#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of the matrix
#4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}


#The function `cacheSolve` calculates the inverse of the special "matrix"
#created with the function `makeCacheMatrix`. But before that it checks if the inverse
#has already been calculated. If the inverse has been calculated it gets the inverse from the 
#cache and skips the calculation. If it has not already been calculated, it 
#calculates the inverse of the matrix and sets the inverse of the matrix in the
#cache by the setinverse function. 


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix<- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
  
}




