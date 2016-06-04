##creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
## i will store the inverse oject  
  i <- NULL
##set the function  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
##get the function  
  get <- function() x
##set i to inverse
  setinverse <- function(inverse) i <<- inverse
##get the value of i
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}
##compute inverse and checks with the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
##display message and will return the inverse of the matrix    
    message("getting cached data")
    return(i)
  }
#If not cached the solve it and then cache  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
