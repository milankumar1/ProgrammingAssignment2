## raher than computing the inverse of a matrix over and over again, 
## its much easier to cache the inverse of a matrix to skip the lengthy
## computation process. The next two functions are used to cache the inverse
## of invertible matrices.

## makeCacheMatrix makes a list of functions so you can:
#1) set the value of the matrix using the 'set' function 
#2) get the value of the matrix using the 'get' function
#3) set the value of the inverse of the matrix using the 'getinverse' function
#4) get the value of the inverse of the matrix using the 'getinverse' function.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## this function returns the inverse of the matrix. by using the 'if' statement
## it checks to see if the inverse has already been calculated.If so, the returns 
## the result o the prior computation. however, if the inverse has not benn 
## computed, it computes the inverse, caches the return value, and then returns 
## the value of the inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data.")
    
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv 
  
}
