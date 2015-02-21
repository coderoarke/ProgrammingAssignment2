##    For the sake of simplicity and time, this script is similar to the assignment example.  This script creates two functions:

##    1) "makeCacheMatrix" allows the instantiation of a special matrix object that carries a matrix and its inverse.  The special  
##    matrix object can be read and set, and so can its inverse (stored in the "inv" variable).

makeCacheMatrix <- function(x = matrix()) {
      
            inv <- NULL
            set <- function(y) {
                  x <<- y
                  inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inv) inv <<- inv
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
            
}

##    2) "cacheSolve" takes a special matrix object defined in "makeCacheMatrix" and solves it using R's "solve()" function, returning 
##    its inverse.  If the special matrix object has already had its inverse calculated, the function will retrieve that object's inverse
##    value, print an indication to the console that it is doing so, and skip the unnecessary calculation.

cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      
}
