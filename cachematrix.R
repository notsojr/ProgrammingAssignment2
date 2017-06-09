

## The following function creates a special object that can inverse a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set = function(y) {
      x <<- y
      i <<- NULL
    }
    
    get = function() x
    setinverse = function(inverse) i<<- inverse
    getinverse = function() i
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function checks to see if a cached value is already available.
## If that's not available, it runs the inverse function. 

cacheSolve <- function(x, ...) {
        i = x$getinv()
          if (!is.null(i)) {
            message("getting cached data")
            return(i)
          }
            
    matrix.data = x$get()
    i = solve(matrix.data,...)
    x$setinverse(i)
    i
      
}