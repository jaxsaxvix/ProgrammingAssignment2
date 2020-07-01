## Thiss function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
 set <- function(y){
   x <<- y
   i <<- NULL
 }
 get <- function() x
 setinversion <- function(inversion) i <<- inversion
 getinversion <- function() i
 list(set = set,
     get = get,
     setinversion = setinversion,
     getinversion = getinversion)
}


## When calling this function with the special "matrix" object created above (x), it first try to get its
## inversion from cached data. If it hasn't been solved yet, it solves it and stores it for future 
## scenarios where the inversion is needed.

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
    
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
       
}
