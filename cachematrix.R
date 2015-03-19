## Cache the Inverse of a Matrix - 2 functions

## makeCacheMatrix function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix(), nrow=2, ncol=2) {
  i <- NULL                           
  set <- function(y) {          
    x <<- y                     
    i <<- NULL                
  }
  get <- function() x               
  setsolve <- function(solve) i <<- solve       
  
  getsolve <- function() i      # returns value i from the 
  
  list(set = set, get = get,     # lists out the values of the functions
       setsolve = setsolve,      # in the makeCacheMatrix frame
       getsolve = getsolve)
}


## cacheSolve function computes the inverse of the matrix;
## if the inverse has already been calculated, 
##   retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  i <- x$getsolve()             
  if(!is.null(i)) {                 
    message("getting cached data")      
    return(i)                 
  }
  data <- x$get()              
  i <- solve(data, ...)       
  x$setsolve(i)               
  i                                   
}

