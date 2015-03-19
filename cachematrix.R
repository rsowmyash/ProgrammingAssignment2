## Cache the Inverse of a Matrix - 2 functions

## makeCacheMatrix function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix(), nrow=2, ncol=2) {
  i <- NULL                           # initialize the inverse variable
  set <- function(y) {          # Y is the arg passed into the func
    x <<- y                       # set x for the env to y
    i <<- NULL                # set i for the env to null
  }
  get <- function() x               # create function in the parent
  setsolve <- function(solve) i <<- solve      # set solve value to value 
  # of i in the makeCacheMatrix frame
  getsolve <- function() i      # returns value i from the 
  # makeCacheMatrix frame
  list(set = set, get = get,     # lists out the values of the functions
       setsolve = setsolve,      # in the makeCacheMatrix frame
       getsolve = getsolve)
}


## cacheSolve function computes the inverse of the matrix;
## if the inverse has already been calculated, 
##   retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  i <- x$getsolve()              # goes to the x env and assigns the
  # i value from that env to this one
  if(!is.null(i)) {                  # if the x env has been evaluated before
    message("getting cached data")      #print the message and
    return(i)                  # the value of the cached inverse
  }
  data <- x$get()              # if never been evaluated, pull the
  # x matrix into data variable
  i <- solve(data, ...)       # calcuate the inverse
  x$setsolve(i)                 # assign the inverse to the x env
  i                                      # display the calculate inverse
}

