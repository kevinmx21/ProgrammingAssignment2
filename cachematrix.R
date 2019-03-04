## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a set of functions (set, get,setsolve,getsolve) 
## and returns the functions in a list to the parent environment.


makeCacheMatrix <- function(x = matrix()) {
    ## initialize m
    m <- NULL
    
    ## defines set function
    ## Assign the input argument to x in the parent environment,
    ## Assign the value of NULL to m in the parent environment.
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## defines the getter function for x
    get <- function() x
    
    ## defines setter for the inverse m
    setsolve <- function(inverse) m <<- inverse
    
    ## defines getter for the inverse m
    getsolve <- function() m
    
    ## returns to the parent
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## retrieves the inverse matrix from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  ## retrieve an inverse from the object passed in as the argument. 
  m <- x$getsolve()
  
  ## checks if the result is NULL
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if m is null, cacheSolve() gets the vector from the input object, calculates a solve()
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
}