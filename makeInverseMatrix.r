#initialize & populate a vector. this is the function we'll call when running this code
 makeCacheMatrix <- function(x = matrix()) {
     m <- NULL #initialize solve to 0
     
    set <- function(y) {
    x <<- y  #assign x's value to y
    m <<- NULL  #re-initialize solve to NULL
    }
    get <- function() x
       
    #get the inverse of the above function
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    #initialize into a list all 4 of the functions (defined above) which we'll use in cacheinverse
    list(set = set, get = get,
      setsolve = setsolve,
      getsolve = getsolve)
       }
 
cacheSolve <- function(x, ...) {
# get value of getinverse function from the makeVectorlist of functions
   m <- x$getsolve()
#check if solve is null; if not, return solve
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
#if solve is null, get the solve
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  }