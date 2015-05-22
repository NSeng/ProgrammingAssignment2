makeCacheMatrix <- function(mx = matrix()) {
  #create s object with a NULL value, to hold 'solved' matrix
  s <- NULL
 
  #The 'set' function may not be needed here, as it is not called by cachesolve.
  #It has been included in case it is needed for the evalation of this assignment.
  set <- function(y) {
    mx <<- y
    s <<- NULL
  }
  
  #get will return the value of the matrix
  get <- function() {
    mx
  }
  
  #setinverse caches the solved matrix
  setinverse <- function(solve) {
    s <<- solve
  }
  
  #get inverse gets the solved matrix
  getinverse <- function() {
    s
  }
  
  #The output is a list of functions
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x,...) {
  #THe input (x) should be the variable to which the output from
  #makeCacheMatrix is stored
  
  #test to see if s is NULL. If it is not, dipslay a message
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #if s is NULL, the matrix is retrieved and solved. The output is cached to 
  #the s variable and the value of s is printed to the screen.
  else {
  data <- x$get()
  s <- solve (data,...)
  x$setinverse(s)
  s
  }
}