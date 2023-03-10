
  setwd('C:/Users/rubind1/Documents/Coursera-R')
  makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }
  cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting inversed matrix")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
  
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
  makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
   
          
