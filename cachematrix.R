## These functions allow the inverse of a matrix to be cached to save time 
## if it is called repeatedly
 

## The makeCacheMatrix function creates a custom matrix within which the inverse to 
## the input matrix can be stored.

makeCacheMatrix <- function(x = matrix()) {

    sol <- NULL
    set<-function(y){
      x<<-y
      sol<<-NULL
    }
    get<- function() x
    setInverse <- function(sol) s<<-sol
    getInverse <- function() sol
    list(set = set , get = get, 
         setInverse =setInverse, 
         getInverse=getInverse)
  
}

## The cacheSolve function makes use of the makeCacheMatrix function and does the calculation 
## of the inverse by calling the solve() function on it.

cacheSolve <- function(x, ...) {
  
  sol<-x$getInverse()
  if(!is.null(sol)){
    message("getting Cached data")
    return(sol)
  }  
  data <-x$get()
  sol<-solve(data)
  x$setInverse(sol)
  sol
  ## Return a matrix that is the inverse of 'x'
}
