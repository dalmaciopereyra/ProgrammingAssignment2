## This function calculates the inverse of a matrix

## makeCacheMatrix creates a matrix and caches it inverse, returns a matrix with functions to
## set and get values or set and get inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  #set the variable for the cached inversion of the matrix
  inv<-NULL
  
  #get and set matrix
  get<-function() x
  set<-function(y) {
    x<<-y
    inv<-NULL
  }
  # get and set matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  # return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)


}


## cacheSolve checks if the inverse of a matrix has been calculated, if it has it returns the inverse
## otherwise it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  
  
  #if it has been calculated return inverse
  if (!is.null(inv)) {
    message("inverse cached")
    return(inv)
  }
  
  #now we compute the inverse
  m<-x$get()
  inv<- solve(m, ...)
  
  x$setinv(inv)
  
  #return inverse
  return(inv)
}
