##  Matrix inversion : These two functions are used 
##  to cache the inverse of a matrix. 
 

makeCacheMatrix <- function(x) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}

## cacheSolve returns the inverse of the matrix. 
## Computes inverse only if it is not in cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}