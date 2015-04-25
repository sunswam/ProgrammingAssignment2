# functions to return an inverse matrix from a cached object in R
# this demonstrates lexical scoping and caching functions


# The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# set the value of the matrix (set)
# get the value of the matrix (get)
# set the value of the inverse (setmat)
# get the value of the inverse (getmat)

makeCacheMatrix <- function(x = matrix()) {
  
  # Initially assigning 'NULL' to inverse
  m<-NULL
  set<-function(y){
    
    # Setting the matrix 'x'
    x<<-y
    m<<-NULL
  }
  
  # Returning matrix 'x'
  get<-function() x
  
  # Cache the value of the inverse
  setmat<-function(solve) m<<- solve
  
  # Returning inverse
  getmat<-function() m
  list(set=set, get=get,
       setmat=setmat,
       getmat=getmat)
}


#***********************************************************************************************************
# The following function calculates inverse of the "matrix" created in the above function.
# first checks to see if the inverse has been calculated avoiding the inverse calculation. 
#  Else, it calculates the inverse and sets the value of the inverse in cache thru the setmat function.

cacheSolve <- function(x=mat(), ...) {
  
  # Getting inverse
  m<-x$getmat()
  
  # Checking for the presence of inverse
  if(!is.null(m)){
    
    # Displaying message
    message("getting cached data")
    return(m)
  }
  
  # Getting Matrix
  mat<-x$get()
  
  # compute inverse
  m<-solve(mat, ...)
  
  # To cache the inverse
  x$setmat(m)
  
  #returns inverse
  m
}