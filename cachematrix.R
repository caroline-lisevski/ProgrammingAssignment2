#creates a special object that can cache matrix inverse
#this function set the matrix, get the matrix, set the inverse and get the
#inverse; this is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()){
  
  #assumption: x is a square invertible matrix
  
  #initialize the inverse property
  inv <- NULL
  
  #this method sets the matrix (<<- used to assign a value to an object in an
  #different environment than the current one)
  set <- function(matrix){
    x <<- matrix
    inv <<- NULL
  }
  
  #this method gets the matrix
  get <- function(){
    #returns the matrix
    x
  }
  
  #this method sets the inverse
  setinv <- function(inverse){
    inv <<- inverse
  }
  
  #this method gets the inverse
  
  getinv <- function(){
    #returns the inverse
    inv
  }
  #returns a list of the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#this function compute the inverse of the matrix returned by "makeCacheMatrix"
#if the inverse has been calculated, then the cacheSolve returns the inverse
#from the cache
cacheSolve <- function(x, ...){
  
  #returns the inverse of x
  inv <- x$getinv()
  
  #if the inverse is already calculated:
  if(!is.null(inv)){
    #get from the cache and skips calculation
    message("Get from cached data")
    return(inv)
  }
  
  #or get the matrix...
  dat <- x$get()
  #... and then calculated the inverse
  inv <- solve(dat, ...)
  #sets the value of the inverse on the cache via the setinv function
  x$setinv(inv)
  #returns the matrix
  inv
}


