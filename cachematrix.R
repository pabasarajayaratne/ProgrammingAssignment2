## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #store a matrix
  set <- function(y){
    x <<- y
    #since a new value is assigned to the matrix, inv should flush
    inv <<- NULL
  }
  
  #return the stored matrix
  get <- function(){ 
    x
  }  
  
  #cache the given argument
  setInverse <- function(solveMatrix) {
    inv <<- solveMatrix
  }
  
  #get the cached value
  getInverse <- function(){ 
    inv
  }
  
  #return a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the cached value
  inv <- x$getInverse()
  
  #to check whether a cache value exists, if so return it
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #get the matrix and calculate and store it in the cache
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
  #return the inverse
  inv      
}
