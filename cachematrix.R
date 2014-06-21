## Purpose of this code is to create a matrix and inverse it.
## If the matrix has been inversed already, it will give a "Getting Cached Data"
## Message.

##This function creates the matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL

##Here I am setting the matrix  
  set <- function(y) {
    x <<- y
    return(y)
    m <<- NULL
  }
## Now I get the matrix  

  get <- function() x
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Now test to see if inverse has been created.
##If created, I will return a "getting cached data" message.
##If not create, I use the "solve" function to inverse the matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}