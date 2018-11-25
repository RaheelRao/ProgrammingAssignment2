##MakeCacheMatrix##
## "makeCacheMatrix" function takes a matrix class variable x as input
## and returns in the output a list containing functions to set the 
## value of the matrix, get the value of the matrix, set the value of 
## the inverse, get the value of the inverse

makeCacheMatrix <- function(m=matrix())
{
  i<-NULL
  set <- function(matrix)
  {
    m <<- matrix
    i <<- NULL
  }
  get <- function()
  {
   ##Returning MATRIX
     m 
  }
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  # get inverse      
  getinverse <- function()
  {
    i
  }
  # LIST of Methods
  list(set=set,get=get,setInverse=setInverse,getinverse=getinverse)
}

##CacheSolve##
## "cacheSolve" function calculates the inverse of the matrix 
## created with the above function. However, it first checks to
## see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setinv function.

cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  #checking 'm'
  if(!is.null(m))     
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
