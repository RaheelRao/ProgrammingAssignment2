##MakeCacheMatrix##

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
