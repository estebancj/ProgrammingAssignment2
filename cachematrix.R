## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a MATRIX

## makeCacheMatrix: This function creates a special MATRIX object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  funcInv <- NULL
  set <- function(y) 
    {
      x <<- y
      funcInv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) funcInv <<- inverse
  getinverse <- function() funcInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special MATRIX returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

##Note: the matrix supplied must be invertible

cacheSolve <- function(x, ...) 
{
  funcInv <- x$getinverse()
  if(!is.null(funcInv)) 
  {
    message("getting cached data.")
    return(funcInv)
  }
  data <- x$get()
  funcInv <- solve(data)
  x$setinverse(funcInv)
  funcInv
}

##execution example
matrixSample<-matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)
##Check with other example, if you like :)
##matrixSample<-matrix(c(4,3,3,2),nrow=2,ncol=2)
cacheSample = makeCacheMatrix(matrixSample)
cacheSample$get()
cacheSolve(cacheSample)
##if the inverse is previously saved then you gonna get the cached computation
cacheSolve(cacheSample)
cacheSolve(cacheSample)