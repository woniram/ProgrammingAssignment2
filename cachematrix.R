## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## We have the function makeCacheMatrix that makes a special "matrix" object that can cache its inverse.
## We use the library(MASS) in order to get the inverse square matrices and also the non squuared
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                 ## setting inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x         ## to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x              ## to get the inverse of the matrix x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
       
}

## Write a short comment describing this function
## We use this function to obtain the cache data

cacheSolve <- function(x, ...) { ## to get cache data
  inv<-x$getinv()
  if(!is.null(inv)){             ## to check if inverse is NULL
    message("cahed data obtained")
    return(inv)                  ## returns the value of the inverse
  }
  data<-x$get()
  inv<-solve(data,...)           ## computes value of the inverse
  x$setinv(inv)
  inv                           ## returns a matrix which is the inverse of x
}
