# R Programming Assignment 2
# Author : N Srikar Reddy

# the function makeCacheMatrix below creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(a24 = matrix()) 
{
inverse <- NULL
set <- function(y)
  {
a24 <<- y
    inverse <<- NULL
  }
  get <- function() a24
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}

# The function below computes the inverse of a special matrix returned by the 
# function above, if the inverse has already been calculated then the cacheSolve
# function retrieves the inverse from the cache

cacheSolve <- function(a24, ...) 
{
  #  To return a matrix that is the inverse of 'a24'
  inv <- a24$getinverse()
  if(!is.null(inv))
  {
    message("getting cached matrix")
    return(inv)
  }
  data <- a24$get()
  inv <- solve(data,...)
  a24$setinverse(inv)
  inv
}

