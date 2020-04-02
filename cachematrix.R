## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function serves to:
## 1. Create a special "matrix" object
## 2. Store the value of the inverse matrix (if it is already calculated)
## 3. Get the value of the inverse matrix (if it is calculated and the matrix unchanged)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## This function serves to:
## 1. Check the cache if the value of the inverse is already calculated and get the value.
## 2. Calculate the inverse if the value is not already calculated and get the value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}