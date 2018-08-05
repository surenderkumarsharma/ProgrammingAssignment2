## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

## i am using package 'matlib' for inverse matrix

library(matlib)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    # variable for holding matrix which initialized with NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- inv(data)
  x$setinverse(inv)
  inv
  
}


# ## testing of these functions
# 
# x <-matrix( c(5, 1, 0,
#               3,-1, 2,
#               4, 0,-1), nrow=3, byrow=TRUE)
# 
# mat <- makeCacheMatrix(x)
# mat$get()
# 
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# 
# ## nothing find in cache
# cacheSolve(mat)
# 
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500
# 
# ## Second run and retrive from the cache
# cacheSolve(mat)
# 
# getting cached data.
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500