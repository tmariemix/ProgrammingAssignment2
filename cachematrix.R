## Assignment: Caching the Inverse of a Matrix
##Taylors Work 7/23/2015

##Step 1
##makeCacheMatrix
## This function creates a special matrix object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}
##Step2
##cacheSolve
## This function computes the inverse of the matrix 
## makeCacheMatrix thats created above
##If the inverse has already been calculated then 
##cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

##This is how I checked to make sure that it worked correctly

##Example to Check
##x<-makeCacheMatrix(matrix(c(1,2,3,4), c(2,2)))
##cacheSolve(x)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
