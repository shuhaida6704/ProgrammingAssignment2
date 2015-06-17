
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
##The assignment is to write a pair of functions that cache the inverse of a matrix.

##nameAuthor: Shuhaida Shuhidan
##todo::makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##todo::cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  ##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cacheSolve <- function(x, ...) {
  ##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  ## Return a matrix that is the inverse of 'x' 
  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

##--SAMPLE OUTPUT--
##try1<-makeCacheMatrix(x = matrix(1:4, 2))
##> try1$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> try1$set(matrix(1:4, 2))
##> try1$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(try1)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> try1$getmean()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> b<-try1$getmean()
##> try1$get() %*%b
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1