## The functions perform the following:
## makeCacheMatrix: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## 
## Function "makeVector" creates a special matrix that is a list contains a function to ## do the following:
## 1:   set the value of the matrix
## 2:   get the value of the matrix
## 3:   set the value of the Inverse of matrix
## 4:   get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
    	x <<- y
        m <<- NULL
}
	get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
    	setinv = setinv,
        getinv = getinv)
  
}
## This function computes the inverse of the matrix object returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	    	## Get Matrix "m" from Cache
    m <- x$getinv()             
    	## Check if not null, Retrun value read from Cache
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
	}
    	## if "m" is NULL, get origal Matirx
      mydata <- x$get()
      	## Calculate inverse using Solve
      m <- solve(mydata, ...)
      x$setinv(m)
       	## Returm inverse
      m
  
}