## These functions create an object that allows the inverse of a matrix
## to be cached after calculation in order to avoid the need to perform
## a potentially costly calculation multiple times.


## 'makeCacheMatrix' creates a special matrix-like object that takes 
## in a matrix and returns a list of functions that will cache the 
## matrix's inverse.
makeCacheMatrix <- function(x=matrix()) {
	inv <- NULL
  	
	set <- function(y) {
    	x <<- y
    	inv <<- NULL
	}
  
  	get <- function() x
  	setInverse <- function(inverse) inv <<- inverse
  	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 'cacheSolve' takes an object created w/'makeCacheMatrix' and returns
## the inverse of the matrix by calling it from the cache if possible;
## if not, it calculates the inverse w/'solve' and caches the result.
cacheSolve <- function (x, ...) {
	inv <- x$getInverse()                 
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	tempMatrix <- x$get()
	inv <- solve(tempMatrix, ...)
	x$setInverse(inv)
	inv
}
