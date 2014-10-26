## The functions in this file can be used to cache the inverse an input matrix

## Creates a list of four functions:
## set - sets the input matrix
## get - gets the input matrix
## setinv - sets the inverse of input matrix
## getinv - gets the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(s) i <<- s
	getinv <- function() i	
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)	
}


## Checks whether the inverse of input matrix is already in cache or not.
## If it is, then returns the cached inverse matrix otherwise computes the 
## inverse and return its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
}


}
