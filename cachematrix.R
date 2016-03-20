##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly Your assignment is to write a pair of functions that
## Those functions cache the inverse of a matrix

## Write a short comment describing this function
## The following function creates a Matrix which can cache ist inverse, by using a third variable.

makeCacheMatrix <- function(x = matrix()) {
	iMtx <- NULL
	
	set <- function(y) {
		x <<- y
		iMtx <<- NULL
	}
	
	get <- function() { 
		x
	}
	
	setInverse <- function(ivs) {
		iMtx <<- ivs
	}
	
	getInverse <- function() {
		iMtx
	}
	
	list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse)
}


## Write a short comment describing this function
## The following function, computes or return(if it's cached) the inverse of returned matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	iMtx <- x$getInverse()
	
	if (!is.null(iMtx)) {
		message("getting cached data")
		return (iMtx)
	}
	
	mtx <- x$get()
	iMtx <- solve(mtx, ...)
	x$setInverse(iMtx)
	iMtx		
}