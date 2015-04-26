##
## This file implements functions to compute the inverse of a matrix and cache
## the result. Calculating the inverse of the matrix is computationally
## intensive (especially for large matrices). Caching the inverse after the
## initial computation obviates the need to compute it again for the same
## matrix.

## makeCacheMatrix takes a matrix as its input and creates a list of
## four functions: get (which returns the matrix), set (which stores the
## matrix), setinv (which stores the inverse of the matrix) and getinv(which
## returns the stored value of the inverse)

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	get <- function() x
	set <- function(y) {
		x <<- y
		minv <<- NULL
	}
	setinv <- function(inverse) minv <<- inverse
	getinv <- function() minv
	list (get = get, set = set, setinv = setinv, getinv = getinv)
}


## The following function takes a "square" matrix as its input argument and calculates
## its inverse. It first checks if the inverse is already cached. If it is not
## cached it computes the inverse, stores (caches) the min and returns the 
## inverse. No checks are made to ascertain that the matrix is regular.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  minv <- x$getinv()
	  if (!is.null(minv)) {
		message("getting cached data")
		return (minv)
	  }
	  data <- x$get()
	  minv <- solve(data,...)
	  x$setinv(minv)
	  minv
}
