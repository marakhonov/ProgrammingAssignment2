## This pair of functions caches the inverse of a matrix, because 
## matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m    
}
