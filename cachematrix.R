## Matrix inversion is a costly computation. The following two functions (makeCacheMatrix and cacheSolve)
## help compute matrix inversions more efficiently by caching the inverse of a matrix instead of computing 
## it repeatedly, and retrieve the inverse from the cache. Given a matrix x, its cached inverse can be
## computed using these two functions as cacheSolve(makeCacheMatrix(x)).

## makeCacheMatrix takes the original matrix as the input, and creates a special matrix object composed of
## four functions - get, set, getinv, and setinv that are useful for caching the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
		x <<-y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set,get = get,setinv = setinv,getinv = getinv
}


## cacheSolve computes the inverse of the special matrix object created by makeCacheMatrix. If the inverse is
## already present in the cache, this function would retrive the inverse from the cache. Otherwise, it computes
## the inverse, and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv

}
