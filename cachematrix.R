## Matrix inversion is a costly computation. The following two functions (makeCacheMatrix and cacheSolve)
## help compute matrix inversions more efficiently by caching the inverse of a matrix instead of computing 
## it repeatedly, and retrieve the inverse from the cache. 

## makeCacheMatrix takes the original matrix as the input, and creates a special matrix object composed of 
## four functions - get, set, getinv, and setinv that are useful for caching the inverse. The get function
## displays the current matrix stored in the special matrix object. The set function can be used to modify
## the matrix stored in the special matrix object. The getinv function displays the inverse of the matrix
## stored in the special matrix object if the inverse is cached, otherwise it displays null. The setinv
## function is used to store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
		x <<-y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix object created by makeCacheMatrix. If the inverse is
## already present in the cache (using the getinv function), this function would retrive the inverse from the 
## cache. Otherwise, it computes the inverse (using the solve function), and stores it in the cache using the 
## setinv function.

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
