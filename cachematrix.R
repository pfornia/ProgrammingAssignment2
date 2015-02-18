##------------------------------------------------------------------------------------------------------------------
## Defines a makeCacheMatrix object, which is a matrix that stores its own inverse once that inverse has been
##	calculated by cachSolve(). cacheSolve returns the inverse of a makeCacheMatrix, only repeating the 
## 	calculation if necessary. 
## By rdpeng 2014-04-22
## Forked and modified by pfornia 2015-02-17
##------------------------------------------------------------------------------------------------------------------


## Special "Matrix" object that stores a matrix and that matrix's inverse, if one has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
	## 'x' is matrix
	## 'inverse' is inverse of 'x' if one has been calculated, else 'inverse' is NULL

	inverse <- NULL

	##get and set the matrix itself
	get <- function() x
	set <- function(xSet){
		x <<- xSet
		inverse <<- NULL
	}

	##get and set the inverse matrix
	getInverse <- function() inverse
	setInverse <- function(inverseSet) inverse <<- inverseSet

	## Return list of above functions
	list(get = get,
		set = set,
		getInverse = getInverse,
		setInverse = setInverse)
}


## Returns the inverse of a 'makeCacheMatrix' object. Only performs inverse calculation if one has not yet been
##	performed.

cacheSolve <- function(x, ...) {

	##if no inverse property of the matrix object exists, then one must be calculated.	
	if(is.null(x$getInverse())) {
		tempInverse <- solve(x$get(), ...)
		x$setInverse(tempInverse)
	}
	##else return the existing value
	else {
		message("getting cached inverse")
		tempInverse <- x$getInverse()
	}	

	## Return a matrix that is the inverse of 'x'.	
	tempInverse

}
