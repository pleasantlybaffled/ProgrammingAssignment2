## makeCacheMatrix and cacheSolve allow you to calculate the inverse of
## a particular matrix, and store this inverse so it can be called in future
## without being recalculated.

## The makeCacheMatrix function takes a matrix and creates a list of 
## functions: 
## set() allows you to set the matrix makeCacheMatrix will use;
## get() returns the matrix in use;
## setinverse() can be used by cacheSolve to set the inverse for the matrix,
## thereby caching it;
## getinverse() returns the cached inverse.

makeCacheMatrix <- function(mymatrix = matrix()) {
	inverse <- NULL				
	set <- function(newmatrix){		
		mymatrix <<- newmatrix			## replaces matrix in use with newly specified matrix
		inverse <<- NULL				## resets inverse when the matrix is changed
	}
	get <- function() mymatrix
	setinverse <- function(solved) inverse <<- solved
	getinverse <- function() inverse
	list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
}


## The cacheSolve function takes the list produced by makeCacheMatrix and
## checks if it already contains the cached inverse of the matrix.
## If the inverse is already cached, it returns the inverse; otherwise, it
## calculates the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {			## x is the list from makeCacheMatrix
		inverse <- x$getinverse()		## gets inverse from makeCacheMatrix
		if(!is.null(inverse)){			## if inverse is not null (ie it's already been calculated)
				message("getting cached data")
				return(inverse)		## just returns cached inverse
		}						## otherwise
		data <- x$get()				## gets matrix from makeCacheMatrix
		inverse <- solve(data, ...)		## calculates the inverse
		x$setinverse(inverse)			## sets inverse in makeCacheMatrix so it is cached for later use
		inverse					## returns inverse
}        

