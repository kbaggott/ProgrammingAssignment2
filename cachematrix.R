## Functions "makeCacheMatrix" & "cacheSolve", combine to 
# invert any (invertible) matrix and cache the result for
# use in later querying.

## makeCacheMatrix, takes an (invertible) matrix
# and creates a list of functions for use in 
# converting and caching it's inverse with "cacheSolve"

makeCacheMatrix <- function(x = matrix()){
	b <- NULL
	set <- function(a) {
		b <<- a
	}
	get <- function() x
	setInverse <- function(inverse) b <<- inverse
	getInverse <- function() b
	list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}


## cacheSolve, take the object/function list from
# "makeCacheMatrix" and use "solve" to invert the matrix
# and store it for later use.

cacheSolve <- function(x, ...){
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}