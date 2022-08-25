## My first function creates a special matrix that can cache its inverse
## My second function computes the inverse of the special matrix

##My first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to

##1: sets the value of the matrix
##2: gets the value of the matrix
##3: sets the value of the inverse
##4: gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special matrix created with the above function. 
## It first checks to see if the inverse has already been computed. If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
	m
}
