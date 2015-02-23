## Put comments here that give an overall description of what your
## functions do

## 1. Takes a matrix as an input
#  2. sets m equal to NULL
#  3. creates internal function 'get' which returns x when called
#  4. create internal function 'setinverse' which sets global variable m to
#		the value of solve if it doesn't already have a value
#  5. create internal function 'getinverse', which returns the value of m
#  6. creates a list that stores the values for set, get, setinverse, and
#		getinverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function
#	1. takes x(which is likely the outcome of calling makeCacheMatrix on some
#	value) and other values as arguments.
#	2. sets m equal to the getinverse() value of x
#	3. if m is not null, then the function returns a message confirming that
#		there is already a value assigned to m.
#	4. sets the get() value of x to the variable 'data'
#	5. sets the value of the solve function called on data to the m variable
#	6. calls the setinverse value of x with m as it's argument
#	7. returns m, which is the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
