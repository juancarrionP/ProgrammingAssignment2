
# Below are two functions that are used to create a special object
# that stores a numeric invertible matrix and caches its inverse.




# This first function, 'makeCacheMatrix' creates a special 'matrix', which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the inverse matrix
# 4.  get the inverse matrix

makeCacheMatrix <- function(x = matrix())
{
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}




# This second function calculates the inverse of the special 'matrix'
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it 'get's' the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse 
# and sets its value in the cache via the 'setInverse' function

cacheSolve <- function(x, ...)
{
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
