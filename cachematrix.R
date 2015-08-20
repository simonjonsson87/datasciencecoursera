## These two functions will be able to cache matricies and calculate the inverse of those matricies.

#
# This function creates a list which contains four functions for storing 
# and retrieving matricies and the inverse of those matricies.
#
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	#
	# We make the functions
	set <- function(y)
	{
		x <<- y
		m <- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	#
	# ...and we put the functions in a list, which we return
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#
# This function takes a matrix from a list created by makeCacheMatrix() and returns the inverse of 
# that matrix. If the inverse is already calculated, it returns the already existing inverse.
#
cacheSolve <- function(x, ...) {
        #
        # First, we check if there's already a inverse calculated, if there is, we return the 
        # value and exit the function.
        inv <- x$getInverse()
        if (!is.null(inv)) 
        {
        	return(inv)
        }
        #
        # We only get here if there's no inverse already calculated.
        # Hence, we get the matrix, use solve() to calculate, and return the result.
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
