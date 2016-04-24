## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following two functions of makeCacheMatrix and cacheSolve create a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
		## Create a special "matrix", which is a list containing a function to
		## 1. set the value of the matrix
		## 2. get the value of the matrix
		## 3. set the value of the matrix inverse
		## 4. get the value of the matrix inverse
		## used as input of cacheSolve function
		inverse <- NULL
		set <- function(y) {
				x <<- y
				inverse <<- NULL
		}
		get <- function() x
		set_inverse <- function(inverse_value) inverse <<- inverse_value
		get_inverse <- function() inverse
		list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if (!is.null(inverse)) {
	        	message("getting cached data")
	        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}
