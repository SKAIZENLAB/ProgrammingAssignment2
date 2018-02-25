## set of two functions illustrating the principles of lexical scoping, list object creation
## allowing access to defined in the environment of the original function
## application to the display of inverted matrix from cache in case of this inverted matrix has already been computed

## Create a CacheMatrix Object with setters and getter defined as functions
## cacheMatrix object persists out of the original function call due to lexical scope
## it has attributes (x and inv) that can be updated and accessed (get and set) from out of the original function

## makeCacheMatrix creates as a list a CacheMatrix object containing named getter and setter functions
## to get and set the value of the matrix and the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	## getter and setter  functions for the CacheMatrix Object
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x

	## getter and setter  functions for the inverted matrix
	setinv <- function(inverse) inv <<- inverse
	getinv <- function () inv

	## returns the new Object created as a list of named attributes
	## it should be noted that this object will keep access to x and inv
	## due to lexical scoping properties of R

	list (set=set, get=get,setinv=setinv, getinv=getinv)
}


## Assuming that the Matrix can be inverted
## Uses Solve function to calculate the interted matrix
## first sear in memory if the matrix invert has already been calculated
## 
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv ()

      ## Check in global environment if the inverted matrix has already been calculated
	## and is stored for the x matrix, then returns this inverted matrix without new calculation

	if (!is.null (inv)) {
		message ("getting cached data")
		return(inv)
	}

	## otherwise uses the solve function on the matrix and sets it in CacheMatrix object environment
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv (inv)

	inv
}

