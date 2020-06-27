## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix will take a matrix as the input,
## set a value of the matrix, get the value of the matrix, set
## the inverse of the inverse, and get the value of the inverse.

makeCacheMatrix <- function (x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

## The function cacheSolve will compute the inverse of the matrix
## returned by makeCacheMatrix. If the inverse has been calculated,
## cacheSolve will retirieve the inverse from the cache, as long
## as the matrix has not changed.

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
	inv <-x$getinv()
	if(!is.null(inv)) {
		message("getting cached result")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
