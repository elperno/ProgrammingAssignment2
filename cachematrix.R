## These functions, makeCacheMatrix and cacheSolve, will be used to 
## cache the value of an inverse matrix (which is typically an expensive 
## operation), so that in the case we need to use that inverse multiple 
## times, it will only be computed once, and later reused.

## The function makeCacheMatrix returns a list of four functions that 
## operate over a given matrix object, particularly setting and 
## getting its value and setting and getting the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initially, set object invx (inverse matrix) to NULL
	invx <- NULL
	
	## Function set: stores the value of a given matrix,
	## and sets its inverse (object invx) to NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
	
	## Function get: retrieves the cached value of a matrix
        get <- function() x
	
	## Function setsolve: computes the inverse of a matrix,
	## and stores it in object invx
        setsolve <- function(solve) invx <<- solve
	
	## Function getsolve: retrieves the cached value of a 
	## matrix, by returning the object invx that contains it
        getsolve <- function() invx
	
	## return an object that contains the four functions
        list(	set = set, 
		get = get,
             	setsolve = setsolve,
             	getsolve = getsolve)
}

## The function cacheSolve operates over the special object created
## with makeCacheMatrix (i.e., the list of four functions). It first
## checks whether the inverse matrix has already been computed and
## cached. If so, it returns the cached version of the matrix. Otherwise,
## it computes the inverse for the first time and caches its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invx <- x$getsolve()
	
	## Check if object invx is not empty (i.e. we have
	## already computed and cached the inverse of 'x')
        if(!is.null(invx)) {
            	message("getting cached inverse matrix")
		## if not empty, returned the cached inverse
            	return(invx)
	} 
	
	## Otherwise, get stored value of matrix and compute inverse	
	message("computing inverse")
        data <- x$get()
        invx <- solve(data, ...)
	
	## cache the inverse
        x$setsolve(invx)
	
	## and return it
        invx
}
