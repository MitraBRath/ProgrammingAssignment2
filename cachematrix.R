## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The below provided cacheSolve function calculates the inverse of the special matrix from makeCacheMatrix function "matrix" given above.
##But the cacheSolve function first finds if the matrix values have changed or same. If same then it gets the inverse of the matrix from cache, else 
##it recalculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("Retriving already cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
