## The function cache the square invertivle matrix and store its
## inverse for repeatative use

## this function create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv<<-inverse
	getInv <- function() inv
	list(set = set, get = get,
		setInv = setInv,
		getInv = getInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)){
        	message ("getting cache data")
        	return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
