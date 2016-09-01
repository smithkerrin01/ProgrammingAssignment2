## The following functions can be used to create a matrix that can cache its inverse to help
## avoid the computational costs associated with repeated matrix inversion.

## The makeCacheMatrix function creates an S3 object that initializes a matrix and its inverse
## and provides associated setter and getter functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function accepts a makeCacheMatrix object and returns its inverse. If the
## inverse already exists, the cached inverse is returned; otherwise, the inverse is determined
## and the existing object is set to this new value.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
