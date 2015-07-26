## These are functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL
set <- function(y) {
x <<- y
inver <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inver
list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inver <- x$getinv()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinv(inver)
    inver
}

