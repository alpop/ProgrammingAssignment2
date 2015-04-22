## Assignment: Caching the inverse of a matrix.
## by Alexander Popov (Github user alpop )

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {           
        inv <- NULL
        get <- function() x 
        getinv  <- function() inv 
        setinv  <- function(inverse) inv <<- inverse
        list(get = get, setinv = setinv, getinv = getinv)
}

## 'cacheSolve' computes the inverse of the matrix from 'makeCacheMatrix'. 
## If already calculated it will be retrieved from the cache.
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
