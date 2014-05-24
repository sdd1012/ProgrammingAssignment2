## This file contains 2 functions, makeCacheMatrix and cacheSolve, that
## work in conjuction to compute the inverse of an invertible matrix
## and store the computed state for recurring need


## The makeCacheMatrix creates a special "matrix" object, which is a list, 
## from matrix 'x' as argument and can cache the inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                # reset inverse as special matrix has changed
                inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The cacheSolve returns the inverse of the special matrix 'x' 
## from its cache or after computing when not in cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(is.matrix(inv)) {
                #debug message below, used for testing
                #message("getting cached data")
                
                return(inv)
        }
        data <- x$get()
        # Create an identity matrix b of same dimensions and
        # pass it to 'solve' function to compute ONLY inverse
        b <- diag(nrow(data))
        inv <- solve(data, b, ...)
        
        x$setinverse(inv)
        inv
}
