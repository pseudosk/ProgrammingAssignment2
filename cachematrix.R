## Put comments here that give an overall description of what your
## functions do

## These functions are to inverse a matrix and cache it. 
## Returns cached inverse matrix if already cached else computes the inverse and returns

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## Returns cached inverse matrix if already available in cache 
## else computes inverse matrix and returns 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
