## Since matrix inversion can be computationally extensive, we want to create a 
## function that can cache the inverse of a matrix. This will allow to avoid 
## repeated computation of the inverse of the particular matrix 
 
## makeCacheMatrix creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # initialization for the inversion
        set <- function(v) {
                x <<-v
                m <<-NULL
        }
        get <-function() x #this returns the matrix 
        setinverse <-function(inverse) m <<- inverse
        getinverse <-function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This cachesolve function computes the inverse of the special matrix returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dataframe <- x$get()
        m <- solve(dataframe, ...) # inversion of the matrix
        x$setinverse(m) 
        m
}


