## This program is patterned after the makeVector R program to
## cache the inverse of a given matrix. The first time the call
## to make the inverse is made, the inverse is saved locally.
## On subsequent calls with the same input matrix to invert, the
## cached inverted matrix is returned. Sample calling sequence is 
## as follows:

## To create the cached matrix, start with
##	m <- makeCacheMatrix( )

## To tell the routine the matrix to invert use:
##	m$set( matrix(.....)) such as
##	m$set( matrix( c(0,2,2,0), 2,2))

## To see the matrix you loaded in, use:
##	m$get()

## To get the inverse matrix use:
##	cacheSolve(m)

## June 20, 2014. Bob G. with ennormous help from the Forum notes

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The section computes the inverse of the matrix the first
## time through, and then saves it for later use as described
## above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                ## message("Getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
