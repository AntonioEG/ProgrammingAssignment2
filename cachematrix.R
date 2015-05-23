## The following functions calculate, cache and return the inverse of a matrix.

## makeCacheMatrix creates creates a special "matrix", which is really a list 
## containing functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse of the matrix (setsolve)
## 4. get the value of the inverse of the matrix (getsolve)

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

## cacheSolve calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the mean from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}