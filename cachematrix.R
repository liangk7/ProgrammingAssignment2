## [Put comments here that give an overall description of what your
## functions do]

## [Write a short comment describing this function]
## The following function creates a special matrix that can retain
## its inverse in cache
## Variables
## 'x' as the matrix, 'xinv' as the matrix inverse
## Functions
## 'set', 'get' to set and get value of x
## 'setinv', 'getinv' to set and get value of xinv
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
    	x <<- y
    	xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
     	setinv = setinv,
     	getinv = getinv)
}

## [Write a short comment describing this function]
## The following function allows one to pull the inverse of a
## matrix (whether it already exists in cache or not)
## Variables
## 'x' as the matrix object, 'xinv' as the matrix inverse
## 'data' as the matrix
cacheinv <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)){
    		message("getting cached data")
    		return(xinv)
    }
    data <- x$get()
    xinv <- solve(data,...)
    x$setinv(xinv)
    xinv
}
