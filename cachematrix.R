## These functions are for Programming Assignment 2 for the coursera 
##  'R Programming' Course

## makeCacheMatrix will create a list object that can cache 
##  its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<-inverse 
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will either calculate the inverse and cache it or 
##  retrieve the inverse from the cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- ginv(data, ...)
    x$setinverse(inv)
    inv    
}
