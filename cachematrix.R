##These functions allow for custom matrix objects that can store their inverses in a cache so that the 
##computationally-intensive calculation need not be repeated.

##Creates a list of functions for the matrix able to set/return the matrix and set/return the inverse. 
##This list acts as a custom matrix object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


##After inputting a custom matrix object from makeCacheMatrix, this function checks and returns the inverse if it has 
##already been stored. If not, it calculates and stores it in the custom matrix object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
