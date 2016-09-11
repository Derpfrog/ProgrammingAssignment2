## chachematrix - Assignment - by: derpfrog 9/10/2016
## Below functions will create a special "matrix" object that will chache
## the input matrix's inverse.  
## Functions assume that the matrix provided is invertable.

## This fucntion will create a"matrix" object that when run through the cacheSolve
## function will return the inverse of the matrix or the cached inverse of the matrix. 
##  depending if the inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                              
    get <-function() x
    setinverse <- function(inversemat) inv <<- inversemat
    getinverse <- function() inv
    list(get = get, setinverse = setinverse, getinverse = getinverse)

}


## The following function will check to see if there the inverse matrix has been previously calculated.
## If not it will calculate the inverse of the matrix and store the inverse into cache for faster recall
## at a later date.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Using cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
