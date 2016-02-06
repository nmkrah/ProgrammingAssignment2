## R programming, Coursera, Assignment 2, February 2016

## This function creates a special matrix that caches its own inverse
## We use the "set" and "get" functions to create the object as a list

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## the function "cacheSolve" will calculate the inverse of a square matrix
## If the inverse of the starting function has already been calculated, it should
## still retrieve the cached result

cacheSolve <- function(x, ...) {
        ## To return a matrix that is the inverse of matrix x...
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("calculating inverse matrix")
                return(inv)
        }
        data <- x$get() 
        inv <- solve(data, ...) ##Calculate the inverse
        x$setInverse(inv)
        inv
}
##It works!

