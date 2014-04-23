## Function to create a special matrix vector which 
## returns a list of 4 functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse to NULL
    inverse <- NULL

    # 1) set the value of the matrix
    set <- function(y) {
        x <<- y
        # NULL inverse variable since matrix is new
        inverse <<- NULL
    }
    
    # 2) get the value of the matrix
    get <- function() x
    
    # 3) set the value of the inverse
    setInverse <- function(inverse) inverse <<- inverse
    
    # 4) get the value of the inverse
    getInverse <- function() inverse

    # return a vector of functions 1-4
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function to calculate the inverse of the special matrix
## created with the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
    # first check to see if the inverse has already been calculated
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # since inverse not cached, read matrix into local variable
    data <- x$get()
    # compute inverse of matrix
    inverse <- solve(data, ...)
    # cache the inverse for future lookups
    x$setInverse(inverse)
    # return the value of matrix inverse
    inverse
}

