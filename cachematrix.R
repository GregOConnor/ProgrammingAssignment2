## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##set the inv to NULL matrix
        inv <- NULL
        ##Set value of original matrix,reset inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##returns original matrix
        get <- function() x
        ##set the value of the inverse matrix 
        setInverse <- function(inverse) inv <<- inverse
        ##get the value of the inverse matrix
        getInverse <- function() inv
        ##Gives a list of the functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of the special matrix, 
##if already calculated returns the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## if matrix is not null, return the inverse
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##Else get the matrix and solve it
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}