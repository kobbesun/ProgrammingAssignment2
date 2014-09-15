## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        iMatrix <- NULL
        iMatrixInverse <- NULL
        setMatrix <- function(x) {
                if (!is.matrix(x)) {
                        message("SetMatrix variable isn't a matrix.")
                } else {
                        iMatrix <<- x;
                        iMatrixInverse <<- NULL
                }
        }
        getMatrix <- function() iMatrix
        setInverse <- function(i) {
                if (!is.matrix(i)) {
                        message("SetInverse variable isn't a matrix.")
                } else {
                        iMatrixInverse <<- i;
                }
        }
        getInverse <- function() iMatrixInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached invesed matrix data")
                return(inverse)
        }
        iMatrix <- x$getMatrix()
        inverse <- try(solve(iMatrix))
        if (class(inverse) == "try-error") {
                message("Matrix can't be solved.")
                x$setInverse(NULL)
        } else {
                x$setInverse(inverse)
                return(inverse)
        }
}
