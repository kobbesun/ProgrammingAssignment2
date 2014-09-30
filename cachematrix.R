## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # define variables to store matrix and the inversed matrix
        iMatrix <- NULL
        iMatrixInverse <- NULL
        
        # define the set function that sets matrix - iMatrix
        setMatrix <- function(x) {
                # check whether the input parameter x is matrix
                if (!is.matrix(x)) {
                        message("SetMatrix variable isn't a matrix.")
                } else {
                        # assign x to iMatrix and reset iMatrix inverse
                        iMatrix <<- x;
                        iMatrixInverse <<- NULL
                }
        }
        
        setMatrix(x)
        
        # define get function that returns iMatrix
        getMatrix <- function() iMatrix
        
        # define set function that sets inversed matrix - iMatrixInverse
        setInverse <- function(i) {
                # check whether the input parameter i is matrix
                if (!is.matrix(i)) {
                        message("SetInverse variable isn't a matrix.")
                } else {
                        # assign x to iMatrixInverse
                        iMatrixInverse <<- i;
                }
        }
        
        inverse <- try(solve(iMatrix))
        if (class(inverse) == "try-error") {
                # if the stored matrix is unsolvable, notice the user.
                message("Matrix can't be solved.")
                # iMatrixInverse <<- NULL
                setInverse(NULL)
        } else {
                # if stored matrix is solvalbe, solve it and assign the 
                # inversed matrix to input parameter x
                setInverse(inverse)
        }
        
        # define get function that returns iMatrixInverse
        getInverse <- function() iMatrixInverse
        
        # return the list of four functions defined above
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # obtain stored inversed matrix from input parameter x
        inverse <- x$getInverse()
        # check if returned inversed matrix is null
        if(!is.null(inverse)) {
                message("getting cached invesed matrix data")
                # return the stored inversed matrix
                return(inverse)
        }
        
        # if no inversed matrix stored, obtain the matrix and solve it
        iMatrix <- x$getMatrix()
        inverse <- try(solve(iMatrix))
        # check if stored matrix is slovalbe
        if (class(inverse) == "try-error") {
                # if the stored matrix is unsolvable, notice the user.
                message("Matrix can't be solved.")
                x$setInverse(NULL)
        } else {
                # if stored matrix is solvalbe, solve it and assign the 
                # inversed matrix to input parameter x
                x$setInverse(inverse)
                # return the inversed matrix
                return(inverse)
        }
}
