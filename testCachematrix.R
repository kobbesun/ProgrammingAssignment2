## Test cachematrix.R

# create one solvable matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
solvableMatrix <- hilbert(8)

# create one unsolvale matrix
unsolvableMatrix <- matrix(c(1,2,3, 11,12,13, 21,22,23), nrow = 3, ncol = 3, 
                           byrow = TRUE, dimnames = list(c("row1", "row2", "row3"),
                           c("C.1", "C.2", "C.3")))

# import definition source file
source("cachematrix.R")

# test solvale matrix
m1 <- makeCacheMatrix()
m1$setMatrix(solvableMatrix)
m1$getMatrix()

# return inverse matrix by calculation
cacheSolve(m1)
# retrun invese matrix by retrieving cache
cacheSolve(m1)

# test unsolvable matrix
m2 <- makeCacheMatrix()
m2$setMatrix(unsolvableMatrix)
m2$getMatrix()

# return warning that the matrix is unsolvable
cacheSolve(m2)

## end
