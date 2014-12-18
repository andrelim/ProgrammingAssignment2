## Given a n x n matrix, we will create a special matrix that contain both itself 
## and its inverse matrix before retrieving it using another function

## makeCacheMatrix solves for the inverse of x before row binding it to create a
## 2n x n matrix which the first n rows is the original matrix and the next n
## rows is for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xInv <- solve(x) ##defines xInv as the inverse of x
        x <- rbind(x,xInv) ## row binds the inverse to the original matrix
}


## cacheSolve retrieves the inverse of portion of the matrix by determining how 
## many rows there are

cacheSolve <- function(x, ...) {
        nRows <- dim(x)[2] ##calculates number of rows (= columns) of inverse matrix
        xInv <- matrix(nrow=nRows, ncol=nRows) ##creates empty n by n matrix 
        for (i in 1:nRows){
                for (j in 1:nRows){
                        xInv[i,j] <- x[i+nRows,j]
                }
        } ##writes into the empty matrix
        xInv
        ## Return a matrix that is the inverse of 'x'
}
