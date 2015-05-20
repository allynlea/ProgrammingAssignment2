## makeCacheMatrix.R and cacheSolve.R
## Given a matrix, the 2 functions working together return the inverse
## of the matrix.  If the inverse of the matrix has been stored, it is
## not recalculated, but the saved inverse matrix is returned. If the
## inverse has not been cached, then the inverse matrix is calculated,
## returned, and stored.


## makeCacheMatrix() takes a matrix as input and returns a list
## of 4 functions:  get and set (input) a matrix; get and set (cache) the
## inverse of the matrix. The inverse matrix is cached by setting a global variable.

makeCacheMatrix <- function(mat = matrix()) {
                # argument inputs a matrix
        
                cached_inv <- NULL
                setmatrix <- function(y) {       # alternate way to input a matrix
                        mat <<- y
                        cached_inv <<- NULL
                }
                getmatrix <-function() {
                        mat                       # returns matrix
                }
                setinverse<- function(inverse) {
                        cached_inv <<- inverse    # caches inverse matrix
                }
                getinverse <- function() {
                        cached_inv                # returns cached inverse 
                                                  # might be NULL
                }
                
                list(setmatrix = setmatrix, getmatrix = getmatrix,
                     setinverse = setinverse, 
                     getinverse = getinverse)
}



## cacheSolve()  takes as input the output of makeCacheMatrix(), which is a 
## list of 4 functions. It returns the inverse of the original matrix,
## which may be a cached version or a calculated one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', the original matrix
       
                mat_inv <- x$getinverse()       # see if inverse is cached
                if (!is.null(mat_inv)) {
                        message("getting cached data")
                        return(mat_inv)
                }
                data <- x$getmatrix()           # if not cached, get the matrix      
                mat_inv <- solve(data, ...)     # invert the matrix
                x$setinverse(mat_inv)           # cache the inverse
                mat_inv                         # return inverse matrix
      
}
