## Below set of functions that cache the inverse of a matrix.
## Since Matrix inversion is usually a costly computation, there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL                      #Defining an inversion matrix variable with NULL value
        set <- function(matrix_input){                         
                x <<- matrix_input
                invmatrix <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inversionmatrix){
                invmatrix <<- inversionmatrix
        }
        getinv <- function() {
                invmatrix }
        
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the  object returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invmatrix <- x$getinv()    #Gets inverse from the special matrix object
        
        # Checks if invmatrix object is empty or not and returns it if not empty
        if(!is.null(invmatrix)) {
                message("getting cached inverse matrix")
                return(invmatrix)
        }
        
        # Gets the original matrix from special matrix object
        matrix <- x$get()
        # Computes inverse of the matrix
        invmatrix <- solve(matrix)
        # Sets the computed inverse into cache
        x$setinv(invmatrix)
        invmatrix             
}
