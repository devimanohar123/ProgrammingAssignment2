## Since Matrix inversion is usually a costly computation, there will be
## benefit to caching the inverse of a matrix rather than computing it repeatedly.

## "makeCacheMatrix" & "cacheSolve" functions defined below help cache the inverse 
## of a matrix & avoids recomputing if need to reexecute for the same matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #Defining an inversion matrix variable with NULL value
        invmatrix <- NULL             
        
        #Set function that can be used to set input for the "matrix" object
        set <- function(matrix_input){                         
                x <<- matrix_input
                #Sets the inverse matrix to NUll if original matrix ix changed
                invmatrix <<- NULL    
        }
        
        #Get function that can be used to retrieve matrix
        get <- function() {
                x
        }
        
        #setinv function that caches the inversion matrix
        setinv <- function(inversionmatrix){
                invmatrix <<- inversionmatrix
        }
        #getinv function that can be used to retrieve inversion matrix
        getinv <- function() {
                invmatrix }
        
        #Returns list of functions for "matrix" objects
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the  object returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Gets inverse from the special matrix object
        invmatrix <- x$getinv()    
        
        #Checks if invmatrix object is empty or not and returns it if not empty
        if(!is.null(invmatrix)) {
                message("getting cached inverse matrix")
                return(invmatrix)
        }
        
        # Below code is executed if above "if" statement is false
        
        # Gets the original matrix from special matrix object
        matrix <- x$get()
        
        # Computes inverse of the matrix
        invmatrix <- solve(matrix)
        
        # Sets the computed inverse into cache
        x$setinv(invmatrix)
        
        #Returns inversion matrix
        invmatrix             
}
