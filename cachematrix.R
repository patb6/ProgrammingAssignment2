## Put comments here that give an overall description of what your
## functions do
## 
## CONTEXT
## The following Mentor site was extremely helpful in explaining the scope
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md)
##
## Since we can assume the matrix is always invertable I used diag() to create
## an Identiy matrix.
##
## makeCacheMatrix()  - creates a R Object that stores a Matrix and it's invers
## in the Parent environment for retrieval.
## cachesolve() - calculatest the inverse of a matrix.  If the inverse already
## exists then the cached matrix is retreived. makeCacheMatrix() must be used
## in conjuction with the function cachesolve(). 


## Write a short comment describing this function
# FUNCTION DESCRIPTION:
# makeCacheMatrix() function creates an R object that stores a matrix,
# the matrix inverse, and methods for storing and retreiving the matrix.
# Usage:
# cascheX <- makeCacheMatrix()  # An instance of makeCacheMatrix
# cascheX$get()                 # retreive matrix
# cascheX$getInv()              # retreive Inverse
# cascheX$set(diag(5,3,3))      # Store a Matrics and
#                                 Set current stored inverse to NULL


makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        # Initialize variables in Parent Environment
        set <- function(y) {
                x  <<- y
                ix <<- NULL
        }
        # Define get/set operations
        get <- function() x
        setInv <- function(slv) ix <<- slv
        getInv <- function() ix
        # Assign names to access function list elements
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## Write a short comment describing this function
# Description:
# cacheSolve() function creates an R object that calculates the inverse of a
# matrix stored in an instance of makeCacheMatrix. Rerunning cacheSolve()
# results in the 'cached' inverse being retrieved.
#
# Usage:
# Requires an instances of makeCacheMatrix() e.g. cascheX$set(diag(5,3,3))
# cacheSolve(cascheX)           # Solve & store inverse of stored matrix
#                               # Execute again returns "getting cached data"


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getInv()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setInv(ix)
        ix    
}
