##-----
## Testing Matrix
## The following Mentor site was extremely helpful in explaining the scope
## (https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md)
## 
## Since we can assume the matrix is always invertable I used diag() to create
## an Identiy matrix.
#
# Usage:#
# cascheX <- makeCacheMatrix() 
# cascheX$get()                 # retreive matrix
# cascheX$getInv()              # retreive Inverse
# cascheX$set(diag(5,3,3))      # Resets current stored inverse to NULL
# cachesolve(cascheX)           # Solve & store inverse of stored matrix
#                               # Execute again returns "getting cached data"


makeCacheMatrix <- function(x = matrix()){
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




cachesolve <- function(x, ...){
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

