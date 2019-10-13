##------
#The first function, makeVector creates a special "vector", which is really a 
#list containing a function to
#
# vset the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

# If global and permanent assignments are intended within a function, 
# then either the “superassignment” operator, <<- or the function assign() 
# can be used. See the help document for details.

# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
#
# Initialize x & m
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(lset = set, lget = get,
             lsetmean = setmean,
             lgetmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



