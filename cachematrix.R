## In case of large inverse matrix we are facing time-consuming computations which
## appear especially in situations when we have huge dataset. By using following
## functions we can save plenty of time by caching values instead of recomputing
## them

## In this function we define makeCacheMatrix which creates a special "matrix"
## object. The goal is to cache its inverse 


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve    
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
    }


## Following function is called cacheSolve and its goal is to compute the inverse
## of the returned matrix above. In case the inverse has already been calculated 
## and it has not changed it should be retrieved from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    }
