## Functions to fetch int inverse of a Matrix
## check cache if the result already exists
## use cache if it does, else compute
## Samiran Roy 1/25/2014
## Cousera Programming Assignment2 - R programming

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get =get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Compute the inverse of Matrix returned by makeCacheMatrix above
## Return inverse from cache if already calculated and matrix hasn't changed
## Else compute

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
