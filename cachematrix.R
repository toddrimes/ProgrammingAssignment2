## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix object
##        - first time, the cached matrix object is cleared and getmatrix will
##          return NULL
##        - after setmatrix, the cached matrix is returned
makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setmatrix <- function(x) matrix <<- solve(x)
        getmatrix <- function() matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve takes a makeCacheMatrix instance
##        - if cacheSolve is called with a new makeCacheMatrix, invoke setmatrix
##          on it and return the result
##       - subsequently, makeCacheMatrix$getmatrix returns the cached matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached already-inverted matrix")
                return(m)
        }
        data <- x$get()
        x$setmatrix(data)
        x$getmatrix()
}
