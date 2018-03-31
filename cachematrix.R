## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
