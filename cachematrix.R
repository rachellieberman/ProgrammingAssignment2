## This script consists of a pair of functions that cache the inverse of a matrix.
## Caching is used in order to avoid repeatedly compute the inverse, as this can be a 
##   computentionally intensive calculation


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

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


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getmatrix()
    if(!is.null(m)) {     ##If inverse has already been calculated, return cached value
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()     ##If inverse has not already been calculated, calculate inverse
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    

}
