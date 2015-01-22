## The makeCacheMatrix function saves an existing matrix and its inverse.
## It is also able to return the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    setm <- function(n) { # Feed the matrix of interest into cache.
        x <<- n
        minv <<- NULL
    }
    getm <- function() x
    setinv <- function(inv = matrix()) {
        minv <<- inv
    }
    getinv <- function() minv
    list(setm = setm, getm = getm, setinv = setinv, getinv = getinv, getenv = getenv)
} # a list of methods from this function: sets and returns the matrix and its inverse

## The cacheSolve function calculates the inverse to a matrix if there's not already a computed inverse stored.
## If there is no computed inverse in the chache, the function returns the inverse of the matrix

cacheSolve <- function(x) {
    # Check cached value of inverse
    minv <- x$getinv()
    # If cache value is found, return that value along with a message.
    if(!is.null(minv)) {
        message("Getting cached data")
        return(minv)
    }
    # If cache value is not found, get cache value of matrix and solve for its inverse.
    m <- x$getm()
    minv <- solve(m)
    x$setinv(minv)
    minv
}
