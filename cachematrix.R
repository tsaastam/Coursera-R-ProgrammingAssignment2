## Use makeCacheMatrix() to put a given matrix in a cache.
## Use cacheSolve() to get a matrix inverse for a matrix in such a cache;
## either by computing & caching it or by getting an already-computed result
## from the cache.

## Example usage:
## 
## source("cachematrix.R")
## m <- makeCacheMatrix(matrix(nrow=3,ncol=3,data=c(1,2,3,4,5,-6,7,-8,9)))
## m$get()          # returns the matrix
## m$getInverse()   # returns NULL
## cacheSolve(m)    # computes, caches and returns the inverse
## m$getInverse()   # returns the inverse, which is now cached
## cacheSolve(m)    # returns the inverse from cache (with a message)
##
## m$set(matrix(nrow=3,ncol=3,data=c(-1,2,-3,4,5,6,-7,8,-9)))
## m$get()          # returns the new matrix
## m$getInverse()   # returns NULL, since the matrix was changed
## cacheSolve(m)    # computes, caches and returns the new inverse
## m$getInverse()   # returns the new now-cached inverse
## cacheSolve(m)    # returns the new inverse from cache (with a message)


## Creates a cache that can be used to store the given matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix        # note the <<-
        inverse <<- NULL    # on these two lines
    }
    get <- function() x
    setInverse <- function(inv) {
        inverse <<- inv     # note the <<-
    }
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## For the given matrix cache, gets its inverse, computing it with solve(x, ...).
## If the result is already in the cache, returns it directly.
## If the result is not in the cache, computes it and puts it in the cache, and
## returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(is.null(inv)) {
        # not computed yet, let's compute and save
        substance <- x$get()
        inv <- solve(substance, ...)
        x$setInverse(inv)
        inv
    } else {
        # already known
        message("yay, cached")
        inv
    }
}
