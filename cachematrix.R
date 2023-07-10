## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function first initializes inv to NULL, inv will be used to store the inverse. the set function sets the value of the matrix
# and sets the inv to NULL. the get function returns the matrix m. The setInverse changes the inv to the inverse of the matrix. With
# the getInverse the inverse matrix is returned. the makeCacheMatrix returns a list of the 4 functions just discriped.

makeCacheMatrix <- function(m = matrix(1:9, nrow=3, ncol=3)) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# First the function try to get the inverse from the cache. Then the function checks if the inverse exists, if so, it returns the
# inverse from the cache. If the inverse does not exists in the cache, then it gets the original matrix and then it calculates the 
# inverse of the matrix and saves this in the cache and it returns the inverse as well. 

cacheSolve <- function(m, ...) {
    inv <- m$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setInverse(inv)
    inv
}
