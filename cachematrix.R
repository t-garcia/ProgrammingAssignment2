## Computes the inverse of a given matrix unless it was computed previously and stored in the cache.

## Creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Computes the inverse of the matrix object. If the inverse has been calculated, the inverse is retrieved from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

# -- Testing --
# 1: Creates a random 3x3 matrix
m0 <- replicate(3, rnorm(3))
# 2: Show the matrix
m0
# 3: Creates a special matrix w/ setters & getters
m1 <- makeCacheMatrix(m0)
# 4: Computes for the first time the inverse of the matrix, then shows the result.
cacheSolve(m1)
# 5: Second request to calculate de inverse. The result shown is recovered from the cache
cacheSolve(m1)
