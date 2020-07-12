## These two functions retrieve the cache value for a inverse of a matrix

## makeCacheMatrix defines the getters and setters in the fuction makeCacheMatrix
##inv defines the variable carrying the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
            set <- function(y) {
                x <<- y
                inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve defines the computation of the inverse of the matrix set by above function 
## It retrieves inv value from cache if it is available in cache, else calculates the inverse and set the inv value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinverse()
            if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
            }
            dataMatrix <- x$get()
            inv <- solve(dataMatrix, ...)
            x$setinverse(inv)
            inv
}
