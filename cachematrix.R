#makeCachematrix creates a matrix and cache its inverse
# cacheSolve return the cached inverted matrix if it has been calculated
# before. Otherwise it calculates it and caches the result.

#The first function, makeCacheMatric creates a special "matrix", 
# which is really a list containing a function to :
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The second function computes the inverse of the matrix returned by makeCacheMatrix. 
# If it has been calculated, it retrieves it from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
