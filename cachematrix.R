# The first function is makeCacheMatrix which computes the inverse a special matrix 
# returned by the makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL   # initializing the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x # get the matrix
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv  # get the inverse of the matrix
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        

}



##  The second function cacheSolve retreaves the inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv  ## Return a matrix that is the inverse of 'x'
        
        
}

