## These functions will cache a matrix and its inverse.

## This function creates a special matrix object and
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        
        set <- function(y){
                x <<- y
                inv <<- matrix()
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function will return inverse of special matrix in 
## 'x' using cached inverse if already calculated

cacheSolve <- function(x, ...) {       
        inv <- x$getinv()
        if(!(sum(is.na(inv)) > 0)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}