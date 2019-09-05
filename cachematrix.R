# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                            
    newvalue <- function(y) {                    
        x <<- y                            
        inv <<- NULL                        
    }
    get <- function() x                    
    
    newvalueinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(newvalue = newvalue, get = get, newvalueinverse = newvalueinverse, getinverse = getinverse)  
                                                                                  


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$newvalueinverse(inv)
    inv
}
