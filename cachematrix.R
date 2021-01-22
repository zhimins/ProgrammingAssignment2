## R programing assignment 2
## Create functions to caching the inverse of a matrix and cathes its inverse

## Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    reverse<-NULL 
    
    
    set <- function(y) {
        x <<- y
        reverse <<- NULL
    }
   
    get <- function() x
    
 
    setreverse<-function(res) reverse<<-res
    

    
    getreverse<-function() reverse
    
   
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
    
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),
##  then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    reverse<-x$getreverse
    
    if(!is.null(reverse)) {
        message("getting cached data")
        return(reverse)
    }
    data <- x$get()
    reverse <- solve(data, ...)
    x$setreverse(reverse)
    reverse
}
