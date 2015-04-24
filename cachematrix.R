## The assignment 2 is to write a pair of functions 
## that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated, then 
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(nrow(data)==ncol(data)){
        m <- solve(data, ...)
        x$setinverse(m)
        m    
    } else {
        message("not a invertible matrix ")
        return(data)
    }
}
