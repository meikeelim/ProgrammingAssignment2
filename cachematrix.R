##makeCacheMatrix function creates a special "matrix" which to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   ## m is set as NULL to store the cached inverse matrix
        set <- function(y) {    ## this is to set the value of the matrix
                x <<- y
                m <<- NULL
        }
        
        ## <<- operator which can be used to assign a value to an 
        ## object in an environment that is different from the current environment.
        
        get <- function() x
        ## get the value of the matrix
        setinverse <- function(solve) m<<- solve
        ## this is to set the inverse matrix
        getinverse <- function() m
        ## this is to get the inverse matrix
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
        ## creates a list to house the four functions
}

## cacheSolve will calculate the inverse of the matrix, 
#If the inverse has already been calculated then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #checking for cached data
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m<-solve(data,...)     ## calculates the inverse of the matrix
        x$setinverse(m)        
        m                      ## Return a matrix that is the inverse of 'x'
}
