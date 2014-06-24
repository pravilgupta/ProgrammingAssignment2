## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #set the data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #get the data
        get <- function() x
        
        #set inverse
        setInverse <- function(y) inv <<- y
        
        #get inverse
        getInverse <- function() inv
        
        # register the functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #retrieve the inverse
	inv <- x$getInverse()
	
        if(!is.null(inv)) {
        	# get and return the cached data
                message("getting cached data")
                return(inv)
        }
        
        # get the data
        data <- x$get()
        
        # compute inverse
        inv <- solve(data, ...)
        # now cache it by setting it
	x$setInverse(inv)
        inv
}
