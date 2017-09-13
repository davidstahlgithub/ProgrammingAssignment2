## Caching the Inverse of a Matrix:

## Matrix inversion can be costly and one could save resources by caching the inverse.

## makeCacheMatrix creates a matrix object that can have its inverse cached.     

makeCacheMatrix <- function(x = matrix()) {
        
	z <- NULL
        
	set <- function(y) {
                
		x <<- y
                
		z <<- NULL
        
	}
        
	get <- function() x
        
	setinverse <- function(inverse) z <<- inverse
        
	getinverse <- function() z
        
	list(set = set, get = get,
            
		 setinverse = setinverse,
             
		 getinverse = getinverse)

}


## cacheSolve finds the inverse of the object created above.  If inverse has already been calculated then it should retrive inverse from ## cache.  


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
          
	z <- x$getinverse()
       
        if(!is.null(z)) {
                
		message("getting cached data")
                
		return(z)
        
	}
        
	data <- x$get()
       
	 z <- solve(data, ...)
        
	x$setinverse(z)
       
	 z
}