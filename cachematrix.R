
## Caching the Inverse of a Matrix (we assume that the matrix supplied is square matrix and it's always invertible)
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## It contains nested functions get, set, getCacheSolve and setCacheSolve to retrieve or assign the value of matrix

makeCacheMatrix <- function(x = matrix()) {
        
        cm <- NULL
        set <- function(y) {
                x <<- y
                cm <<- NULL
        }
        
        get <- function() x
        
        setCacheSolve <- function(cmatrix) cm <<- cmatrix
        
        getCacheSolve <- function() cm
        
        list(set = set, get = get,
             setCacheSolve = setCacheSolve,
             getCacheSolve = getCacheSolve)
}


## cacheSolve computes the inverse of the invertible "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        cm <- x$getCacheSolve()
        
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        
        data <- x$get()       
        
        cm <- try(solve(data, ...),silent=TRUE)
        
        if(class(cm)=="try-error")
        {
             cm<- paste("Please enter a square invertible matrix as mentioned in Programming assignment 2.")  
        }
        
        x$setCacheSolve(cm)        
        cm
}
