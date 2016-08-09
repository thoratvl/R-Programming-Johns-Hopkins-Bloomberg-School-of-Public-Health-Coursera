## This program is used to calculate the inverse of the matrix using cache

## This function is used to create special tye of matrix used for cacheing

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    #set matrix
    set <- function( matrix ) {
        x <<- matrix
        i <<- NULL
    }
    
    #get matrix
    get <- function() {
        x
    }
    
    #set inverse
    setInverse <- function(inverseMatrix) {
        i <<- inverseMatrix
    }
    
    #get inverse
    getInverse <- function() {
        i
    }
    
    #return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function is used to find inverse using cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## return the inverse if its already present
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix 
    data <- x$get()
    
    ## Calculate the inverse
    m <- solve(data) %*% data
    
    ## Set the inverse to the inverse calculated
    x$setInverse(m)
    
    ## Return the matrix
    m
}
