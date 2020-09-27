## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# To create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setmatrix <- function(matrix) m <<- matrix
        
        ## Method to get the inverse of the matrix
        getmatrix <- function() m
        
        ## Return a list of the creeated methods
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        #return the inverse if set
        
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        #get the matrix 
        
        data <- x$get()
        
        #matrix inverse by solve 
        m <- solve(data, ...) %*% data
        
        ## Set the inverse and return m
        x$setmatrix(m)
        m
}

