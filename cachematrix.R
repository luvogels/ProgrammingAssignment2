## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as the argument, and has functions to get and set the value of the matrix. It also has funtions to set and get the cached value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                    ## sets a new value of the matrix
                x <<- y                         ## the new value is stored in x
                m <<- NULL                      ## m contains the inverse of the matrix and is set to null when a new value of the matrix is set
        }
        get <- function() x                     ## returns the matrix
        setinverse <- function(cachedSolve){    ## caches the value of the inversed matrix
                m <<- cachedSolve
        }                                       
        getinverse <- function() m              ## returns the inverse matrix if it has been cached, if not it returns NULL
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a variable x containing makeCacheMatrix. It returns the inverse matrix of x. Before returning a value it checks if the inverse has already been calculated. If it has been calculated it returns the stored value without calculating it. If no value has been cached it calculates the inverse caches it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()             ## gets the cached matrix from x
        if(!is.null(m)) {               ## if m is not null, x has already been inversed. No need to inverse the matrix, instead the cached value is reaturned
                message("getting cached data")
                return(m)               ## the cached value of x inverse is returned
        }
        data <- x$get()                 ## get the matrix that need to be inversed from x
        m <- solve(data, ...)           ## the function solve() is used to inverse the matrix
        x$setinverse(m)                 ## the inverse of x is passed back to x for caching
        m                               ## the inverse of x is returned
}