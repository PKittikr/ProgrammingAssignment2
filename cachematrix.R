## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                #initialize inverse as NULL              
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}      #function to get matrix x
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}     #function to obtain inverse of matrix x
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function is for getting the cache data.
cacheSolve <- function(x, ...) #get cache data
{
        inv <- x$getinverse()
        if(!is.null(inv))      #check whether inverse is NULL
        {
                message("getting cached data")
                return(inv)    #return inverse value
        }
        matrix_invert <- x$get()
        inv <- solve(matrix_invert, ...)   #calculate inverse value
        x$setinverse(inv)                
        inv                                #return a matrix that is the inverse of 'x'
}

