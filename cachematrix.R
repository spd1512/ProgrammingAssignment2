## Matrix inversion is usually a costly computation.
##There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
##The two main functions that are used for this purpose are as follows:
##1.makeCacheMatrix: 
##This function creates a special "matrix" object that can cache its inverse.
##2.cacheSolve: 
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then 
##cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix
##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse 
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix <<- inverse
        getinverse <- function() matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##cacheSolve
## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the data and sets the value of the inverse in the cache via the `setinverse`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$getinverse()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data,...)
        x$setinverse(matrix)
        matrix
}
