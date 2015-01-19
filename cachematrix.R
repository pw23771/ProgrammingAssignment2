## Example of running the following two functions:
## mat<-matrix(c(1.1,2.1,3.1,4.1),nrow=2,ncol=2) # Create an invertable square matrix
## makeCache<-makeCacheMatrix(mat) # Assign a returned list to makeCache
## cacheSolve(makeCache) # Calculate inverse matrix, 1st time no cached
## cacheSolve(makeCache) # Calculate inverse matrix, from cached value from above.

## This "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.
## Input parameters:
## x: An invertable square matrix
## Output:
## A list of functions (set,get,setinverse,getinverse)

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(matrixinverse) minv <<- matrixinverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of an invertable square "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse matrix of
## and sets the value of the inverse matrix in the cache via the `setinverse`
## function.
## Input parameter:
## x: Value returned from makeCacheMatrix()
## Output parameter:
## minv: Inverse matrix of the matrix in makeCacheMatrix input matrix, x.

cacheSolve <- function(x, ...) {
        minv <- x$getinverse() ## Return a matrix that is the inverse of matrix
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}
