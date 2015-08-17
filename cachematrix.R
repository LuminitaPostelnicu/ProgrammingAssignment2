## Caching the inverse of a matrix:
## Below there are two functions that create a special "matrix" object that can 
##cache its inverse and retreives the inverse of this special "matrix" 

## The function makeCacheMatrix creates the special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #this field ('inv') will store the cached value (the inverse matrix)
        inv <- NULL
        #this function ('setmatrix') can be used to change the embedded matrix
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #this function ('getmatrix') will be used to retrieve the embedded matrix
        getmatrix <- function () x
        #this function ('setinverse') will be used to store the calculated inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        #this function ('getinverse') will be used to retrieve the cached inverse matrix
        getinverse <- function() inv
        #return the constructed object:
        list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## The next function retrieves the inverse of the special matrix (if it has been
## calculated and the matrix did not change), or otherwise it computes its inverse

cacheSolve <- function(x, ...) {
        #try to get the cached value:
        inv <- x$getinverse()
        #if there was a cached value, then return it:
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #else, if there was no cached value, then compute it by computing 
        #(solving) the inverse of the original matrix:
        mat <- x$getmatrix()
        inv <- solve(mat)
        #store the computed value into the cache:
        x$setinverse(inv)
        #return the computed inverse:
        inv
}
