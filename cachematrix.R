## Cache functionality for computing the inverse of a Matrix.
## Example usage:
## >
## >ma<-makeCacheMatrix(matrix(1:4,2,2)) #2x2 matrix which is invertible
## >cacheSolve(ma) # computes the inverse, stores this in cache and returns inverse 
## >cacheSolve(ma) # returns the cached inverse without needing to recompute 

## This function creates a special "matrix" object that can cache its inverse.
## $get and $set functions store the original matrix
## $getMatrix and $setMatrix store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(mat) m <<- mat
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated, then the cachesolve  retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
