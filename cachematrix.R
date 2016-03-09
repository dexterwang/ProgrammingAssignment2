## Assignment: Caching the Inverse of a Matrix
##
## given example code of Caching the Mean of a Vector, 
## there is only a few changes needed to get the job done
## (mostly replacing function mean by solve)
##
## There is no code in the function checking if the 
## matrix supplied is a square matrix which is inversable.
## As the requirment stated:
## "For this assignment, assume that the matrix supplied is always invertible. "


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## testing the function 
## > m <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinverse()
## NULL
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m$set(matrix(c(2,3,4,5),nrow=2,ncol=2))
## > m$get()
##      [,1] [,2]
## [1,]    2    4
## [2,]    3    5
## > m$getinverse()
## NULL
## > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > m$getinverse()
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > 



