## Following functions will cache the inverse of the matrix 
## and if the inverse of the matrix is not in the cache then 
## it will put the inverse in the cache and will return the 
## inverse by computing it through solve() function

## This function will create a special matrix object that 
## will cache the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
         i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setInverse <- function(matrixinverse) i <<- matrixinverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function will calculate the inverse of the matrix returned by the above 
## function and if the inverse is already calculated then it will retuen it from 
## cache and otherwise it will calculate it through R solve() method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                print("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
