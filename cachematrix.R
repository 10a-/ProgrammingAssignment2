##  create a special "Matrix", which is actually a list containing functions to set and get the value of the Matrix en to set and get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## Solve the inverse of the Matrix, which was created with the makeCacheMatrix function. 
## If the inverse has not been calculated yet, this will be done and the value of the inverse will be set in  teh cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
       inv <- solve(data, ...)
        x$setinverse(inv)
        inv 
}
