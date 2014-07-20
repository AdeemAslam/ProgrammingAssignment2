## Put comments here that give an overall description of what your
## functions do

## Following function creates a special matrix the inverse and value
## of which can be set and retrieved.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function (y)
    {
      x <<- y
      inv <<- NULL
    }    
    get <- function () x
    setinv <- function (inverse) inv <<- inverse
    getinv <- function () inv
    
    list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## Following function returns the inverse of the matrix by either
## computing it using the solve() function or fetching it from the 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv))
    {
      message ("getting cached inverse")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
