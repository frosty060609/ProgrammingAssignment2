## Put comments here that give an overall description of what your
## functions do
## Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse.

## Write a short comment describing this function
## makeCacheMatrix creates a special object, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) im <<- inverse
    getInverse <- function() im
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function calculates the inverse of a special "matrix" created with the above function.
## It first checks if the inverse matrix has been calculated
## If so, it gets the inverse matrix from the cache and return 
## Otherwise, it calculates the inverse matrix and set the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverse(im)
    im
}
