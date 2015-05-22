## CourseRA Assignment #2
## This is a R function that can create a matrix, calculate its inversion,
## save the inversion in the cache, and retrive it if inversion in the cache

## This function will create a matrix, calculate its inversion, and save 
## the inversion in cache.

makeCacheMatrix <- function(x = matrix()) {
     x_inv <- NULL
     set <- function (y) {
         x <<- y
         x_inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) x_inv <<- solve
     getinv <- function() x_inv
     list (set = set, get = get,
           setinv = setinv, getinv = getinv)
}

## Function to calculate the matrix inversion using the above function.
## First, it will check whether the matrix inversion is already in the cache.
##        yes, it will get the matrix inversion
##        no, it will calculate with solve(), and put it into cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       x_inv <- x$getinv()
       if (!is.null(x_inv(,))) {
           message("getting matrix inversion from cache")
           return(x_inv)
       }
       data <- x$get()
       x_inv <- solve(data,...)
       x$setinv(x_inv)
       x_inv
}
