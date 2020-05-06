## This pair of functions will (1) create a "special matrix" and cache its
## inverse, (2) retrieve the cached inverse of the matrix or calculate and cache
## it if the cache is empty

## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y # set matrix value
                m <<- NULL # clears old inverse from cache
        }
        get <- function () x # get matrix value
        set_inverse <- function(inverse) x <<- inverse # set inverse value
        get_inverse <- function() m # get inverse value
       
        # create a list with all the functions above
        list (set = set, get = get, 
              set_inverse = set_inverse, 
              get_inverse = get_inverse)
}

## cacheSolve retrieves the calculated inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has not already been calculated, this 
## function will then compute the inverse of the "matrix" returned by 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$get_inverse() # retrieve calculated inverse from cache
        if(!is.null(m)) { # if the cache is not empty, retrieve data
                message("getting cached data")
                return(m)
        }
        # if the cache is empty, proceed to calculating the inverse
        data <- x$get() # get the matrix value (from makeCacheMatrix)
        m <- solve(data) # calculate the inverse of the matrix
        x$set_inverse(m) # cache the matrix inverse value
        m # return the inverse of the matrix
}





