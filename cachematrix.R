## R Programming Course: Programming Assignment no. 2
## By Benjamín Manchado, July 2014
#
# Since matrix inversion is usually a costly computation, we are going to cache
# inverse of a matrix in order not to compute it repeatedly every single time
# we need it in our analysis. The procedure consists in defining two functions:
#
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. Actually, this "matrix" is a list containing a function to:
# a) set the value of the "matrix"
# b) get the value of the "matrix"
# c) set the value of the inverse
# d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL        # in here we will put the cached inverse of matrix  
        set <- function(y) {   # this is the first element of the list
                x <<- y        # note the use of the "<<-" operator
                inverse <<- NULL
        }
        get <- function() x    # this is the second element of the list
        setinverse <- function(solve) inverse <<- solve # third element of the list
        getinverse <- function() inverse   #fourth element of the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    # this is our special "matrix"
        
}

# For example:
# > a <- rnorm(9)                 : elements for my example matrix
# > dim(a) <- c(3,3)              : creates the matrix
# > b <- makeCacheMatrix(a)       : calls the first funtion
# > b                             : shows the result
# > class(b)                      : special "matrix" which is actually a list
# > b$get()                       : returns 'b' matrix
# > b$getinverse()                : should be NULL until inverse is cached
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
# 2. cacheSolve: This function computes the inverse of the special "matrix"
# returned by the first function. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve function should retrieve
# the cached inverse (showing a message).

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()   # assigns value from element from "matrix"
        if(!is.null(inverse)) {     # checks if the inverse is in the cache... 
                message("getting cached data")  # if so returns a message... 
                return(inverse)     # and also the cached inverse
        }
        data <- x$get()             # if the inverse has not been cached yet...
        inverse <- solve(data, ...) # it calculates the inverse...
        x$setinverse(inverse)       # caches it...
        inverse                     # and finally returns it.        
}

# For example:
# cacheSolve(b)                 returns inverse of b calculated for the 1st time
# b$getinverse()                inverse is now cached!!!
# cacheSolve(b)                 returns inverse of b from the cache showing a...
#                               message
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
# and this is it! Thanks for your time!
#