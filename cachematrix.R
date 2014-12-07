## Put comments here that give an overall description of what your
## functions do
#
# makeCacheMatrix creates a special "matrix", which is really a list 
# containing  functions to
#
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse
# 4 get the value of the inverse
#
# cacheSolve solve the inverse of the input "matrix", in the manner that 
#
# 1. return the cached inverse if the cache is not NULL
# 2. calculate the inverse with solve if the cache is NULL, set it to the cache, 
#    and then return it
#
# Example:
# > A<-matrix(c(1,2,3,4),c(2,2))
# > B<-makeCacheMatrix(A)
# > B$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(B)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(B)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#



## Write a short comment describing this function
#
# creates a special "matrix", which is really a list 
# of the following functions  
#
# set(y) set y as the data of the matrix
# get() : get the data of the matrix
# setinverse(): set the value of the inverse
# getinverse(): get the value of the inverse
#
# arguments:
# x is a matrix to be set as the "data" of the special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
    }
    get <- function() x

    setinverse <- function(inverse){
        inv <<-inverse
    }
    getinverse <- function() inv
    
    list(set = set, get=get, setinverse = setinverse, getinverse=getinverse)
    
}


## Write a short comment describing this function
#
#  return the cached inverse and print a message, 
#  calculate the inverse with solve and set it to the cache otherwise
#
#  arguments:
#  x is a special matrix created by makeCacheMatrix
#  ... is the other options that will be passed to "solve"
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
    }
    else{
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
    }
    inv
}
