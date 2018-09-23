## Put comments here that give an overall description of what your
## functions do

## cachematrix.R contains two functions, makeCacheMatrix and cacheSolve.
## The former function takes an invertible square  matrix as input and returns
## a list containing a list of four 'elements' (see more notes on this function
## below). The latter function takes the output of makeCacheMatrix and returns
## the inverse of the input matrix given to makeCacheMatrix (see additional
## notes below).

##-------------------------------------------------------------------

## Write a short comment describing the function makeCacheMatrix

## The function makeCacheMatrix takes as its input a square, invertible matrix
## object, meaning that the number of columns is equal to the number
## of rows, and returns a special matrix 'object' that can cache its
## inverse.  This special object is a list containg a function to:

## 1.) Set the value of the input matrix
## 2.) Get the value of the input matrix
## 3.) Set the inverse of the input matrix
## 4.) Get the inverse of the input matrix

## Note: The output of this function returns an object of the class 'list'.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##-------------------------------------------------------------------

## Write a short comment describing the function cacheSolve

## The function cacheSolve takes as its input the output of makeCacheMatrix
## and returns the inverse of the original square matrix used as input for
## makeCacheMatrix.

## Note,: the output of this function returns an object of the class 'matrix'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where 'x' 
        ## is the output of makeCacheMatrix 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}