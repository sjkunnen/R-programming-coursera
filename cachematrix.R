## Put comments here that give an overall description of what your functions do: 
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function:
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
      
      ## set the value of the matrix
      cachematrix <- NULL
      set <- function(y) {
            x <<- y
            cachematrix <<- NULL
      }
      ## return the values of the matrix
      get <- function() {
            x
      }
      
      ## set the value of the inverse matrix
      setsolve <- function(solve) {
            cachematrix <<- solve
      }
      
      ## return the value of the inverse matrix
      getsolve <- function() {
            cachematrix
      }
      
      ## create a list for the matrix
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Write a short comment describing this function:
## This function computes the inverse of the special "matrix" created with the above function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cacheSolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## First, get the matrix from cache
      cachematrix <- x$getsolve()
      
      ## If the cachematrix exists, then the result is returned
      if(!is.null(cachematrix)) {
            message("Getting cached matrix")
            return(cachematrix)
      }
      ## If the cachematrix is not found, then data is assigned to x, which is a matrix
      data <- x$get()
      
      ## The inverse of the matrix is given with the solve function
      cachematrix <- solve(data, ...)
      x$setsolve(cachematrix)
      
      ## Return the inverse matrix result:
      cachematrix
}
