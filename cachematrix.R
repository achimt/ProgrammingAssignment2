## makeCacheMatrix and cacheSolve calculate the inverse of an
## invertible matrix. The matrix is stored as an object with
## its inverse as a property (voiding the necessity of calculating)
## the inverse every time it is needed

## makeCacheMatrix sets up the object, with the inverse as
## one of its properties and setter and getter functions
## as its methods. 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL # m is the transpose matrix and it is reset to NULL
               # every time makeCacheMatrix is called
     set <- function(y) {     # set receives a matrix
          x <<- y             # and puts it into the 'external' x
          m <<- NULL          # its inverse is set to NULL
     }         # The set function 
     get <- function(){x} # get returns the matrix x as it was
     set_inverse <- function(solve){m <<- solve}
               # cacheSolve calls this when it is called for the
               # first time. The superassignment vector makes sure
               # that the resulting matrix is stored in m
     get_inverse <- function() {m}
               # once m exists, this will return the inverse matrix
               # when cacheSolve is called
     list(get = get,
          set_inverse = set_inverse
          get_inverse = get_inverse)
               # this is, I think, a list of the methods in this
               # class. It helps calling functions to access the
               # methods. 
}


## cacheSolve performs the actual calculation, if that has not
## been done before. In the latter case, it recalls the inverse
## matrix that has been stored as one of the object's properties. 

cacheSolve <- function(x, ...) {   # x is set by makeCacheMatrix
                                   # as x is an object, it also has
                                   # methods that can be called. 
     m <- x$get_inverse()          # this gets the inverse matrix, 
                                   # either from the cache or it will
                                   # be calculated. 
     if(!is.null(m)){              # this is only done, if we already 
                                   # have a value in m
          message("getting cached data")
          return(m)                # the "return" command ends the 
                                   # cacheSolve() function and returns the value
     }
     mat <- x$get()                # if m is NULL, we arrive here
                                   # and use the get method to get our matrix
     m <- solve(mat)               # we then fill m with the inverse matrix
     x$set_inverse(m)              # and we set it as a property of the object x
     m                             # the last value called by a function is always 
                                   # returned as the result
}
