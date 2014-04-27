## These functions create a special matrix data structure which is able
## to retrieve cached inverse computations, leading to more 
## speed-efficient programs.

## makeCacheMatrix: Creating the data structure with inside functions for 
## setting and getting its original and/or inversed values.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL # Create variable to store inverse
      set <- function(y, nrow=length(y), ncol=1) { # Setting values of the matrix
            x <<- y
            dim(x) <<- c(nrow,ncol) # Adding dimensions
            s <<- NULL # Matrix is initialized/modified, cached data must be cleared
      }
      get <- function() x
      setSolved <- function(solved) s <<- solved # Setting inverse computed by cacheSolve
      getSolved <- function() s
      list(set = set, get = get, # Returning with a list of values
           setSolved = setSolved,
           getSolved = getSolved)
}


## cacheSolve: Looks up cached inverse data in matrices built with the 
## makeCacheMatrix function. If found, cacheSolve returns with this 
## matrix, otherwise it computes (and sets) it again.

cacheSolve <- function(x, ...) {
      s <- x$getSolved() # Getting cached inverse from matrix 'x'
      if(!is.null(s)) { # If found, it is returned
            message("getting cached data")
            return(s)
      }
      data <- x$get() # Otherwise cacheSolve reads original matrix..
      s <- solve(data, ...) # ..computes inverse
      x$setSolved(s) # ..sets it inside 'x'
      s #..and gets returned with this inverse. 
}
