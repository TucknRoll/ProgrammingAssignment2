## Used together makeCacheMatrix and cacheSolve will create a "special matrix
## that will able to cache its inverse once created along with functions
## to support the testing to see if the inverse has been previously created
## and should be retrieved instead of being created again.
## example: 
## invmtrx<-matrix(c(1,.25,.35,1),2,2)
## x<-makeCacheMatrix(invmtrx)
## xinvers<-cacheSolve(x)


## Function creates a "special" matrix that will cache its inverse
## once calculated. Input is an invertable matrix
makeCacheMatrix <- function(x = matrix()) {

      m <- NULL              ## Initialize m to NULL
      set <- function(y) {   ## Create set funtion
            x <<- y          ## Set x in parent env to y
            m <<- NULL       ## Set m in parent env to null
      }
      
      ## Function to get the matrix
      get <- function() x    
      
      ## Function to call solve funtion and set m to inverse
      setinverse <- function(solve) m <<- solve
      
      ## Function to get the inverse
      getinverse <- function() m
      
      ## Create list to be returned with the functions created
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      ## Function to retrieve the previously created inverse of a matrix or
      ## calculate, store and return the inverse
      
      ## Pull the inverse from the special matrix and see if it exits
      m <- x$getinverse()
      
      ## if the inverse has already been calculated the return its stored
      ## value, print a message and exit the function
      if(!is.null(m)) {
            message("getting inverse of matrix")
            return(m)
      }
      ## If here the inverse hasn't been calculated yet so calculate it
      ## get the matrix
      data <- x$get()
      ## solve for the inverse
      m <- solve(data, ...)
      ## store the inverse value in the special matrix
      x$setinverse(m)
      ## return the inverse
      m
}
