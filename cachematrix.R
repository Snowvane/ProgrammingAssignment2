## The two functions allow a user to get the inverse of a matrix without having
## to calculate the inverse each time. The inverse of the matrix is calculated
## only the first time cacheSolve is called. After that whenever
## the inverse needs to be calculated for that matrix, a cached copy stored
## by using makeCacheMatrix, is returned instead of evaluating inverse again.
## It is assumed that user will always enter an invertible matrix so no
## measures have been taken to avoid an error in that case

## makeCacheMatrix allows the user to create a list of functions (cache Matrix)
## that allows the user to get and set the matrix. It also allows the user to
## get and set the inverse of the matrix. These functions are utilised in first
## creating a cache matrix that can be passed to cacheSolve and are also later
## used by the cacheSolve matrix when an inverse needs to be evaluated

makeCacheMatrix <- function(x = matrix()) {
      xi <- NULL
      set <- function(y) {
            x <<- y
            xi <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) xi <<- inverse
      getInverse <- function() xi
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)  
}


## cacheSolve allows the user to get the inverse of a matrix without having to
## make tedious calculations each time. It takes a list of functions 
## (cached matrix) and checks if the inverse already exists in cache by 
## utilising functions in makeCacheMatrix. If the inverse is already present 
## then it is returned. Otherwise it calculates the inverse, stores it in cache 
## and then returns the inverse

cacheSolve <- function(x, ...) {
      xi <- x$getInverse()
      if(!is.null(xi)) {
            message("getting cached data")
            return(xi)
      }
      data <- x$get()
      xi <- solve(data)
      x$setInverse(xi)
      xi
}
