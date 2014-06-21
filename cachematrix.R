##Coursera R Programming Assignment 2


##Function to cache inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##set solve function to apply to matrix  
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##Function to solve for the inverse of a matrix if not cached

cacheSolve <- function(x=matrix(), ...) {
  ##Sets cached inverse (if exists)
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    ##Pulls cached matrix
    message("getting cached data")
    ##Prints cached matrix
    return(m)
  }
  
  ##Calculates matrix inverse if 
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  ##prints result
  m
}






