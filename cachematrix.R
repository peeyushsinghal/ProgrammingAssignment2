## Put comments here that give an overall description of what your
## functions do


# The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		# initialising m (inverse matrix) to NULL
		m <- NULL
	  
	  set <- function(y) {
		# setter  for matrix, which is required to be inversed
		x <<- y
		m<<-NULL
	  }
	  
	  get <- function() x #getter for the matrix, which is required to be inversed
	  setinverse <- function(inverse_matrix) {
		# setter  for inversed matrix
		m <<- inverse_matrix
	   }
	  getinverse <- function() m     # getter  for inversed matrix

	  # returning the list of functions
	  list(set = set, get = get,
		   setinverse = setinverse,
		   getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
# the function cacheSolve takes in a matrix object
# it returns cached info if that is present for non changed matrix
# it computes inverse if the matrix is new and/or changed

cacheSolve <- function(x, ...) {
	  m <- x$getinverse()   # populating m and getting inverse matrix
	  if(!is.null(m)) {
		#this block is run only when the inverse has already been computed for an existing matrix
		return(m) 
	  }
	  
	  # the below block of code is executed only when the inverse matrix has not been computed 
	  
	  data <- x$get() #setting the original matrix x (non inversed)
	  m <- solve(data,...) # using solve to get the inverse of the original matrix
	  x$setinverse(m) # setting inverse matrix for future use
	  m # returning inverse matrix
}
