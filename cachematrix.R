## The inverse of matrix is resource intensive operation so this program checks for cached 
## matrix data or creates a new inverse matrix if not cached

## This function creates a list object that can cache an inverse matrix & actual matrix

makeCacheMatrix <- function(x = matrix()) {
	  invm <- NULL
	  set <- function(m1)
	  {
		x <<- m1
		invm <<- NULL
	  }
	  
	  get <- function() x
	  
	  setInverse <- function(invm1) invm <<- invm1
	  getInverse <- function() invm
	  
	  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function sets the inverse matrix value to cache it for later use

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  invm <- x$getInverse()
	  if(!is.null(invm))
	  {
		message("getting from cache")
		return(invm)
	  }
	  data <- x$get()
	  invm <- solve(data)
	  x$setInverse(invm)
	  invm
}
