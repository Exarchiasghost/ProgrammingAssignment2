## Both makeCacheMatrix() and cacheSolve() are working for the same reason. It is a king of gun/trigger relation
## as the first function (makeCacheMatrix) initiates a matrix in order to be cacheable and inverted, so long the
## second function pulls the trigger and inverts the matrix and solves the inversion with a simple comparison.


#makeCacheMatrix(): takes and initiates a matrix placed in variable x and handles the cacheable matrix R
#for further use.

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL 
  set <- function(y) { 
    x <<- y 
    ma <<- NULL 
  }
  get <- function() x 
  setinvert <- function(inverse) ma <<- inverse
  getinvert <- function() ma 
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

##cacheSolve(): "Activates" as a trigger the cacheable matrix which is placed in the variable ma
## and after a short check, inverts and returns the cached matrix ma.


cacheSolve <- function(x, ...) {
  ma <- x$getinvert() 
  if(!is.null(ma)) { 
    message("getting cached data")
    return(ma) 
  } 
  ma <- x$get() 
  ma <- solve(dataalm, ...) 
  x$setinvert(ma) 
  return ma 
}
