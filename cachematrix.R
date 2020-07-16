## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { ##Store the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x   
  setsolve <- function(solve) s <<- solve  ## Cache its inverse
  getsolve <- function() s  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##Function solve for computing the inverse of the square matrix
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x = matrix (), ...) {
  s <- x$getsolve()   ## Retrieve from the cache
  if(!is.null(s)) {
    message("getting inversed matrix")  ## Condition if the inverse has been calculated, retrieve from cache
    return(s)   
  }
  data <- x$get()
  s <- solve(data, ...)     ## And if not calculate the inverse.
  x$setsolve(s)
  s
}