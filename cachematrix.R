##The purpose of these R functions is to cache the the value of the inverse of a
##matrix so that when we need it again, it can be looked up in the cache rather
##than recomputed.


##This first function creates a special "vector" object that stores a matrix and
##caches its inverse. The resulting is a list containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix's inverse
## 4.  get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##Initialize matrix inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ##Function to get matrix x
  setinverse <- function(solve) inv <<- solve     ##Function to set inverse
  getinverse <- function() inv    ##Function to get inverse
  list(set = set, get = get,     ##List the functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix`. If the inverse has
##already been calculated, then`cacheSolve` should retrieve
##the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {   ## Check if the inverse of 'x' is already set
    message("getting cached data")  
    return(inv)   ## If positive (TRUE), retrieve its value.
  }
  data <- x$get()   ##Get the matrix 'x'
  inv <- solve(data, ...) ##If negative (FALSE), uses solve() to calculate inverse value.
  x$setinv(inv)
  inv   ##Return the inverse of 'x'.
}
