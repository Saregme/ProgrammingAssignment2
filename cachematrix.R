## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Computes the inverse of the special matrix or retrieves the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # cached inverse
        set <- function (y){
                x <<- y
                inv <<- NULL # reset cached inverse when matrix changes
                }
        get <- function () x
        setinverse <- function (inverse_matrix) inv <<- inverse_matrix
        getinverse <- function () inv
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Write a short comment describing this function
#Computes the inverse of a cached matrix or retrieves it if already cached.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  
  inv
}
