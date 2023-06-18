## This function creates a list containing a function to 
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) set the value of the inverse and
## (d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  
  set <- function(y) {                             
    x <<- y                                             ## Sets the value of the matrix
    inv <<- NULL                                        ## Initializes the value of the inverse
  }
  get <- function() x                                   ## Gets the value of the matrix
  setinv <- function(inverse) inv <<- inverse           ## Sets the value of the inverse 
  getinv <- function() inv                              ## Gets the value of the inverse
  list(set = set, get = get,                            ## Returns a list
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                                ## Returns the inverse 
  if(!is.null(inv)) {                                    ## Checks to see if the inverse has already been calculated
    message("getting cached data")
    return(inv)                                          ## Returns the inverse from the cache
  }
  data <- x$get()                                        ## Gets the value of the matrix
  inv <- solve(data, ...)                                ## Calculates the inverse if it is not already in the cache
  x$setinv(inv)                                          ## Sets the value of the inverse in the cache
  inv                                                    ## Returns the inverse
}
