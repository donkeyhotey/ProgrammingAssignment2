##These functions fine the inverse of a matrix and cache
##the inverse. When run again it checks to see if the cache
##is empty before solving again, saving memory. To do this
##we need to make sure they are stored in the correct
##parent environment

## The first function sets the values to empty, then
## creates a list of functions to 1) assign the input
## argument to the x object in the parent argument, 2)
## assign NULL to i object int he parent environment, 3)
## retrieve x from set argument, 4) set the inverse for
## x, 5) retrieve the inverse of x and store in i, and
## 6) put these functions into a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<-y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list( set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function checks to see if the inverse of
## the matrix has already been found. If it hasn't
## it calculates the inverse of the matrix and stores
## it in i

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
