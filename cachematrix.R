## This script is used to cache calculated matrices inverse

##makeCacheMatrix function stores a matrix and caches its value


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ##for cached matrix storage
  set <- function(y) {
    x <<- y
    inv <<- NULL ##cache clearing incase a new value is set
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheginv calculates the inverse of a matrix calculated by "makeCacheMatrix"
##incase the inverse is calculated before, the function returns the cached value

##[IMPORTANT] this function requires the package "MASS" to be installed
##so it can be universal and take any matrix of any dimention

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  library(MASS) ##Loading MASS package
  inv <- x$getinv()
  if(!is.null(inv)) {   ##Checking for cached data
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  
  inv <- ginv(data, ...)##inverse calculation incase no cached data available
  x$setinv(inv)
  inv
}
