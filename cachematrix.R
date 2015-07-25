## The below functions optimize system performance by caching
## matrices along with their inverses.  This allows a developer
## using this code in their system to calculate the inverse only
## once rather than each time it is needed.

## function makeCacheMatrix creates a special cached matrix object 
## that includes its inverse in the cached data.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
  	mat <<- x
  	inv <- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## function cacheSolve checks to see whether a matrix and its
## inverse have already been cached.  If so, the stored inverse can
## be quickly retrieved and used.  If not, the calculation is
## performed and stoed so it is available if needed again.

cacheSolve <- function(x, ...) {
  # see if the data is available from the cache	
  i <- x$getinv()	
  # if so, display a message and return the cached inverse
  if (!is.null(i)) {
  	message("getting cached data")
  	return(i)
  }
  # if not, obtain the matrix from the cache
  data <- x$get()
  # use solve() to compute the inverse
  i <- solve(data)
  # store the inverse on the matrix for next use
  x$setinv(i)
  # return the inverse
  i
}


## developers can use this code as follows:
## initialize the CacheMatrix object with the subject matrix
##    m3 <- matrix(data = c(1,5,7,9,8,2,6,3,3), nrow = 3, ncol = 3)
##    m3cached <- makeCacheMatrix(m3)
##  first call to cacheSolve caculates and caches inverse
##    m3inv1 <- cacheSolve(m3cached)
##  subsequent calls to cacheSolve return inverse without repeating calculation
##    m3inv2 <- cacheSolve(m3cached)


