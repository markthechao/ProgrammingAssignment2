## Two functions to compute and cache the inverse of a matrix

## First function to set and get results

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Second function to check the cache and compute the inverse result 
## if it is not cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



# testing -----------------------------------------------------------------

set.seed(1)
mat <- matrix(rnorm(90000),300,300)
cache <- makeCacheMatrix(mat)
matInv <- cacheSolve(cache)

identical(matInv, solve(mat))
