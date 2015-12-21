## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the inverted matrix
# 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# The following function calculates the inverted matrix of the special "vector"
# It checks if the inverted matrix exists first, if it does, it skips the calculation
# If not, the matrix is inverted and cached via the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
