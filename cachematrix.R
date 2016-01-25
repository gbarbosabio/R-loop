## As far as I understand, the function gets matrix x and inverts it with solve
## function. Than if a matrix x gets the same inverted matrix it returns the
## message "getting cached data" and the inverted matrix. If not it inversts the 
## new matrix

## It gets the inverted matrix from a certain matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## If checks if the inverted solution already exists, and if it doesnt it gets the
## inverted matrix
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
