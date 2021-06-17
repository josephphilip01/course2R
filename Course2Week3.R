makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL # initializing the inverse as NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x # function to get matrix x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j # function to get the inverse of the matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j))  # checking whether inverse is NULL
  {
    message("getting cached data")
    return(j) # returns inverse value
  }
  mat <- x$get()
  j <- solve(mat,...) # calculates inverse value
  x$setInverse(j)
  j # return a matrix that is the inverse 
}