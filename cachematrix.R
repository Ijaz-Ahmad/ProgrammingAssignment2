## This function creates a special matrix which contains a list
## of functions ....inverse of a matrix/matrices

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {                      ## set the value of x and m,
    x <<- y                                 ## if the special matrix has not been calculated before
    m <<- NULL
  }
  get <- function() x                       ## get the function to implement
  setsolve <- function(solve) m <<- solve   ## apply the function 'solve' and place the result in m
  getsolve <- function() m                  ## get the value of the matrix inverse
  list(set = set, get = get,                ## list down the results
       setsolve = setsolve,
       getsolve = getsolve)
}


## This fuction checks the availability of required data from cache 
## and calculates/stores the same if not available in cache


cacheSolve <- function(x, ...) {
 makeCacheMatrix(directory)            ## get complete directory of function 'makeCacheMatrix
  sp_mat <- makeCacheMatrix(x)          ## create a special matrix of matrices 'x'
  m <- sp_mat$getsolve()                ## check if inverse of matrix has alresdy been calculated
  if(!is.null(m)) {
    message("getting cached data")      ## print "getting cached data"
    return(m)                           ## get cached data
  } 
  data <- sp_mat$get()                  ## calculate the inverse of the data and set the values
  m <- solve(data,...)                 ## of the solve via 'setmean function' 
  sp_mat$setsolve(m)
  m
}
