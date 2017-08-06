<## This program stores inverse of a matrix into a cache so that it does not need to be
## stored again and again and can be retrieved when needed.


## This function stores Inverse of the matrix x in to cache.
## This function returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  
  ## set function stores cache value; y = cache value  
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  
  ## get function helps to get the value of matrix passed to makeCacheMatrix
  get <- function() x
  
  ## setInv function helps to set the value of inverseMatrix so that it can be stored in Cache
  setInv <- function(InvMatrix) InverseMatrix <<- InvMatrix
  
  ## getInv helps to retrieve the value stored in cache
  getInv <- function() InverseMatrix
  
  ## a list of function is returned via makeCacheMatrix function
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
##
## Input parameter is a function x, where in ... are 3 parameters of the function x.


cacheSolve <- function(func, ...) {
        ## Return a matrix that is the inverse of matrix 'x'
  InvMat <- func$getInv()
  if (!is.null(InvMat)) {
    message("getting cache Inverse Matrix")
    return(InvMat)
  }
  Mat <- func$get()
  InvMat <- solve(Mat,...)
  func$setInv(InvMat)
  InvMat
}
