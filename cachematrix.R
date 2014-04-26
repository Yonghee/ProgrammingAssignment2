## R Programming Assignment2 
## There is two functions that makes the invesersion of matrix by using cache
##
## How to run this program
## > x <- matrix(rnorm(25),5,5)
## > sx <- makeCacheMatrix(x)
## > ix <- cacheSolve(sx) # first time there is no cached matrix
## > ix <- cacheSolve(sx) # second time you can see the message "getting cached data"


## The following function, makeCacheMatrix creates a special Matrix, which is really a list containing a functio to
# set, get value of matrix
# set, get value of inversion of matrix
makeCacheMatrix <- function(x = matrix()) {
  
  cachedMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    # attemped new matrix, so clear the cache
    cachedMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inversedMatrix){
    cachedMatrix <<- inversedMatrix  
  } 
  getInverseMatrix <- function() cachedMatrix
  list(set = set,
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix
  )

}


## The following function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # if the invsersion of matrix already exists return that
  cachedMatrix <- x$getInverseMatrix()
  if(!is.null(cachedMatrix)) {
    message("getting cached data")
    return(cachedMatrix)
  }
  
  # since there is no cache you rechaed here.
  data <- x$get()
  #computes the inservsion of matrix,
  inversedMatrix <- solve(data,...)
  #save that result to the cache
  x$setInverseMatrix(inversedMatrix)
  
  inversedMatrix
}
