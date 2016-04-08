## Function to calculate the inverse of any matrix
## it will take any matrix as a parameter and cache it
## How to test it: 
## mm<- makeCacheMatrix()
## mm$set(matrix(1:4,2,2))
## cacheSolve(mm)

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setmtinv <- function(mtinv) iv <<- mtinv
  getmtinv <- function() iv
  list(set = set, get = get,
       setmtinv = setmtinv,
       getmtinv = getmtinv)
}


##will calculate the inverse matrix of x and cache the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getmtinv()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setmtinv(iv)
  iv
}
