## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function defines 5 functions:
# "set": setting the cached matrix, set inverse matrix as matrix with null value, Set flag "inv_calculated=FALSE".
# "get": getting the value of cached matrix
# "setinv": calculate inverse matrix and save in cached inverse matrix, set flag "inv_calculated=TRUE".
# "getinv": get the value from the cached inverse matrix
# "getivc": get the value of flag "inv_calculated"

makeCacheMatrix <- function(x = matrix()) {
  xmatrix <- x
  xmatrix.inv <- matrix()
  inv_calculated <- FALSE
  
  set <- function(xm) {
      inv_calculated <<- FALSE
      xmatrix.inv <<- matrix()
      xmatrix <<- xm
  }
 
  get <- function() {xmatrix}
  
  setinv <- function(xm) {
      inv_calculated <<- TRUE
      xmatrix.inv <<- solve(xm)
  }
  
  getinv <- function() {xmatrix.inv}
  
  getivc <- function() {inv_calculated}
   
  list(set=set, get=get, setinv=setinv, getinv=getinv, getivc=getivc)
}


x <- matrix(1:4,2,2)
aa <- makeCacheMatrix(x)
aa$get()
aa$getinv()
aa$getivc()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xm <- x$get()
  xm.inv <- x$getinv()
  ivc <- x$getivc()
  
  if (ivc==TRUE) {
    message("Inverse has been calculated before, getting cached data")
    return(xm.inv)
  }
  else {
    message("Inverse not calculated yet, calculating this time")
    xm.inv <- x$setinv(xm)
  }        
}

aa.inv <- cacheSolve(x=aa)

x <- matrix(c(1:8,33),3,3)
aa$set(x)
aa$get()
aa$getinv()
aa$getivc()
aa.inv <- cacheSolve(x=aa)
aa.inv


