## Following functions will accept an invertible matrix, compute
## its inverse and cache it.  The purpose of caching is to reduce
## costly computation of the inverse repeatedly. 

##Implementation/Steps for testing
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## mymatrix<-makeCacheMatrix(m1)  
## cacheSolve(mymatrix)  ## returns inverse of m1
## cacheSolve(mymatrix)  ## returns cached inverse of m1.


## makeCacheMatrix function creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set <- function(y){
            x<<-y
            inv <<-NULL
      }
      get <-function() {x}
      setInverse <-function(inverse) {inv <<-inverse}
      getInverse <-function() {inv}
      list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse was already calculated, it returns cached 
## inverse matrix.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      
      #checks if inverse matrix already calculated/available 
      # if yes, then returns cached inverse matrix.
      if(!is.null(inv)){
          message("get cached data")
          return(inv)
      }
     
       data<-x$get()
      #inverse the supplied matrix
      inv <-solve(data)
      x$setInverse(inv)
      inv
}

