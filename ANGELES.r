makeCacheMatrix <- function(x = matrix()) { ##CACHE MAKER}
  inv <- NULL ## NO MATRIX
  get <- function() x ##FUNCTION CREATE
  set <- function(y) { ##ASSIGN FUNCTION
    x <<- y ##INVERSE MTRX
    inv <<- NULL ##INVERSE MTRX
  }
  getinv <- function() inv ##ASSIGN FUNCTION
  setinv <- function(inverse) inv <<- inverse
  list(get=get, set=set, getinv=getinv, setinv=setinv)
  ##MTRX
}
cacheSolve <- function(x, ...)  { ## SOLVE FOR THE INVERSE
  inv <- x$getinv() ##FNCTION FOR INVERSE
  if (!is.null(inv)) {
    message("inverse is cached") ##RESULT
    return(inv) ##RETURN TO THE INVERSE MTX
  }
  m <- x$get() ##SOLVE FOR INVERSE
  inv <- solve(m, ...) ##SOLVE FOR MATRIX
  x$setinv(inv) ##RETURN TO INVERSE FN
  return(inv)
}
##USED FOR SOLVING INVERSE MATRIX USING RSTUDIO