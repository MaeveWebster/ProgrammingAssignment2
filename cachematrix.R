## I tried to follow a couple examples and organize it in a way 
#that visually made sense to me. Tested it out and it worked for me

## makeCacheMatrix creates a special "matrix" object that 
#can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      matrixInv <- NULL
      set <- function(y){
          x <<- y
          matrixInv <<- NULL
      }
      
      get <- function() x
      setMatInv <- function(solution) matrixInv <<- solution
      getMatInv <- function() matrixInv
      
      list(set = set, get = get, setMatInv = setMatInv, getMatInv = getMatInv)
      
}

## computes the inverse of the special "matrix" returned by 
#`makeCacheMatrix` above...
cacheSolve <- function(x, ...) {
  matrixInv <- x$getMatInv()
  if(!is.null(matrixInv)){
      message("Retreiving cached data...")
      return(matrixInv)
  }
  mat <- x$get()
  matrixInv <- solve(mat, ...)
  x$setMatInv(matrixInv)
  matrixInv
## Returns a matrix that is the inverse of 'x'
}

