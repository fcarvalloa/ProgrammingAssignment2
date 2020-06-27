## Programming Assignment 2: Lexical Scoping

## Fernando Carvallo Arrau


## First, set the input x as a matrix
## Second, set the solved value "inv" as a null
## Third, change references "mean" to "inverse"


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Here, changed every reference to "mean" to "inverse" and "m" to "inv"

cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of "x"
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}