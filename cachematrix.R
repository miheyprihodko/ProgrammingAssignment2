## These functions could be used to cache the inverse matrix
## and save time on its recalculations

## makeCacheMatrix uses for creating the list, contating the input
## matrix, its inverse and internal functions of resetting
## its values

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(z) inverseMatrix <<- z
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function has two options of its actions: if inverse matrix
## has already been calculated, it would be return without any additional cacls.
## If inverse matrix is absent in cache, it would be calculated and saved in cache
## for future uses

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
