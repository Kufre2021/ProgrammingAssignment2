makeCacheMatrix<- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function calculates the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (with no  
## change in the matrix), then it should retrieve the inverse from the cache

cachesolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cache data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
