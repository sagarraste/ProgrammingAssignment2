
## makeCacheMatrix: This function creates a 
## special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) {i <<- solve}
  getinverse <- function() i 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of
## the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated
## (and the matrix has not changed), 
## then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## create an invertible 4 X 4 matrix for testing
nums <- c(2, 1, 0, 0, 3, 0 , 2, 2, 1, 3, -3, 3, 5, 1, 2, 1)
mnums <- matrix(nums, nrow = 4, ncol = 4)

## call function that will create special matrix
cachemnums <- makeCacheMatrix(mnums)

## call function that will return inverse matrix
## from cache when available else will calculate it
## and return
cacheSolve(cachemnums)

## RESULT - 
## first time when your run the functions  
## for the test matrix you get results
## like below - 
## ------------------------------------
## [,1] [,2] [,3] [,4]
## [1,]   18  -35  -28    1
## [2,]    9  -18  -14    1
## [3,]   -2    4    3    0
## [4,]  -12   24   19   -1
## ------------------------------------

## if you run the same cachesolve(cachemnums) again
## we get following results
## ------------------------------------
## getting cached data
## [,1] [,2] [,3] [,4]
## [1,]   18  -35  -28    1
## [2,]    9  -18  -14    1
## [3,]   -2    4    3    0
## [4,]  -12   24   19   -1
## ------------------------------------

## please notice extra row printed with message - getting cached data
## that proves function is not recalculating the
## results, its just pulling it from cache.




