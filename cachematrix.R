## Peer assignment 2
##
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## The following scripts cache the invert of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    # invert the data matrix using solve
    i <- solve(data, ...)
    x$setInverse(i)
    m
    
}

## testing the function
## creating a matrix of doubles with the hilbert function 
# (taken from http://stat.ethz.ch/R-manual/R-patched/library/base/html/solve.html )
# hilbert <- function(n) { 
#   i <- 1:n; 
#   1 / outer(i - 1, i, "+") 
# }
# h8 <- hilbert(8) 
# h8
#           [,1]      [,2]      [,3]       [,4]       [,5]       [,6]       [,7]       [,8]
# [1,] 1.0000000 0.5000000 0.3333333 0.25000000 0.20000000 0.16666667 0.14285714 0.12500000
# [2,] 0.5000000 0.3333333 0.2500000 0.20000000 0.16666667 0.14285714 0.12500000 0.11111111
# [3,] 0.3333333 0.2500000 0.2000000 0.16666667 0.14285714 0.12500000 0.11111111 0.10000000
# [4,] 0.2500000 0.2000000 0.1666667 0.14285714 0.12500000 0.11111111 0.10000000 0.09090909
# [5,] 0.2000000 0.1666667 0.1428571 0.12500000 0.11111111 0.10000000 0.09090909 0.08333333
# [6,] 0.1666667 0.1428571 0.1250000 0.11111111 0.10000000 0.09090909 0.08333333 0.07692308
# [7,] 0.1428571 0.1250000 0.1111111 0.10000000 0.09090909 0.08333333 0.07692308 0.07142857
# [8,] 0.1250000 0.1111111 0.1000000 0.09090909 0.08333333 0.07692308 0.07142857 0.06666667
# invert <- makeCacheMatrix(h8)
# c <- cacheSolve(invert)
# c
#         [,1]      [,2]       [,3]       [,4]        [,5]        [,6]        [,7]       [,8]
# [1,]      64     -2016      20160     -92400      221760     -288288      192192     -51480
# [2,]   -2016     84672    -952560    4656960   -11642400    15567552   -10594584    2882880
# [3,]   20160   -952560   11430720  -58212000   149688000  -204324119   141261119  -38918880
# [4,]  -92400   4656960  -58212000  304919999  -800414996  1109908794  -776936155  216215998
# [5,]  221760 -11642400  149688000 -800414996  2134439987 -2996753738  2118916783 -594593995
# [6,] -288288  15567552 -204324119 1109908793 -2996753738  4249941661 -3030050996  856215352
# [7,]  192192 -10594584  141261119 -776936154  2118916782 -3030050996  2175421226 -618377753
# [8,]  -51480   2882880  -38918880  216215998  -594593995   856215351  -618377753  176679358
# checking if the invert of the invert is equal to the original, can be tricky due to roundings
# all.equal(solve(c),h8)
