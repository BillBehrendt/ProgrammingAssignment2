## makeCacheMatrix -- used to create a 'special' cacheable matrix which is the inverse
##                    of a square matrix provided to it. It has methods for getting
##                    and setting the matrix and its inversion. setting the inversion
##                    with $setinv automatically caches the inverted matrix.
## 
## cacheSolve --    accepts a matrix and gets it inverse, if it hasn't been cached
##                  or returns the cached version if it has been cached.

## assuming matrix gh, use x<-makeCacheMatrix(gh) to make x a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## assuming x is a cached matrix, retrieve the inverse or its cached version with
## xI<-cacheSolve(x)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
## run output
#> source('~/GitHub/ProgrammingAssignment2/cachematrix.R')
#> gh<-matrix(c(1,2,3,4),nrow=2,ncol=2)
#> as<-makeCacheMatrix(gh)
#> as$get()
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> mata<-cacheSolve(as)
#> mata
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> mata<-cacheSolve(as)
#getting cached data
##
##
# proof:
#> gh
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> xc<-solve(gh)
#> xc
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
