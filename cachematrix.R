## In this R-code file we introduce the `<<-` operator which can be used to
## assign a value to an object in an environment that is different from the
## current environment. Below are two functions that are used to create a
## special object that stores a numeric matrix and caches its invert matrix.
## 
## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse-matrix
## 4.  get the value of the inverse-matrix
##
## makeVector-example provide in README.md, was used as a base to this function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}

## The following function calculates the inverse matrix of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from
## the cache and skips the computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the inverse matrix in the cache via the `setinvmatrix`
## function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
     if(!is.null(m)) {
       message("getting cached data")
       return(m)
     }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}

