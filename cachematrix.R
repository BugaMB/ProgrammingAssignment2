## This function will speed up calling matrix inversion,
## after it was done the first time, by caching the result
## of the first inversion.
## Procedure is split in two steps, first to create
## a list of functions used in the second step when these
## are called as necessary.

## Function makeCacheMatrix gives a list containing a 
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    Inv.m <- NULL
    set <- function(y) {
        x <<- y
        Inv.m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Inv.m <<- inverse
    getinverse <- function() Inv.m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function returns the inverse of the given
## matrix. It calls the inverse if it was already computed,
## otherwise it computes the inversion and sets the result
## in the cache. In the following call of this function then
## it will call the inverse from the cache (if the
## matrix was not changed). This exercise assumes table
## is invertible.

cacheSolve <- function(x, ...) {
    Inv.m <- x$getinverse()
    if(!is.null(Inv.m)) {
        message("getting cached data.")
        return(Inv.m)
    }
    data <- x$get()
    Inv.m <- solve(data)
    x$setinverse(Inv.m)
    Inv.m
}
