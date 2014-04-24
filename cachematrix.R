## The functions below create a special "matrix" object that can cache its inverse.

## The function makeCacheMatrix below creates a special "matrix" object, which is a list
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {      
        m <- NULL
        set <- function(y){
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function cacheSolve below calculates the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix 
## has not changed) then the cacheSolve function will retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
              message("getting cashed inverse matrix")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
