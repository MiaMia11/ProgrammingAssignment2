## The following functions calculate the inverse of matrix and cache the value. If the contents of 
## a matrix is not changing, the inverse can be looked up in the cache rather than recomputed.

## The makeCacheMatrix function creates special "matrix" which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y){
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setiv <- function(solve)  iv <<- solve
        getiv <- function() iv
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with 
## the above function. it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of inverse in the cache via the setiv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getiv()
        if (!is.null(iv)) {
                   message("getting cached data")
                   return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setiv(iv)
        iv
}
