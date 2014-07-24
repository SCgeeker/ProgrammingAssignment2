## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 

makeCacheMatrix <- function(x = matrix() ) {
        s <- NULL
        setmatrix <- function(y) {
            x <<- y
            s <<- NULL
        }
        getmatrix <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() solve(s)
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached matrix")
            return(s)
        }
        
        d <- x$getdet()
        if(is.null(d)) {
            message("input was not a squared matrix")
            return(d)
        }
        else if(abs(d) < 0.001) {
            message("input was not a inversable matrix")
            return(d)
        }
        
        data <- x$get()
        s <- det(data, ...)
        x$setsolve(s)
        s
}

### d <- NULL
###   d <<- NULL
###setdet <- function(det) d <<- det
###getdet <- function() d
##setdet = setdet,
##getdet = getdet,
