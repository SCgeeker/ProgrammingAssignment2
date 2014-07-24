## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 

makeCacheMatrix <- function(x = matrix() ) {
        s <- NULL
        setMat <- function(y) {
            x <<- y
            s <<- NULL
        }
        getMat <- function() x
        setInv <- function(solve) s <<- solve
        getInv <- function() s
        list(setMat = setMat, 
             getMat = getMat,
             setInv = setInv,
             getInv = getInv
             )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInv()
        if(is.null(s)) {
            message("getting cached matrix")
            message("cached matrix was not calculated")
        }
        
        data <- x$getMat()
        
        data.nr <- nrow(data)
        data.nc <- ncol(data)
        if( data.nr == data.nc) {
            message("cached matrix is squared matrix")
        }
        else {
            message("cached matrix is not squared matrix")
            return(data)
        }
        
        data.det <- det(data)
        if( abs(data.det) >= 0.001) {
            message("cached matrix has a inversed matrix")
        }
        else {
            message("cached matrix has no inversed matrix")
            return(data)
        }
        
        s <- solve(data, ...)
        if(!is.null(s)) {
            message("getting inversed matrix")
        }
        
        x$setInv(s)
        s
        
}
