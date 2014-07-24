## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 'makeCacheMatrix' creates a list containing two matrixes
## 'Mat' is assgined the matrix in the argument. Two sub-functions
## 'setMat' and 'getMat' serve this. 'Inv' is the inversed matrix
## of 'Mat'. Two sub-functions 'setInv' and 'getInv' wait the matrix
## that will be calculated in cacheSolve().

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

## Inversed matrix has not be generated at the input of
## 'Mat'. First if loop makes sure a matrix was got and
## passes to 'data' object. Second if loop makes sure this
## object a squred matrix, and third if loop makes sure
## an inversed matrix able to be calcualated. Final if loop
## checks the output of calculation and passes to 'Inv'. 

## This function had been checked in use of the function
## provided by Daniel Fumanal who suggested the oringial
## script in the forum. 

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
