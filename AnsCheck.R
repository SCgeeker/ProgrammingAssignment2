### Copied from fornum of Assignment 2 
### origin: Daniel Camacho Fumanal
### improve: anonymous participant

seeTimesWithTest <- function (n = numeric)
{
  ## Shows how to create the special "matrix" object 
  ## calling 'makeCacheMatrix'.
  ## Generate a square matrix of dimension n,
  ## using random elements.
  my_matrix <- matrix(rnorm(n*n), n, n)

  ## Create a special "matrix" object that can
  ## cache its inverse.
  my_special_matrix <- makeCacheMatrix(my_matrix)


  ## Shows how to replace the original matrix into an
  ## already created special "matrix" object invoking
  ## the 'set' method.

  ## Replace the original matrix into an already
  ## created special "matrix" object.
  ### changed the names for my cacheSolve().
  my_special_matrix$setMat(my_matrix)



  ## First  call to cacheSolve().
  ## Print the time required to compute the inverse
  ## and cache the result.
  print(system.time(cacheSolve(my_special_matrix)))

  ident <- diag(n)
  ### changed the names for my cacheSolve().
  shouldbe_ident <- my_special_matrix$getMat() %*% my_special_matrix$getInv()

  if(isTRUE(all.equal(ident, shouldbe_ident)) == TRUE) {
    print("Answer Correct !!  :~)")

    ## Second call to cacheSolve().
    ## Print the time required to retrieve the already
    ## cached result.
    print(system.time(cacheSolve(my_special_matrix)))

    ## Return some funny message
    return ("Do you see the difference? :)")   
}
    return("Answer Appears Incorrect !! :(")
}