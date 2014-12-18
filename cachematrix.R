## makeCacheMatrix and #cacheSolve
## a set of matrix helper functions to set & get matrices and their inverses


## makeCacheMatrix(X) accepts a matrix X and returns an object which allows
## cached storage of an inverse of the matrix.  Setters and getters are 
## provided for both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  #constructor
  i <- NULL    #i will hold the matrix inverse
  
  #setter for matrix
  set <- function(mat) {
    x <<- mat  #store the matrix
    i <<- NULL #clear the inverse of the newly stored matrix...
    #note we compute this only when we need to
  }
  
  #getter
  get <- function() x  #return the value of the matrix
  
  #setter for inverse cache
  setinverse <- function(inverse) i <<- inverse
  #set the value of the cache to be the argument passed in
  
  
  #getter for inverse cache
  getinverse <- function() i   #return the value of the cache...
  #note that the value of the cache could be the matrix inverse...
  #or it could be null
  
  #return the list with the functions for accessing x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  #give the functions names in the returned list
}


## cacheSolve(X) accepts an object created with makeCacheMatrix 
## returns the inverse of the matrix
## will use the cached inverse if it has been previously computed
## otherwise will compute the inverse and store it for future use
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()  #attempt to check the cache of x for the inverse
  
  #if there is a cached inverse, return it
  if(!is.null(i)) {  #if there is a cached inverse...
    message("getting cached data")  #tell that there is an inverse...
    return(i)  #return the inverse
  }
  
  #since inverse was not cached, compute the inverse, cache it, return it
  #Per assignment, assume the matrix is invertible
  mat <- x$get()  #fetch the matrix from the object x...
  i <- solve(mat, ...)  #...take the inverse of the matrix...
  x$setinverse(i)  #...store the inverse in the cache of the object x...
  i   #... and finally return the inverse to the calling program
  

}
