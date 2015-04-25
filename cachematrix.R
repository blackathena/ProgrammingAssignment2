
## The pair of functions are designed to cache the inverse of an invertible matrix.
## The first function creates a matrix that can cache its inverse.


makeCacheMatrix <- function(x= matrix()){
	inverse <- NULL             ## Store the cached value and Initialize it to NULL
	set <- function(y) {	## Set the value of the local matrix (x)i.e. create the matrix in the working environment 
	x <<- y
	inverse <- NULL
     }
	get <- function() x         ## Get the value of the local matrix (x)
	setinverse <- function(inverseMatrix) inverse <<- inverseMatrix  ## Invert the matrix and store it in cache
	getinverse <- function() inverse    ## Get the inverted matrix from cache
	
	list(set= set, get= get,    ## Return the created functions to the working environment
	     setinverse= setinverse, getinverse= getinverse)
}
	

## The second function below returns the inverse of the matrix from the cache (created by makeCacheMatrix) 
## if previously calculated or create it here

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()   ## Get the inverse of the matrix stored in the cache
	if(!is.null(inverse)) {      ## Retrun the value of the inverted matrix if it exists
		message("Retrieving cached data") ## Else create the matrix 
		return(inverse)
   }
	data <- x$get()     ## Create the matrix if it doesn't exist
	inverse <- solve(data, ...)  ## Set and return the inverse of the matrix
	x$setinverse(inverse)         ## Set inverted matrix value in the Cache
	inverse          ## Display matrix in the R console
 }


