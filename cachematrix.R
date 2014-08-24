## makeCacheMatrix function will create a list of 4 elements which
## sets,gets,sets inverse and gets the inverse respectively of a matrix

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
  set <- function(y = matrix()){
           x <<- y
           minv <<- NULL
         }
  get <- function() { x }
  setinv <- function(minv) minv <<- minv
  getinv <- function() minv
  
list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## cacheSolve function will return inverse of a new matrix. It uses Cached data 
## to retrieve the inverse of an existing matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           minv <- x$getinv()
            if(!is.null(minv)) {
                    message("getting cached data")
                    return(minv)
            }
            data <- x$get()    #Retrieves the matrix and assigns it to data.
            minv <- solve(data) # Calculates inverse and assigns to minv
            x$setinv(minv)     # Sets the inverse to "setinv" variable of list.
            minv               # Returns the matrix inverse.

}

