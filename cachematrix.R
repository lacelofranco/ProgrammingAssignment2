# R Programming assignment 2


# This function allows caching of the matrix inverse as a function variable 
# returning a list (acting like a pseudo object) with getters and setters
#
# INPUT PARAMETERS: 
# matrix (optional - if not provided an empty matrix is created)
#
makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setMatrixInverse <- function(inverse) {
    matrixInverse <<- inverse
  } 
  
  getMatrixInverse <- function() {
    matrixInverse
  }
  
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


# This function returns a matrix that is the inverse of 'x', taking in output of makeCacheMatrix
# It initially checks if a cached inverse matrix exists prior to computing the inverse
#
# INPUT PARAMETERS: 
# matrix (optional - if not provided an empty matrix is created)
#
cacheSolve <- function(x, ...) {
  
  matrixInverse <- x$getMatrixInverse()
  
  if(!is.null(matrixInverse)) {
    message("getting cached matrix inverse")
    return(matrixInverse)
  }
  
  data <- x$get()
  matrixInverse <- solve(data)
  x$setMatrixInverse(matrixInverse)
  
  matrixInverse
        
}

########################
# Sample use

# Create the sample cached matrix
sampleCachedMatrix = makeCacheMatrix(matrix(c(4,3,3,2), nrow = 2, ncol = 2, byrow = TRUE))

# Inital call, so matrix is not yet cached
cacheSolve(sampleCachedMatrix) 

# Second call, matrix cached is retrieved. Notice "getting cached matrix inverse" is printed out.
cacheSolve(sampleCachedMatrix) 



