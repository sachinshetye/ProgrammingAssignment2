### Evaluate matrix inversion
### cache result and return cached result if exists


## Generate a list of matrix manipulating functions
##
## Input: matrix
## Output: list containing functions below:
##        set: unused but needed
##        get: return the matrix
##        setInverse: cache value to a variable
##        getInverse: get value from cache [empty if not cached]
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setter
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # getter
    get <- function() x
    
    # set inverse
    setInverse <- function(value) m <<- value
    # get inverse
    getInverse <- function() m
    
    # return list of functions created above
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Get the matrix inverse
##
## Input: function list created by makeCacheMatrix
## Output: matrix inverse
cacheSolve <- function(x, ...) {
    result <- x$getInverse()
    
    # If inverse already exists, return it
    if (!is.null(result)) {
      message("Getting cached data")
      return(result)
    }
    
    data <- x$get()
    #evaluate inverse
    result <- solve(data, ...)
    #set inverse in environment
    x$setInverse(result)
    
    #return inverse
    result
}