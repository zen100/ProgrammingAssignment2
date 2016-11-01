## makeCacheMatrix caches the value of the matrix
## cacheSolve returns the cached value or calculates the inverse if 
##   nothing is cached

## This function accepts a matrix x as input, 
## the anonymous functions are necessary to get the variables in the environment
## where the function is called
##  sets the matrix
##  gets the matrix  
##  sets the inverse
##  gets the inverse

## Notes:  `<<-` assigns a value to an object in an environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## The function checks if the inverse already exists
## If the inverse is not null it gets the cached data
## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x: output of makeCacheMatrix()
    
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    
    return(inv)    
}
