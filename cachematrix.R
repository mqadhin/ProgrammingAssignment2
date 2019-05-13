## Computes the inverse of a matrixx
## Caches the inverse once calculated or uses cached data to retrieve already computed inverse matrix

## Creates a speacial list matrix that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
    # Inititalizing x to an empty matrix prevents code from throwing an error
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## If inverse has already been calculated, cacheSolve should retrieve inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
