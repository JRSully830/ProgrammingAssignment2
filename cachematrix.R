## The functions below are part of coding assignment 2 and take advantage of R
## lexical scoping characteristics
## Two functions are below.

## The 'makeCacheMatrix' creates a matrix and stores it's inverse in cache memory. This 
## allows for it to be retrieved by subsequent functions and used in calculations.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## As illustration, set input object = to a square matrix with inverse
testMatrix <- matrix(c(0,1,1,0),2,2)

## Set x equal to the object output of the 'makeCacheMatrix' function having used square
## matrix as input
x <- makeCacheMatrix(testMatrix)

## The 'cacheSolve' function checks to see if the matrix inverse was stored in cache 
## memory and uses the cached inverse if available. Otherwise it calculates and returns
## the inverse.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
    ## return matrix inverse
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
## return matrix inverse
    return(m)
}

## Execute cacheSolve function twice -- first time to calculate the inverse, 2nd time
## to pull it from cache memory instead
cacheSolve(x)
