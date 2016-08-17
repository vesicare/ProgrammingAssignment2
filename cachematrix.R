## Matrix inversion
## input matrix to makeCacheMatrix function then forward to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  # m or mean: inverse matrix
    }
    get <- function() x
    setmean <- function(mean) m <<- mean # inverse
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  #matrix operator
    x$setmean(m)
    m
}

## check this work
#A <- matrix(c(2,2,3,2), 2,2)
#A1 <- makeCacheMatrix(A)
#cacheSolve(A1)
#A<-matrix(c(7,-1,-1,-3,1,0,-3,0,1),3,3)
#A2 <- makeCacheMatrix(A)
#cacheSolve(A2)
#cacheSolve(A1)
#cacheSolve(A2)


