# A pair of functions that cache an inverted matrix
# Matrix inversion is a computationally intensive process. 
# So long as the contents of the matrix are not changing, 
# it may be beneficial to look up the inverted matrix in cache rather than recomputing.

#makeCacheMatrix - create a special object that stores a cached inverted matrix as well as four functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #resets cached inverted matrix to NULL if makeCacheMatrix(x) is called
    set <- function(y) { #set function enables resetting of makeCacheMatrix(x) using z$set(y) if z <- makeCacheMatrix(x)
        x <<- y #passes z$set(y) to makeCacheMatrix(x)
        m <<- NULL #resets the cached inverted matrix if z$set() is called
    }
    get <- function() x #used by cacheSolve to obtain matrix x from makeCacheMatrix
    setMatrix <- function(invmatrix) m <<- invmatrix #caches the inverted matrix calculated in cacheSolve as m in makeCacheMatrix
    getMatrix <- function() m #getMatrix function allows cacheSolve to obtain m from makeCacheMatrix
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix) #sets makeCacheMatrix as a list of functions
}

#cacheSolve - if no cached matrix exists, calculates the inverted matrix and stores it in cache

cacheSolve <- function(x, ...) { #the ... allows arguments to pass from cacheSolve to makeCacheMatrix
    m <- x$getMatrix() #calls makeCacheMatrix$getMatrix which obtains m from the makeCacheMatrix function
    if(!is.null(m)) { #if cached matrix exists then return the cached matrix
        message("getting cached data")
        return(m) #returns the cached matrix from m in makeCacheMatrix
    }
    data <- x$get() #calls makeCacheMatrix$get to obtain the input matrix x from makeCacheMatrix
    m <- solve(data, ...) # returns a matrix that is the inverse of 'x'
    x$setMatrix(m) #calls makeCacheMatrix$setMatrix to cache the inverted matrix
    m #prints m
}
