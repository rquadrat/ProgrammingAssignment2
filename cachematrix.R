## Both functions together allow caching of a matrix inverse. As long as the 
## cached matrix is not changed via the set function the inverse is not 
## recalculated and taken from memory.

## This function creates a matrix able to cache it's inverse from a standard 
## matrix. The inverse is stored inside the inv variable. Getter and setter 
## methods for the matrix itself and it's inverse are defined and
## stored inside a list. Therefore the four methods can be called using e.g. 
## x$getinv() or x$set(matrix), where x is a CacheMatrix and matrix is a 
## matrix. The setinv and getinv methods should not be used directly. Instead 
## the cacheSolve function should be used (See next function).

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y)
    {
        x<<-y
        inv<<-NULL
    }
    get<-function()
    {
        x
    }
    setinv<-function(inverse)
    {
        inv<<-inverse
    }
    getinv<-function()
    {
        inv
    }
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}


## If this function is called on a CacheMatrix object, it returns the inverse 
## of the original matrix. If the inverse had been calculated previously and 
## the matrix wasn't modified in meantime it is taken from memory (the inv 
## variable from makeCacheMatrix), otherwise the inverse is calculated via the 
## standard solve function and the inv variable in makeCacheMatrix is set 
## according to this value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
