# To avoid the repeated calculation of the inverse of a matrix and thus safe computational effort,
# the result is cached instead.

# The function makeCacheMatrix creates a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
         }
        get<-function() x
        setinverse<-function(solve) i<<-solve
        getinverse<-function() i
        list(set=set, get=get,
                setinverse=setinverse,
                getinverse=getinverse)
}

# The function cacheSolve checks if the inverse of the matrix is already been calculated.
# If that's the case, it gets the result from the charge,
# if not it calculates the inverse using the solve function.
# The inverse matrix is returned.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return (i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
