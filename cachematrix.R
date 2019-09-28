## These functions solve the inverse of a matrix and cache the result for 
## efficient access. 

## Creates an object containing four functions based on matrix input as an 
## argument. Function set() assigns a new matrix value; get() returns the 
## matrix; setsolve() assigns the inverse matrix value as calculated from the 
## parent environment using function cacheSolve(); getsolve() returns inverse
## matrix value. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) m <<- inverse
        getsolve <- function() m
        list(set = set, get = get, # allows calling of function using $ operator
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function retrieves the inverse matrix of matrix x if it has already been
## cached, or calculates the inverse and stores it for later access if it has 
## not already been cached. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
        

