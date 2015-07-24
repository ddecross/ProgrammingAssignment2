## These functions were written to meet the requirements 
## for the programing assignment 2 for the R Programing class.  
## The overall purpose of these two functions is to create the 
## inverse of a provided square matrix.  In addition to having 
## the capability to create the inverse of a matrix the functions 
## will also check to see if the matrix has previously been 
## calculated and cashed. If it has been cashed then instead of 
## recalculating the inverse it will return the cashed invers 
## matrix otherwise it will calculate the inverse, cash it and 
## return the result

## This function will cash the inverse of a matrix.  When setting 
## up it stores a NULL into a variable, telling the cacheSolve 
## function that there is not a cashed value to be pulled.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(amatrix) m <<- amatrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function will check to see if there is an inverted matrix 
## that has been cashed.  If one exists the function will return 
## the cashed matrix.  If one does not exist then this function 
## will invert the matrix and call the setmatrix function to cash 
## the matrix for future use.  In either case this function will 
## return the inverted square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("Getting inverted matrix")
        return(m)
    }
    message("Calculating inverted matrix")
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
