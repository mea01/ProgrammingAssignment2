# The combination of functions below will take a matrix as input, solve or
# retrieve the inverse of the input matrix.

# The makeCacheMatrix function takes a matrix as an input, and defines 4
# functions that act as getters and setters. 1 pair of getter and setter 
# functions to get or set the matrix, another pair of getter and setter
# functions to get or set the inverse of the matrix. This function is only
# complete with the cacheSolve() function.

# Define makeCacheMatrix as function w/ arg "x" as empty matrix object.
# w/o default x assignment, function throws error when called x$get().
makeCacheMatrix <- function(x = matrix()) {
        # set object i to NULL value
        i <- NULL
        
        # define "set" function which takes "y" arg.
        # assumes y is a matrix object.
        # whenever x is reset by set(), i is reset as well. this causes
        # cacheSolve to re-calculate inverse matrix instead of retrieving
        # wrong inverse matrix.
        set <- function(y) {
                # set empty matrix x to y arg in parent environment
                x <<- y
                # set i object to null in parent environment
                # clears value of i that may have been cached by
                # previous execution of cacheSolve
                i <<- NULL
        }
        
        # define "get" function, no input, returns x from parent environment.
        # parent is used since x is not defined in get function definition.
        get <- function() x
        
        # defines "setinv" function and takes inverse matrix as argument.
        # i is defined in parent environment, we use <<- to assign
        # input argument to i in parent environment for access after setinv().
        setinv <- function(inv) i <<- inv
        
        # define "getinv" function, no input, returns i from parent environment.
        # parent is used since i is not defined in getinv function definition.
        getinv <- function() i
        
        # creates list which contains function to set val of matrix, get val 
        # of matrix, set val of inv, get val of inv
        list(set = set,
             # gives the name 'set' to the set() function defined above
             get = get,
             # gives the name 'get' to the get() function defined above
             setinv = setinv,
             # gives the name 'setmean' to the setmean() function defined above
             getinv = getinv)
             # gives the name 'getmean' to the getmean() function defined above
}


# The cacheSolve function is designed to take a list object from 
# makeCacheMatrix() function and retrieve the inverse of the original
# input matrix, or, if null, then solve for the inverse of the input matrix.

# Define cacheSolve as function w/ arg "x" as undefined, and ellipsis for
# additional arguments. 
cacheSolve <- function(x, ...) {
        
        # Attempt to retrieve inverse matrix from x, assigns it to i.
        i <- x$getinv()
        
        # Check to see if result of getinv() is NULL. makeCacheMatrix() sets
        # cached inverse to NULL whenever new matrix is set into the object,
        # if the value here is not NULL, we have a valid, cached inverse matrix
        # and can return it to parent environment
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If !is.null(i) is FALSE, then cacheSolve executes following:
        # Retrieves matrix from input object x.
        data <- x$get()
        
        # Calculates inverse matrix of original matrix in x and assigns it to i.
        i <- solve(data, ...)
        
        # Uses setinv() on input object x to set i inverse matrix to input
        # object x. 
        x$setinv(i)
        
        # Returns value of inverse matrix to parent environment by printing
        # inverse matrix object.
        i
}


# Example run below
# Must uncomment first

# set.seed(5)
# aMatrix <- makeCacheMatrix(matrix(rnorm(100, mean=15, sd=2), nrow=10, ncol=10))
# aMatrix$get()           # retrieve the value of x
# aMatrix$getinv()        # retrieve the value of i, which should be NULL
# aMatrix$set(matrix(rnorm(100, mean=150, sd=25), nrow=10, ncol=10))
#                         # reset value with a new vector
# 
# cacheSolve(aMatrix)      # notice inverse calculated is inverse of 150, not 15
# aMatrix$getinv()        # retrieve it directly, now that it has been cached
