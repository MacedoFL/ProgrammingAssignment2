## Functions to set and use cached resuts for the solve function.
## Rationale: Because the solve function may take a long time to return a result,
## it is a good idea to cache its result.
## Author: Antonio Macedo
## Date: 4/21/2015

## makeCacheMatrix
## Creates a list of functions to get and set a matrix and the result of solve.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize internal solve result to NULL
    sv <- NULL

    # Define set function
    set <- function(y) {
        # Set parent x matrix
        x <<- y
        # Set parent solve result to NULL
        sv <<- NULL
    }

    # Define get function, just return the matrix
    get <- function() x

    # Define setSolve function, set parent solve result
    setSolve <- function(solve) sv <<- solve

    # Define getSolve function, just return solve result
    getSolve <- function() sv

    # Return list of internal functions
    list( set = set
        , get = get
        , setSolve = setSolve
        , getSolve = getSolve)
}


## cacheSolve
## Use functions defined in makeCacheMatrix to call solve and cache its result
cacheSolve <- function(x, ...) {

    # Get cached solve result
    m <- x$getSolve()

    # Check if cached result is there
    if(!is.null(m)) {

        # Display a cache used message
        message("getting cached data")

        # Return cached result
        return(m)
    }

    # Cached result is NOT there
    # Get the data matrix
    data <- x$get()

    # Solve the data matrix
    sv <- solve(data, ...)

    # Cache the solve result
    x$setSolve(sv)

    # Return the solve result
    sv
}
