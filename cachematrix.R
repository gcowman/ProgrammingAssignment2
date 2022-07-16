# makeCacheMatrix function: Create a function that makes a special matrix
# object that can cache its inverse
# The formal argument in the function is a matrix x.  
# Initialize object x as an empty matrix
makeCacheMatrix <- function(x = matrix()) {
    # Initialize object inv as NULL. inv will be used to 
    # create an inverse matrix of x.
    inv <- NULL
    # Define the set function with argument y, using the <<- operator to
    # assign x in the parent environment a value y and 
    # assign inv in the parent environment a value of NULL, this clears any
    # any previous value of inv that may have been cached.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Define the getter for x
    get <- function() x
    # Define the setter for the inverse matrix inv using the solve function 
    # and the <<- operator
    setinverse <- function(solve) inv <<- solve
    # Define the getter for the inverse matrix inv
    getinverse <- function() inv
    # Create a list of the four functions with assigned names
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve function: computes the inverse of the special matrix returned by
# the makeCacheMatrix function, if the inverse was already calculated, it will
# be retrieved from the cache
cacheSolve <- function(x, ...) {
    # Call the getinverse function on the input x
    inv <- x$getinverse()
    # Check if inv is NULL.  If not, return the cached inverse matrix and
    # return it to the parent environment.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If inv is NULL, get the input matrix x 
    data <- x$get()
    # Calculate the inverse matrix
    inv <- solve(data, ...)
    # Set the inverse of the input matrix
    x$setinverse(inv)
    # Return the inverse matrix to the parent environment
    inv
}