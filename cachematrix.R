## As the README file states, some computations may be very demanding in terms of 
## time. Therefore, it may be a good idea to store some results (i.e., to cache them),
## in order to access them without repeating the computation everytime. 
## In this source file, we will take a matrix and the computation of its inverse 
## as a working example.
## In particular, we will define two functions: makeCacheMatrix and cacheSolve.
## Watch right before their definition for a short description of their functionalities.


## This function initialises a cacheable matrix, together with the quantities
## that we want to compute out of it, just the inverse in this case. 
## In particular, it returns a list of set/get couples. 
## The setters construct data, such as the matrix itself or its inverse,
## while the getters allow their retrieval.
## Finally, this method can be easily extended. For instance, if we also wanted 
## to cache&compute another quantity like the determinant, we should only need to add 
## two additional functions to the returned list.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL   # Initialise the inverse to null value
    set <- function(y){
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x    # If x already exists, you can print it out
    setInv <- function(solve) inv_x <<- solve
    getInv <- function() inv_x
    
    # Return the list of methods
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)    
}


## This function reads a square matrix initialised with the makeCacheMatrix function
## and returns its inverse. However, it first checks if said inverse has already
## been cached. If so, it returns it right away, otherwise the inverse matrix is
## computed and stored in memory.

cacheSolve <- function(x, ...) {
    #Return a matrix that is the inverse of 'x'
    inv_x <- x$getInv()
    
    #If the inverse is already computed, just return it from the cache
    if(!is.null(inv_x)) {
        #However, the user should better know whether the value was already cached or not
        message("getting cached data")
        return(inv_x)
    }
    #If the value is not cached already, compute & store it
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setInv(inv_x)
    
    #Return the computed value
    inv_x
}
