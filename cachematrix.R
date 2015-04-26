## Put comments here that give an overall description of what your
## functions do

## Caches a data matrix

makeCacheMatrix <- function(x = matrix()) {
        # sets m to be NULL
        m <- NULL    
        
        # Substitutes x for y in main function
        set <- function(y){    
                x<<-y
                m<<-NULL
        }
        
        # Returns vector x that is stored in main function, makeCacheMatrix()
        get <- function() x    
        
        # caches the inverse of x in main function
        setmatrix <- function(solve) m <<- solve    
        
        # Returns the value of m from main function
        getmatrix <- function() m    
        
        # stores the 4 function into the main function
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Returns an invers of cached data matrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()    # sets m to getmatrix object of makeCacheMatrix function
        
        if(!is.null(m)){
                return(m)
        }
        matrix <- x$get()    # sets matrix to get object of makeCacheMatrix function
        m <-solve (matrix, ...)    #  sets m to inverse of matrix
        x$setmatrix(m)    # Runs setmatrix function from makeCachMatrix function with argument of m
        m    # returns m
}
