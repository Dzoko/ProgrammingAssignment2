## The set of functions defined below will allow to cache the inverse of a matrix. Calculation 
## of an inverted matrix can be a costly process. the process of caching allows a more economic
## use of resources.


## The function 'makeCacheMatrix' creates a 'special matrix' that can store its inverse matrix. 
## This is achieved by
##      1. accepting a matrix 'x' as parameter
##      2. creating a local variable 'i' to store the inverse of 'x'
##      3. creating 4 functions that allow getting/setting of 'x' and 'i'
##      4. returning a list of the 4 newly defined functions for future use

makeCacheMatrix <- function(x = matrix()) {
        
        ## Test, if submitted parameter is a matrix.
        
        if(class(x) != "matrix") {
                message("Please submit a matrix")
                return(x)
        }
        
        ## Create local variable 'i'.
        
        i <- NULL
        
        ## Define function 'set'. This function does 3 things:               
        
        
        set <- function(y) {
                
                ##      1. It ensures that the parameter which has been passed 
                ##      is a matrix.
                
                if(class(y) != "matrix") {
                        message("Please submit a matrix")
                        return(y)
                }
                
                ##      2. It assigns its parameter 'y' to the parent function's 
                ##      parameter 'x', i.e. it assigns a new matrix in the 
                ##      parent environment.
                
                x <<- y
                
                ##      3. It ensures that an inverse that potentially has been 
                ##      calculated for another matrix is deleted.
                ##      The function 'set' sets the parent function's local 
                ##      variable 'i' to NULL so that the need for recalculation can be detected.
                
                i <<- NULL
        }
        
        ## Define function 'get'. This function returns 
        ## the parent environment parameter 'x', i.e. the input matrix.
        
        get <- function() x
        
        ## Define function 'setinverse'. This function assigns a value to
        ## the parent environment variable 'i', i.e. the inverse matrix.
        
        setinverse <- function(inverse) i <<- inverse
        
        ## Define function 'getinverse'. This function returns the value of
        ## the parent environment variable 'i', i.e. the inverse matrix.
        
        getinverse <- function() i
        
        ## Finally a list of all 4 functions is compiled and returned.
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
        
}


## The function 'cacheSolve' calculates the inverse of a matrix that is passed indirectly, i.e. a
## matrix that is enclosed in its parameter - an object returned by the function 'makeCacheMatrix'. 
## The calulation is only triggered, if the respective inverse matrix has not been calculated yet.

cacheSolve <- function(x, ...) {
        
        ## Retrieve the value of the cached inverse matrix 'i'.
        
        i <- x$getinverse()
        
        ## If 'i' does not equal NULL, a message and the cached value are returned. 
        ## The execution of the function 'cacheSolve' is complete.
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If the value of 'i' is equal to NULL, the input matrix is retrieved and stored in 
        ## the local variable 'data'.
        
        data <- x$get()
        
        ## Then the inverse matrix is calculated by using the function 'solve'.
        
        i <- solve(data, ...)
        
        ## The result is assigned to the cached variable 'i'.
        
        x$setinverse(i)
        
        ## Finally the newly calculated value of 'i' is returned.
        
        i
        
        
}