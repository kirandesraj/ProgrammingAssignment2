#23 June 2016
#Kiran Desraj
#Programming Assignment 2 
#Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        #Create null variable
        m <- NULL
        
        #Calibrate matrix
         set <- function(matrix) {
                x <<- matrix
                null_variable <<- NULL 
        }
        
        get <- function() {
                x 
        }

        #Calibrate inverse
        setinverse <- function(inverse) {
                m <<- inverse
        }
        
         getinverse <- function() {
                m 
        }
        
        #List containing functions
        list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
        
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        #Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        #Calculate inverse
        m <- solve(data) %*% data
        
        #Set inverse and return matrix
        x$setinverse(m)
        m
        
}
