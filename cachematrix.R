## The function below will take a matrix and store it in the first part 
## and then find the inverse of the matrix and store it.  The second part will 
## check to see if there is a stored inverse matrix.  If there is one it will 
## return the matrix and if there is not it will find the inverse and then 
## return the matrix

## This function will take a matrix and find the inverse and store the orginial 
## matrix and also the inverse.  The input needs to be a square matrix. 


makeCacheMatrix <- function(x = matrix()) { 
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setMatrix <- function(solve) m <<- solve    # calculates inverse 
                getMatrix <- function() m
                list(set = set, get = get,
                     setMatrix = setMatrix,
                     getMatrix = getMatrix)       # stores the outputs for other
                                                        #function to use 
} 



## This function checks to see if the inverse has been calculated and if so will
## return the value bit it has not it will then calculate the value.  The input
## for this needs to be the stored results from the function above. 


cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getMatrix()           #gets the value of m from the function above
        if(!is.null(m)) {       # value if the inverse has not be found 
                message("getting cached data")
                return(m)      
        }
        data <- x$get()    # get the data from function above 
        m <- solve(data, ...)   #calculated inverse 
        x$setMatrix(m)     
        m          # prints the inverse matrix 
        
} 

