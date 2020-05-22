## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        ##initialize inverse variable
        inv <- NULL  
        ##setting up the matrix for invesion
        set <- function(y)
        {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        
        ##setting inverse matrix
       setInverse <- function(solveMatrix) inv <<- solveMatrix
        
       ##getting the inverse of matrix 
       getInverse <- function() inv
        
        ##returning the list of functions
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ##return the inverse if its already set
        if(!is.null(inv))
        {
               message("getting cached data")
               return(inv)
        }
        
        ##if not set get the object and calculate the inverse of matix
        data <- x$get()
        
        inv <- solve(data)  ## calculating inverse
        
        x$setInverse(inv)   ##setting up inverse value
        
        ##return inverse of matrix
        inv     
}
