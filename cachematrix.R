## The following two functions are used to cache the inverse of a matrix.
## Below are two functions that are used to create a special object that stores a numeric vector 
## and caches the inverse of a matrix.



## The first function, makeCacheMatrix creates a special "vector", 
## which is a list containing a function to

##  - set the value of the vector
##  - get the value of the vector
##  - set the value of the inverse of a matrix
##  - get the value of the inverse of a matrix

## This works when you use Lexico Scoping - 
## the statement returning the environment is defined inside this function, 
## but m is defined in the environment immediately outside i.e. the parent environment. 

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL

                set <- function(y) {
                    x <<- y
                    m <<- NULL
                }
                
                get <- function() x
                setinverseM <- function(inverseM) m <<- inverseM
                getinverseM <- function() m
                list(set = set, get = get,
                     setinverseM = setinverseM,
                     getinverseM = getinverseM)
}




## The following function calculates the mean of the special "vector" 
      ## created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 

## Otherwise, it calculates the mean of the data and sets the value of the 
      ## mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
              m <- x$getinverseM()
              if(!is.null(m)) {
                  message ("getting cached data")
                  return(m)
              }
              
              data <- x$get()
              m <- solve(data)
              x$setinverseM(m)
              m       
}


## You could try to assign this function to a variable vec set below:

## x = matrix(c(1,1/4,2,2/3), # the data elements 
##           nrow=2,              # number of rows 
##           ncol=2,              # number of columns 
##           byrow = TRUE)  

##vec <- makeCacheMatrix(x)
##xInverse <- cacheSolve(vec)

##xInverse
