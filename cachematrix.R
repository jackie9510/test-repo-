## The first function, makeCasheMatrix creates a special "matrix", 
## which is really a list containing a function to

## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 3. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cashed data")
                return(m)
        }
        data <- x$get(x) 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}







