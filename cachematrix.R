## Coursera - R programming - 2nd programming assignment

# Generate a list of functions to: set/get the matrix; set/get its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        # set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # get the matrix
        get <- function() x

        # override the inverse with the given value
        setinverse <- function(inverse) m <<- inverse

        # get the inverse value
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Produce the inverse of the given matrix by either retrieving the cached value or calculating it
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'

        m <- x$getinverse()

        # return cached information to prevent calculating again
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # get the matrix
        data <- x$get()

        # calculate the inverse of the matrix. It assumes it is always possible
        m <- solve(data, ...)

        # set the inverse to the calculated value
        x$setinverse(m)

        # return the inverse
        m
}

