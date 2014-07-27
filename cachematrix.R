## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Using "inv" will store the cached inverse matrix
    inv <- NULL

    # To set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # To get the the matrix
    get <- function() x

    # To set the inverse
    setinv <- function(inverse) inv <<- inverse
    # To get the inverse
    getinv <- function() inv

    # For returning the matrix with the new defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

        
}
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()

    # Return the inverse if it's already been calculated
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is calculated if it has not been done
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}

## That's all folks!
